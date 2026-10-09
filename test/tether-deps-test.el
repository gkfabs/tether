;;; tether-deps-test.el --- Contract tests against magit, forge, ghub  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; These tests load the real magit, forge and ghub (no network) and
;; check that they still provide what tether uses.  CI
;; runs them against the "min" and "latest" dependency sets, and weekly
;; against the latest MELPA snapshots.

;;; Code:

(require 'ert)
(require 'url-parse)
(require 'magit)
(require 'forge)
(require 'ghub)
(require 'tether-deps)

(ert-deftest tether-deps-test-everything-present ()
  "Every function, variable, class, slot, argument and method is there."
  (should (equal (tether--dependency-problems) nil)))

(ert-deftest tether-deps-test-dispatch-key ()
  "Tether is on @ in the dispatch menus: the key was free."
  (require 'tether)
  (dolist (prefix '(magit-dispatch forge-dispatch))
    (let ((suffix (ignore-errors (transient-get-suffix prefix "@"))))
      (should suffix)
      (should (memq 'tether (flatten-tree (if (vectorp suffix)
                                               (append suffix nil)
                                             suffix)))))))

;;;; Requests: the URL and token ghub really uses

(defvar tether-deps-test--auth-searches nil
  "Arguments of each `auth-source-search' call during a test.")

(defun tether-deps-test--capture (thunk)
  "Call THUNK with the network stubbed; return (URL . HEADERS).
`url-retrieve' and `auth-source-search' are Emacs builtins, so
everything between them and THUNK is ghub's real code."
  (let (url headers)
    (setq tether-deps-test--auth-searches nil)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest spec)
                 (push spec tether-deps-test--auth-searches)
                 (list (list :secret "TOKEN"))))
              ((symbol-function 'url-retrieve)
               (lambda (u &rest _)
                 (setq url (if (stringp u) u (url-recreate-url u)))
                 (setq headers url-request-extra-headers)
                 nil)))
      (funcall thunk))
    (cons url headers)))

(defun tether-deps-test--rest (host forge resource)
  "Capture the request tether makes for RESOURCE on HOST of FORGE.
These are the arguments `tether--request' passes."
  (tether-deps-test--capture
   (lambda ()
     (ghub-request "GET" resource nil
                   :host host :auth 'forge :forge forge
                   :username "tester"
                   :callback #'ignore :errorback #'ignore))))

(defun tether-deps-test--graphql (host forge)
  "Capture the GraphQL request tether makes on HOST of FORGE."
  (tether-deps-test--capture
   (lambda ()
     (ghub-query '(query (viewer login)) nil
       :host host :auth 'forge :forge forge
       :username "tester"
       :callback #'ignore :errorback #'ignore))))

(defun tether-deps-test--searched-user-p (user host)
  "Return non-nil if a token for USER was looked up on HOST."
  (cl-some (lambda (spec)
             (and (equal (plist-get spec :user) user)
                  (equal (plist-get spec :host) host)))
           tether-deps-test--auth-searches))

(ert-deftest tether-deps-test-gitlab-com-url ()
  "Problem 2 of the design notes: the API host must be kept."
  (pcase-let ((`(,url . ,headers)
               (tether-deps-test--rest "gitlab.com/api/v4" 'gitlab
                                       "/projects/a%2Fb/merge_requests/1")))
    (should (equal url "https://gitlab.com/api/v4/projects/a%2Fb/merge_requests/1"))
    (should (equal (cdr (assoc "Private-Token" headers)) "TOKEN"))
    ;; Problem 1: forge's token is reused, no `code-review' token.
    (should (tether-deps-test--searched-user-p "tester^forge"
                                               "gitlab.com/api/v4"))))

(ert-deftest tether-deps-test-gitlab-self-hosted-url ()
  (should (equal (car (tether-deps-test--rest
                       "git.example.org/api/v4" 'gitlab
                       "/projects/g%2Fsub%2Fp/merge_requests/7/discussions"))
                 (concat "https://git.example.org/api/v4"
                         "/projects/g%2Fsub%2Fp/merge_requests/7/discussions"))))

(ert-deftest tether-deps-test-github-com-url ()
  (pcase-let ((`(,url . ,headers)
               (tether-deps-test--rest "api.github.com" 'github
                                       "/repos/o/r/pulls/1")))
    (should (equal url "https://api.github.com/repos/o/r/pulls/1"))
    (should (equal (cdr (assoc "Authorization" headers)) "token TOKEN"))
    (should (tether-deps-test--searched-user-p "tester^forge"
                                               "api.github.com"))))

(ert-deftest tether-deps-test-github-enterprise-url ()
  (should (equal (car (tether-deps-test--rest "ghe.example.com/api/v3" 'github
                                              "/repos/o/r/pulls/1"))
                 "https://ghe.example.com/api/v3/repos/o/r/pulls/1")))

(ert-deftest tether-deps-test-graphql-urls ()
  "GraphQL endpoints derived from the REST host."
  (should (equal (car (tether-deps-test--graphql "api.github.com" 'github))
                 "https://api.github.com/graphql"))
  (should (equal (car (tether-deps-test--graphql "ghe.example.com/api/v3"
                                                 'github))
                 "https://ghe.example.com/api/graphql"))
  (should (equal (car (tether-deps-test--graphql "gitlab.com/api/v4" 'gitlab))
                 "https://gitlab.com/api/graphql")))

(provide 'tether-deps-test)
;;; tether-deps-test.el ends here
