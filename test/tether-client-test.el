;;; tether-client-test.el --- Tests for tether-client  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Requests, waiting and error messages, with `ghub-request' stubbed.

;;; Code:

(require 'ert)
(require 'tether-client)
(require 'tether-test-helpers)

(defmacro tether-client-test-failing (err headers &rest body)
  "Run BODY with `ghub-request' failing with ERR and response HEADERS."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'ghub-request)
              (lambda (_method _resource _params &rest args)
                (funcall (plist-get args :errorback) ,err ,headers nil nil))))
     ,@body))

(defun tether-client-test-error-message (thunk)
  "Return the message of the `user-error' signaled by THUNK."
  (condition-case err
      (progn (funcall thunk) (error "No user-error signaled"))
    (user-error (cadr err))))

(ert-deftest tether-client-test-request-arguments ()
  "Host, token and forge come from the repository (SPEC §5.1)."
  (tether-test-with-api '(("/projects/x" . ((id . 1))))
    (should (equal (tether--request (tether-test-gitlab-repo) "GET" "/projects/x")
                   '((id . 1))))
    (let ((request (car tether-test-requests)))
      (should (equal (plist-get request :host) "gitlab.com/api/v4"))
      (should (eq (plist-get request :auth) 'forge))
      (should (eq (plist-get request :forge) 'gitlab)))))

(ert-deftest tether-client-test-forge-type ()
  (should (eq (tether--forge-type (tether-test-github-repo)) 'github))
  (should (eq (tether--forge-type (tether-test-gitlab-repo)) 'gitlab))
  (should-error (tether--forge-type (forge-forgejo-repository :owner "o" :name "r"))
                :type 'user-error))

(ert-deftest tether-client-test-project-id ()
  "Sub-groups are part of the encoded project id."
  (should (equal (tether--project-id (tether-test-gitlab-repo)) "g%2Fsub%2Fp")))

(ert-deftest tether-client-test-token-rejected ()
  (tether-client-test-failing '(error http 401 ((message . "Bad credentials")))
      '(("Content-Type" . "application/json"))
    (let ((msg (tether-client-test-error-message
                (lambda () (tether--request (tether-test-github-repo)
                                            "GET" "/x")))))
      (should (string-match-p "api.github.com rejected the token" msg))
      (should (string-match-p "Bad credentials" msg))
      (should (string-match-p "(forge)Setup" msg)))))

(ert-deftest tether-client-test-rate-limit ()
  "A 403 from an exhausted rate limit is not reported as a token problem."
  (tether-client-test-failing '(error http 403 ((message . "API rate limit exceeded")))
      '(("x-ratelimit-remaining" . "0") ("x-ratelimit-reset" . "0"))
    (let ((msg (tether-client-test-error-message
                (lambda () (tether--request (tether-test-github-repo)
                                            "GET" "/x")))))
      (should (string-match-p "rate limit reached; retry after" msg))
      (should-not (string-match-p "token" msg)))))

(ert-deftest tether-client-test-html ()
  "HTML means the wrong host or a Cloudflare challenge (design problem 2)."
  (tether-client-test-failing '(error http 403 ((message . "Just a moment")))
      '(("Content-Type" . "text/html; charset=UTF-8"))
    (should (string-match-p
             "gitlab.com/api/v4 returned HTML, not JSON"
             (tether-client-test-error-message
              (lambda () (tether--request (tether-test-gitlab-repo)
                                          "GET" "/x")))))))

(ert-deftest tether-client-test-other-http-error ()
  (tether-client-test-failing
      '(error http 422 ((message . "Validation Failed")
                        (errors . ("line must be part of the diff"))))
      nil
    (let ((result (tether--call (tether-test-github-repo) "POST" "/x")))
      (should-not (plist-get result :ok))
      (should (eq (plist-get result :status) 422))
      (should (string-match-p "HTTP 422: Validation Failed; (line must"
                              (plist-get result :message))))))

(ert-deftest tether-client-test-connection-error ()
  (tether-client-test-failing '(error connection-failed "refused") nil
    (should (string-match-p
             "cannot reach api.github.com: connection-failed refused"
             (tether-client-test-error-message
              (lambda () (tether--request (tether-test-github-repo)
                                          "GET" "/x")))))))

(ert-deftest tether-client-test-timeout ()
  "No answer within `tether-request-timeout' is an error, not a hang."
  (cl-letf (((symbol-function 'ghub-request) #'ignore))
    (let ((tether-request-timeout 0.2))
      (should (string-match-p
               "did not answer within 0.2 seconds"
               (tether-client-test-error-message
                (lambda () (tether--request (tether-test-github-repo)
                                            "GET" "/x"))))))))

(provide 'tether-client-test)
;;; tether-client-test.el ends here
