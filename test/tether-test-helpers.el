;;; tether-test-helpers.el --- Fixtures for tether tests  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Real git repositories in a temporary directory, isolated from the
;; user's git configuration, and in-memory forge objects (no database).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'forge)

(defun tether-test-git (&rest args)
  "Run git with ARGS in `default-directory'; return trimmed output.
Signal an error when git fails."
  (with-temp-buffer
    (let ((status (apply #'call-process "git" nil t nil
                         "-c" "user.name=Tester" "-c" "user.email=t@example.com"
                         "-c" "init.defaultBranch=main"
                         "-c" "commit.gpgsign=false"
                         args)))
      (unless (eq status 0)
        (error "Git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun tether-test-commit (file content)
  "Write CONTENT to FILE, commit it, and return the new commit id."
  (with-temp-file (expand-file-name file) (insert content))
  (tether-test-git "add" file)
  (tether-test-git "commit" "-q" "-m" (format "Change %s" file))
  (tether-test-git "rev-parse" "HEAD"))

(defmacro tether-test-with-repos (&rest body)
  "Run BODY in a local clone of a server repository with PR 7.
The server has `main' (commits base, main2) and the PR head
\(two commits on top of base) published only as refs/pull/7/head
and refs/merge-requests/7/head, so the clone lacks the PR commits.
Bind `base', `main2', `pr1' and `head' to the commit ids, and
`server' and `clone' to the directories."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "tether-test-" t))
          (server (expand-file-name "server.git/" root))
          (work (expand-file-name "work/" root))
          (clone (expand-file-name "clone/" root))
          (process-environment
           (append (list (concat "HOME=" root)
                         "GIT_CONFIG_NOSYSTEM=1"
                         "GIT_CONFIG_GLOBAL=/dev/null")
                   process-environment))
          base main2 pr1 head)
     (unwind-protect
         (progn
           (make-directory work)
           (let ((default-directory work))
             (tether-test-git "init" "-q")
             (setq base (tether-test-commit "a.el" "one\ntwo\nthree\n"))
             (tether-test-git "checkout" "-q" "-b" "feature")
             (setq pr1 (tether-test-commit "a.el" "one\nTWO\nthree\n"))
             (setq head (tether-test-commit "b.el" "new\n"))
             (tether-test-git "checkout" "-q" "main")
             (setq main2 (tether-test-commit "c.el" "main\n")))
           (let ((default-directory root))
             (tether-test-git "init" "-q" "--bare" server))
           (let ((default-directory work))
             (tether-test-git "push" "-q" server "main"
                              (concat head ":refs/pull/7/head")
                              (concat head ":refs/merge-requests/7/head")))
           (let ((default-directory root))
             (tether-test-git "clone" "-q" "--no-local" server clone))
           (let ((default-directory clone))
             ,@body))
       (delete-directory root t))))

(defun tether-test-github-repo ()
  "Return an in-memory GitHub repository o/r with remote origin."
  (forge-github-repository :id "repo-id" :owner "o" :name "r"
                           :apihost "api.github.com" :githost "github.com"
                           :remote "origin"))

(defun tether-test-gitlab-repo ()
  "Return an in-memory GitLab repository g/sub/p with remote origin."
  (forge-gitlab-repository :id "repo-id" :owner "g/sub" :name "p"
                           :apihost "gitlab.com/api/v4" :githost "gitlab.com"
                           :remote "origin"))

(cl-defun tether-test-pullreq (&key (number 7) (head-ref "feature")
                                    (head-repo "o/r") head-rev)
  "Return an in-memory open pull request NUMBER of the test repository.
HEAD-REF, HEAD-REPO and HEAD-REV set the corresponding slots."
  (forge-pullreq :id (format "pr-%d" number) :repository "repo-id"
                 :number number :title "Fix parsing" :state 'open
                 :author "alice" :base-ref "main" :head-ref head-ref
                 :head-repo head-repo :head-rev head-rev :base-rev nil
                 :their-id nil :cross-repo-p nil :locked-p nil))

(defvar tether-test-requests nil
  "Requests made through the stubbed `ghub-request', most recent first.")

(defmacro tether-test-with-api (responses &rest body)
  "Run BODY with `ghub-request' answering from RESPONSES.
RESPONSES is an alist (RESOURCE . VALUE); VALUE is passed to the
callback.  Each request is recorded in `tether-test-requests' as a
plist with :method, :resource and the keyword arguments."
  (declare (indent 1) (debug t))
  `(let ((tether-test-requests nil)
         (responses ,responses))
     (cl-letf (((symbol-function 'ghub-request)
                (lambda (method resource _params &rest args)
                  (push (append (list :method method :resource resource) args)
                        tether-test-requests)
                  (let ((answer (assoc resource responses)))
                    (unless answer
                      (error "Unexpected request %s %s" method resource))
                    (funcall (plist-get args :callback) (cdr answer) nil nil
                             nil)))))
       ,@body)))

(defmacro tether-test-with-handler (handler &rest body)
  "Run BODY with the forge API answered by HANDLER.
`ghub-request' and `ghub-query' call HANDLER with a plist:
  (:method M :resource R :payload P) for REST,
  (:query Q :variables V) for GraphQL.
HANDLER returns (:value VALUE) or (:error ERR HEADERS), ERR as ghub
passes it to an errorback.  Each request is pushed on
`tether-test-requests'."
  (declare (indent 1) (debug t))
  `(let ((tether-test-requests nil)
         (handler ,handler))
     (cl-flet ((answer (request args)
                 (push request tether-test-requests)
                 (pcase (funcall handler request)
                   (`(:value ,value)
                    (funcall (plist-get args :callback) value nil nil nil))
                   (`(:error ,err ,headers)
                    (funcall (plist-get args :errorback) err headers nil nil))
                   (other (error "Bad test answer %S" other)))))
       (cl-letf (((symbol-function 'ghub-request)
                  (lambda (method resource _params &rest args)
                    (answer (list :method method :resource resource
                                  :payload (plist-get args :payload))
                            args)))
                 ((symbol-function 'ghub-query)
                  (lambda (query variables &rest args)
                    (answer (list :query query :variables variables) args))))
         ,@body))))

(defun tether-test-graphql-operation (request)
  "Return the name of the first field of REQUEST's GraphQL query, or nil."
  (when-let* ((query (plist-get request :query)))
    (car (nth 1 query))))

(provide 'tether-test-helpers)
;;; tether-test-helpers.el ends here
