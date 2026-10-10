;;; tether-reply-test.el --- Tests for reply, resolve and delete  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replying in, resolving and deleting comments of review threads, on
;; both forges, with the API stubbed: the requests, and the commands
;; from a file line and the thread window.

;;; Code:

(require 'ert)
(require 'tether)
(require 'tether-test-helpers)

(defun tether-reply-test--thread (&optional resolved)
  "Return a thread on a.el:2 with two comments; RESOLVED if non-nil."
  (make-tether-thread
   :id "T1" :path "a.el" :line 2 :side 'right :original-line 2
   :resolved resolved
   :comments (list (list :id "C1" :author "alice" :body "Off by one?")
                   (list :id "C2" :author "bob" :body "Fixed: thanks"))))

(defun tether-reply-test--ctx (repo)
  "Return a context of PR 7 of REPO."
  (tether-context--make :repo repo :pullreq (tether-test-pullreq)
                        :toplevel "/nonexistent/tether-test/"
                        :base-sha "BASE" :start-sha "START" :head-sha "HEAD"
                        :threads-state 'done))

(defun tether-reply-test--answer (request)
  "Answer REQUEST like GitHub or GitLab would."
  (pcase (tether-test-graphql-operation request)
    ('addPullRequestReviewThreadReply
     '(:value ((addPullRequestReviewThreadReply
                (comment (url . "https://github.com/o/r/pull/7#r9"))))))
    ('repository '(:value ((repository (pullRequest (reviewThreads))))))
    ('nil (if (equal (plist-get request :method) "POST")
              '(:value ((id . 42)))
            '(:value nil)))
    (_ '(:value ((ok . t))))))

;;;; Requests

(ert-deftest tether-reply-test-github-requests ()
  (let ((repo (tether-test-github-repo))
        (ctx (tether-reply-test--ctx (tether-test-github-repo))))
    (tether-test-with-handler #'tether-reply-test--answer
      (let ((result (tether--reply repo ctx (tether-reply-test--thread) "Done")))
        (should (equal (plist-get result :url) "https://github.com/o/r/pull/7#r9"))
        (should (equal (plist-get result :message) "replied on a.el:2 (#7)"))
        (should (equal (plist-get (car tether-test-requests) :variables)
                       '((input (pullRequestReviewThreadId . "T1") (body . "Done"))))))
      (tether--set-resolved repo ctx (tether-reply-test--thread) t)
      (should (eq (tether-test-graphql-operation (car tether-test-requests))
                  'resolveReviewThread))
      (tether--set-resolved repo ctx (tether-reply-test--thread) nil)
      (should (eq (tether-test-graphql-operation (car tether-test-requests))
                  'unresolveReviewThread))
      (should (equal (plist-get (car tether-test-requests) :variables)
                     '((input (threadId . "T1")))))
      (tether--delete-comment repo ctx '(:id "C2"))
      (should (eq (tether-test-graphql-operation (car tether-test-requests))
                  'deletePullRequestReviewComment))
      (should (equal (plist-get (car tether-test-requests) :variables)
                     '((input (id . "C2"))))))))

(ert-deftest tether-reply-test-github-refused ()
  (let ((repo (tether-test-github-repo)))
    (tether-test-with-handler
        (lambda (_) '(:error (((message . "Resource not accessible by integration"))) nil))
      (let ((err (should-error (tether--set-resolved repo (tether-reply-test--ctx repo)
                                                (tether-reply-test--thread) t)
                               :type 'user-error)))
        (should (string-match-p "GitHub refused to resolve the thread: .*not accessible"
                                (cadr err)))))))

(ert-deftest tether-reply-test-gitlab-requests ()
  (let ((repo (tether-test-gitlab-repo))
        (ctx (tether-reply-test--ctx (tether-test-gitlab-repo))))
    (tether-test-with-handler #'tether-reply-test--answer
      (let ((result (tether--reply repo ctx (tether-reply-test--thread) "Done")))
        (should (equal (plist-get (car tether-test-requests) :resource)
                       "/projects/g%2Fsub%2Fp/merge_requests/7/discussions/T1/notes"))
        (should (equal (plist-get result :url)
                       "https://gitlab.com/g/sub/p/-/merge_requests/7#note_42")))
      (tether--set-resolved repo ctx (tether-reply-test--thread) t)
      (should (equal (list (plist-get (car tether-test-requests) :method)
                           (plist-get (car tether-test-requests) :resource)
                           (plist-get (car tether-test-requests) :payload))
                     '("PUT" "/projects/g%2Fsub%2Fp/merge_requests/7/discussions/T1"
                       ((resolved . t)))))
      ;; Reopening sends false, which ghub encodes from nil.
      (tether--set-resolved repo ctx (tether-reply-test--thread) nil)
      (should (equal (plist-get (car tether-test-requests) :payload)
                     '((resolved))))
      (tether--delete-comment repo ctx '(:id 42))
      (should (equal (list (plist-get (car tether-test-requests) :method)
                           (plist-get (car tether-test-requests) :resource))
                     '("DELETE" "/projects/g%2Fsub%2Fp/merge_requests/7/notes/42"))))))

;;;; Commands

(defmacro tether-reply-test-with (&rest body)
  "Run BODY in the thread window of a GitHub thread on a.el:2.
`ctx', `thread' and `tether-test-requests' are bound."
  (declare (indent 0) (debug t))
  `(let* ((ctx (tether-reply-test--ctx (tether-test-github-repo)))
          (thread (tether-reply-test--thread)))
     (cl-letf (((symbol-function 'tether-context-current-p) (lambda (_) t))
               ((symbol-function 'tether--context-fresh-p) (lambda (_) t)))
       (tether-test-with-handler #'tether-reply-test--answer
         (with-temp-buffer
           (tether-thread-mode)
           (tether-thread--render ctx thread)
           ,@body)))))

(ert-deftest tether-reply-test-resolve-command ()
  "From the thread window: resolve, and fetch the threads again."
  (tether-reply-test-with
    (tether-resolve)
    (should (memq 'resolveReviewThread
                  (mapcar #'tether-test-graphql-operation tether-test-requests)))
    (should (eq (tether-test-graphql-operation (car tether-test-requests))
                'repository))))

(ert-deftest tether-reply-test-delete-command ()
  "Choose the comment, confirm, delete; declining deletes nothing."
  (tether-reply-test-with
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _)
                 (should (equal (mapcar #'car choices)
                                '("alice: Off by one?" "bob: Fixed: thanks")))
                 "bob: Fixed: thanks"))
              ((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (tether-delete-comment)
      (should-not tether-test-requests))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "bob: Fixed: thanks"))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (should (equal prompt "Delete bob's comment “Fixed: thanks”? "))
                 t)))
      (tether-delete-comment)
      (should (equal (plist-get
                      (seq-find (lambda (r) (eq (tether-test-graphql-operation r)
                                                'deletePullRequestReviewComment))
                                tether-test-requests)
                      :variables)
                     '((input (id . "C2"))))))))

(ert-deftest tether-reply-test-reply-command ()
  "The reply is written in the editor, with the discussion below."
  (tether-reply-test-with
    (let ((editor (tether-reply)))
      (with-current-buffer editor
        (should (equal (buffer-name editor) "*tether-comment o/r#7 reply on a.el:2*"))
        (should (string-match-p "the discussion, for reference" (buffer-string)))
        (should (string-match-p "Fixed: thanks" (buffer-string)))
        (insert "Agreed")
        (tether-comment-send))
      (should-not (buffer-live-p editor))
      (should (equal (plist-get
                      (seq-find (lambda (r) (eq (tether-test-graphql-operation r)
                                                'addPullRequestReviewThreadReply))
                                tether-test-requests)
                      :variables)
                     '((input (pullRequestReviewThreadId . "T1")
                              (body . "Agreed"))))))))

(provide 'tether-reply-test)
;;; tether-reply-test.el ends here
