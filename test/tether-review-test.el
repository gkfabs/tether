;;; tether-review-test.el --- Tests for draft reviews and verdicts  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Draft reviews on both forges, with the API stubbed: the requests of
;; each operation, GitLab's change requests and their checks, and the
;; commands.

;;; Code:

(require 'ert)
(require 'tether)
(require 'tether-test-helpers)

(defun tether-review-test--ctx (repo &optional review)
  "Return a context of PR 7 of REPO, with REVIEW as its draft review state."
  (tether-context--make :repo repo :pullreq (tether-test-pullreq)
                        :toplevel "/nonexistent/tether-test/"
                        :base-sha "BASE" :start-sha "START" :head-sha "HEAD"
                        :threads-state 'done :review review))

(defun tether-review-test--request (operation)
  "Return the last request of GraphQL OPERATION, or REST METHOD+RESOURCE."
  (seq-find (lambda (request)
              (if (symbolp operation)
                  (eq (tether-test-graphql-operation request) operation)
                (equal (concat (plist-get request :method) " "
                               (plist-get request :resource))
                       operation)))
            tether-test-requests))

;;;; GitHub

(defun tether-review-test--github (&optional pending viewer)
  "Return a GitHub handler: PENDING (ID . COUNT) review, VIEWER login."
  (lambda (request)
    (let ((variables (plist-get request :variables)))
      (pcase (tether-test-graphql-operation request)
        ('repository
         (if (assq 'number variables)
             (if (equal (plist-get request :query) tether-github--pullreq-id-query)
                 '(:value ((repository (pullRequest (id . "PR_node")))))
               `(:value ((repository
                          (pullRequest
                           (reviews
                            (nodes ,@(and pending
                                          `(((id . ,(car pending))
                                             (comments (totalCount . ,(cdr pending)))))))))))))
           '(:value nil)))
        ('viewer `(:value ((viewer (login . ,(or viewer "carol"))))))
        ('addPullRequestReview
         '(:value ((addPullRequestReview (pullRequestReview (id . "R_new"))))))
        (_ '(:value ((ok . t))))))))

(ert-deftest tether-review-test-github-state ()
  (let ((repo (tether-test-github-repo))
        result)
    (tether-test-with-handler (tether-review-test--github '("R1" . 2))
      (tether--review-state repo (tether-review-test--ctx repo)
                            (lambda (r) (setq result r))))
    (should (equal (plist-get result :review) '(:id "R1" :drafts 2)))))

(ert-deftest tether-review-test-github-start ()
  "The pending review is created on the PR's node id."
  (let ((repo (tether-test-github-repo)))
    (tether-test-with-handler (tether-review-test--github)
      (should (equal (tether--review-start repo (tether-review-test--ctx repo))
                     "R_new"))
      (should (equal (plist-get (tether-review-test--request 'addPullRequestReview)
                                :variables)
                     '((input (pullRequestId . "PR_node"))))))))

(ert-deftest tether-review-test-github-submit ()
  (let ((repo (tether-test-github-repo)))
    ;; With a pending review: submitted with the event and summary.
    (tether-test-with-handler (tether-review-test--github '("R1" . 2))
      (should (equal (tether--review-submit repo (tether-review-test--ctx repo)
                                            'approve "LGTM")
                     "submitted the review of #7 as an approval (2 comments)"))
      (should (equal (plist-get (tether-review-test--request 'submitPullRequestReview)
                                :variables)
                     '((input (pullRequestReviewId . "R1") (event . "APPROVE")
                              (body . "LGTM"))))))
    ;; Without: a review of its own, no empty body sent.
    (tether-test-with-handler (tether-review-test--github)
      (tether--review-submit repo (tether-review-test--ctx repo) 'request-changes "")
      (should (equal (plist-get (tether-review-test--request 'addPullRequestReview)
                                :variables)
                     '((input (pullRequestId . "PR_node")
                              (event . "REQUEST_CHANGES"))))))))

(ert-deftest tether-review-test-github-own-pr ()
  "No approval or change request on one's own PR; a comment is fine."
  (let ((repo (tether-test-github-repo))
        (tether-github--viewers nil))
    (tether-test-with-handler (tether-review-test--github nil "alice")
      (should-error (tether--review-submit repo (tether-review-test--ctx repo)
                                           'approve "")
                    :type 'user-error)
      (tether--review-submit repo (tether-review-test--ctx repo) 'comment "Notes"))))

(ert-deftest tether-review-test-github-discard ()
  (let ((repo (tether-test-github-repo)))
    (tether-test-with-handler (tether-review-test--github '("R1" . 2))
      (tether--review-discard repo (tether-review-test--ctx repo))
      (should (equal (plist-get (tether-review-test--request 'deletePullRequestReview)
                                :variables)
                     '((input (pullRequestReviewId . "R1"))))))))

;;;; GitLab

(defun tether-review-test--gitlab (&optional reviewers after drafts)
  "Return a GitLab handler.
REVIEWERS is the reviewer list before publishing, AFTER after it;
DRAFTS the draft notes."
  (let ((published nil))
    (lambda (request)
      (let ((resource (plist-get request :resource))
            (method (plist-get request :method)))
        (cond
         ((equal resource "/user") '(:value ((username . "fab"))))
         ((string-suffix-p "/reviewers" resource)
          `(:value ,(if published after reviewers)))
         ((string-suffix-p "/bulk_publish" resource)
          (setq published t)
          '(:value nil))
         ((and (equal method "GET") (string-suffix-p "/draft_notes" resource))
          `(:value ,drafts))
         ((string-suffix-p "/discussions" resource) '(:value nil))
         (t '(:value ((id . 5)))))))))

(defconst tether-review-test--me-requested
  '(((user (username . "fab")) (state . "requested_changes")))
  "The reviewers once GitLab recorded the change request.")

(ert-deftest tether-review-test-gitlab-draft ()
  "During a review, comments are draft notes."
  (let* ((repo (tether-test-gitlab-repo))
         (ctx (tether-review-test--ctx repo '(:active t :drafts 0)))
         (map (car (tether-diff-parse
                    "diff --git a/a.el b/a.el\n--- a/a.el\n+++ b/a.el\n@@ -1 +1 @@\n-x\n+y\n")))
         (line (tether-file-map-line map 1)))
    (tether-test-with-handler (tether-review-test--gitlab)
      (let ((result (tether--post-comment repo ctx
                                          (list :path "a.el" :start line :end line
                                                :side 'right)
                                          "Hm")))
        (should (string-prefix-p "added a.el:1 to your draft review of !7"
                                 (plist-get result :message)))
        (let ((request (car tether-test-requests)))
          (should (equal (plist-get request :resource)
                         "/projects/g%2Fsub%2Fp/merge_requests/7/draft_notes"))
          (should (equal (alist-get 'note (plist-get request :payload)) "Hm"))
          (should (alist-get 'position (plist-get request :payload))))))))

(ert-deftest tether-review-test-gitlab-submit-comment ()
  "A comment verdict publishes, without changing the reviewer status."
  (let ((repo (tether-test-gitlab-repo)))
    (tether-test-with-handler (tether-review-test--gitlab)
      (tether--review-submit repo (tether-review-test--ctx repo) 'comment "Thanks")
      (should (equal (plist-get (car tether-test-requests) :payload)
                     '((note . "Thanks")))))))

(ert-deftest tether-review-test-gitlab-submit-approve ()
  "An approval publishes the drafts, then approves the head reviewed."
  (let ((repo (tether-test-gitlab-repo)))
    (tether-test-with-handler (tether-review-test--gitlab)
      (tether--review-submit repo (tether-review-test--ctx repo) 'approve "")
      (should (equal (mapcar (lambda (r) (plist-get r :resource))
                             (reverse tether-test-requests))
                     '("/projects/g%2Fsub%2Fp/merge_requests/7/draft_notes/bulk_publish"
                       "/projects/g%2Fsub%2Fp/merge_requests/7/approve")))
      (should (equal (plist-get (car tether-test-requests) :payload)
                     '((sha . "HEAD")))))))

(ert-deftest tether-review-test-gitlab-request-changes ()
  "As a reviewer: published with reviewer_state, and read back."
  (let ((repo (tether-test-gitlab-repo))
        (tether-gitlab--users nil))
    (tether-test-with-handler
        (tether-review-test--gitlab '(((user (username . "fab")) (state . "unreviewed")))
                                    tether-review-test--me-requested)
      (should (equal (tether--review-submit repo (tether-review-test--ctx repo)
                                            'request-changes "Please split")
                     "submitted the review of !7 as a request for changes"))
      (should (equal (plist-get (tether-review-test--request
                                 "POST /projects/g%2Fsub%2Fp/merge_requests/7/draft_notes/bulk_publish")
                                :payload)
                     '((note . "Please split") (reviewer_state . "requested_changes")))))))

(ert-deftest tether-review-test-gitlab-request-changes-not-reviewer ()
  "Not a reviewer: asked first; declining submits nothing."
  (let ((repo (tether-test-gitlab-repo))
        (tether-gitlab--users nil)
        (asked nil))
    (tether-test-with-handler (tether-review-test--gitlab nil tether-review-test--me-requested)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (setq asked prompt) nil)))
        (should-error (tether--review-submit repo (tether-review-test--ctx repo)
                                             'request-changes "")
                      :type 'user-error))
      (should (string-match-p "You are not a reviewer of !7; GitLab will add you" asked))
      (should-not (tether-review-test--request
                   "POST /projects/g%2Fsub%2Fp/merge_requests/7/draft_notes/bulk_publish")))))

(ert-deftest tether-review-test-gitlab-request-changes-author ()
  "The author cannot request changes on GitLab."
  (let ((repo (tether-test-gitlab-repo))
        (tether-gitlab--users '(("gitlab.com/api/v4" . "alice"))))
    (tether-test-with-handler (tether-review-test--gitlab)
      (should-error (tether--review-submit repo (tether-review-test--ctx repo)
                                           'request-changes "")
                    :type 'user-error))))

(ert-deftest tether-review-test-gitlab-request-changes-fallback ()
  "An instance ignoring reviewer_state gets the quick action, and we say so."
  (let ((repo (tether-test-gitlab-repo))
        (tether-gitlab--users nil))
    (tether-test-with-handler
        (tether-review-test--gitlab '(((user (username . "fab"))))
                                    '(((user (username . "fab")) (state . "unreviewed"))))
      (should (string-match-p "GitLab did not record the request for changes"
                              (tether--review-submit repo (tether-review-test--ctx repo)
                                                     'request-changes "")))
      (should (equal (plist-get (tether-review-test--request
                                 "POST /projects/g%2Fsub%2Fp/merge_requests/7/notes")
                                :payload)
                     '((body . "/request_changes")))))))

(ert-deftest tether-review-test-gitlab-discard ()
  (let ((repo (tether-test-gitlab-repo)))
    (tether-test-with-handler (tether-review-test--gitlab nil nil '(((id . 1)) ((id . 2))))
      (tether--review-discard repo (tether-review-test--ctx repo))
      (should (tether-review-test--request
               "DELETE /projects/g%2Fsub%2Fp/merge_requests/7/draft_notes/1"))
      (should (tether-review-test--request
               "DELETE /projects/g%2Fsub%2Fp/merge_requests/7/draft_notes/2")))))

;;;; Commands

(defmacro tether-review-test-with (repo handler review &rest body)
  "Run BODY with `ctx', a context of REPO with REVIEW, and HANDLER."
  (declare (indent 3) (debug t))
  `(let* ((ctx (tether-review-test--ctx ,repo ,review))
          (tether--started-reviews nil))
     (cl-letf (((symbol-function 'tether-context) (lambda (&rest _) ctx))
               ((symbol-function 'tether-context-current-p) (lambda (_) t))
               ((symbol-function 'tether--context-fresh-p) (lambda (_) t)))
       (tether-test-with-handler ,handler
         ,@body))))

(ert-deftest tether-review-test-start-and-submit ()
  "Start on GitLab, then submit from the editor with an empty summary."
  (tether-review-test-with (tether-test-gitlab-repo) (tether-review-test--gitlab)
      '(:drafts 0)
    (tether-review-start)
    (should (tether--review-active-p ctx))
    (should (member (tether--context-key ctx) tether--started-reviews))
    (let ((editor (tether-review-submit 'comment)))
      (with-current-buffer editor
        (should (string-match-p "publishes your 0 draft comments" (buffer-string)))
        (tether-comment-send))
      (should-not (buffer-live-p editor)))
    (should (tether-review-test--request
             "POST /projects/g%2Fsub%2Fp/merge_requests/7/draft_notes/bulk_publish"))
    (should-not (tether--review-active-p ctx))
    (should-not tether--started-reviews)))

(ert-deftest tether-review-test-discard-command ()
  (tether-review-test-with (tether-test-gitlab-repo)
      (tether-review-test--gitlab nil nil '(((id . 1))))
      '(:active t :drafts 1)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (should (equal prompt "Delete your review and its 1 draft comment? "))
                 t)))
      (tether-review-discard))
    (should-not (tether--review-active-p ctx))))

(ert-deftest tether-review-test-approve-github-pending ()
  "On GitHub, approving during a review goes through it."
  (tether-review-test-with (tether-test-github-repo)
      (tether-review-test--github '("R1" . 1)) '(:active t :id "R1" :drafts 1)
    (let ((tether-github--viewers '(("api.github.com" . "carol"))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
        (should-error (tether-review-approve) :type 'user-error))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
        (tether-review-approve))
      (should (tether-review-test--request 'submitPullRequestReview)))))

(ert-deftest tether-review-test-lighter ()
  "The mode line shows the drafts of a review in progress."
  (let ((tether--contexts (make-hash-table :test #'equal))
        (ctx (tether-review-test--ctx (tether-test-github-repo)
                                      '(:active t :drafts 3))))
    (puthash (cons "/r/" 7) ctx tether--contexts)
    (cl-letf (((symbol-function 'magit-toplevel) (lambda (&rest _) "/r/")))
      (with-temp-buffer
        (tether--update-lighter (cons (tether-test-github-repo) (tether-test-pullreq)))
        (should (equal tether--lighter " #7✎3"))))))

(provide 'tether-review-test)
;;; tether-review-test.el ends here
