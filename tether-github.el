;;; tether-github.el --- GitHub backend  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; Assisted-by: Claude:claude-opus-5-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; GitHub implementations of the per-forge operations.

;;; Code:

(require 'tether-client)

(defun tether-github--pulls (repo number &optional suffix)
  "Return the REST path of pull request NUMBER of REPO, plus SUFFIX."
  (format "/repos/%s/%s/pulls/%d%s" (oref repo owner) (oref repo name) number
          (or suffix "")))

(cl-defmethod tether--fetch-diff-refs ((repo forge-github-repository) pullreq)
  "Return the head and base tip of PULLREQ of REPO.
GitHub's diff base is the merge-base of the two, computed locally."
  (let-alist (tether--request repo "GET"
                              (tether-github--pulls repo (oref pullreq number)))
    (unless (and .head.sha .base.sha)
      (user-error "Tether: GitHub returned no commits for #%s"
                  (oref pullreq number)))
    (list :head .head.sha
          :base nil
          :start nil
          :base-tip .base.sha
          :base-ref .base.ref)))

;;;; Comments

(defun tether-github--side (target)
  "Return GitHub's side of TARGET: RIGHT for the head, LEFT for the base."
  (if (eq (plist-get target :side) 'left) "LEFT" "RIGHT"))

(defun tether-github--line (target line)
  "Return the number of LINE on TARGET's side."
  (if (eq (plist-get target :side) 'left) (tether-line-old line)
    (tether-line-new line)))

(defun tether-github--comment-payload (ctx target body)
  "Return the REST payload commenting BODY on TARGET of CTX."
  (let ((start (plist-get target :start))
        (end (plist-get target :end))
        (side (tether-github--side target)))
    (tether--compact
     `((body . ,body)
       (commit_id . ,(tether-context-head-sha ctx))
       (path . ,(plist-get target :path))
       ,@(if (null end)
             '((subject_type . "file"))
           `((line . ,(tether-github--line target end))
             (side . ,side)
             ,@(unless (eq start end)
                 `((start_line . ,(tether-github--line target start))
                   (start_side . ,side)))))))))

(defun tether-github--thread-input (review target body)
  "Return the GraphQL input adding BODY on TARGET to pending REVIEW."
  (let ((start (plist-get target :start))
        (end (plist-get target :end))
        (side (tether-github--side target)))
    (tether--compact
     `((pullRequestReviewId . ,review)
       (path . ,(plist-get target :path))
       (body . ,body)
       ,@(if (null end)
             '((subjectType . "FILE"))
           `((subjectType . "LINE")
             (line . ,(tether-github--line target end))
             (side . ,side)
             ,@(unless (eq start end)
                 `((startLine . ,(tether-github--line target start))
                   (startSide . ,side)))))))))

(defconst tether-github--pending-review-query
  '(query (repository [(owner $owner String!) (name $name String!)]
                      (pullRequest [(number $number Int!)]
                                   (reviews [(states PENDING) (first 1)]
                                            (nodes id (comments totalCount))))))
  "Find the user's pending review; GitHub shows only one's own.")

(defconst tether-github--add-thread-mutation
  '(mutation (addPullRequestReviewThread
              [(input $input AddPullRequestReviewThreadInput!)]
              (thread id (comments [(first 1)] (nodes url)))))
  "Add a thread to a pending review.")

(defun tether-github--pending-review (repo number)
  "Return (ID . COMMENTS) of the user's pending review of PR NUMBER of REPO.
Return nil when there is none."
  (let-alist (tether--query repo tether-github--pending-review-query
                            `((owner . ,(oref repo owner))
                              (name . ,(oref repo name))
                              (number . ,number)))
    (when-let* ((review (car .repository.pullRequest.reviews.nodes)))
      (cons (alist-get 'id review)
            (or (alist-get 'totalCount (alist-get 'comments review)) 0)))))

(defun tether-github--add-to-review (repo number pending target body)
  "Add BODY on TARGET to the PENDING review of PR NUMBER of REPO.
PENDING is (ID . COMMENTS), as returned by `tether-github--pending-review'."
  (let* ((result (tether--query-call
                  repo tether-github--add-thread-mutation
                  `((input . ,(tether-github--thread-input (car pending)
                                                            target body)))))
         (data (plist-get result :value)))
    (unless (plist-get result :ok)
      (tether--rejected "GitHub" target result))
    (let-alist data
      (list :url (alist-get 'url (car .addPullRequestReviewThread.thread.comments.nodes))
            :message (format "added %s to your pending review of #%d \
(%d comments); submit it on GitHub"
                             (tether--target-label target) number
                             (1+ (cdr pending)))))))

(defun tether-github--pending-review-error-p (result)
  "Return non-nil if RESULT failed because of a pending review."
  (and (eq (plist-get result :status) 422)
       (string-match-p "pending review"
                       (or (tether--error-text (plist-get result :payload)) ""))))

(cl-defmethod tether--post-comment ((repo forge-github-repository) ctx target body)
  "Post BODY on TARGET of CTX's pull request on GitHub REPO.
While the user has a pending review, GitHub refuses single comments,
so the comment goes into that review instead."
  (let* ((number (oref (tether-context-pullreq ctx) number))
         (pending (tether-github--pending-review repo number)))
    (if pending
        (tether-github--add-to-review repo number pending target body)
      (let ((result (tether--call repo "POST"
                                  (tether-github--pulls repo number "/comments")
                                  nil :payload (tether-github--comment-payload
                                                ctx target body))))
        (cond
         ((plist-get result :ok)
          (list :url (alist-get 'html_url (plist-get result :value))
                :message (format "commented on %s (#%d)"
                                 (tether--target-label target) number)))
         ;; A pending review created since the check above.
         ((and (tether-github--pending-review-error-p result)
               (setq pending (tether-github--pending-review repo number)))
          (tether-github--add-to-review repo number pending target body))
         (t (tether--rejected "GitHub" target result)))))))

;;;; Threads

(defconst tether-github--threads-query
  '(query (repository [(owner $owner String!) (name $name String!)]
                      (pullRequest [(number $number Int!)]
                                   (reviewThreads [(:edges t)]
                                                  id isResolved isOutdated path line
                                                  startLine originalLine diffSide
                                                  (comments [(:edges t)]
                                                            id body createdAt url diffHunk
                                                            (author login)
                                                            (originalCommit oid))))))
  "The review threads of a pull request, every page of them.")

(defun tether-github--thread (node)
  "Return the `tether-thread' of the GraphQL reviewThread NODE."
  (let* ((comments (alist-get 'comments node))
         (first (car comments)))
    (let-alist node
      (make-tether-thread
       :id .id :path .path :line .line :start-line .startLine
       :side (if (equal .diffSide "LEFT") 'left 'right)
       :original-line .originalLine
       :original-sha (alist-get 'oid (alist-get 'originalCommit first))
       :diff-hunk (alist-get 'diffHunk first)
       :resolved .isResolved
       :outdated .isOutdated
       :comments (mapcar (lambda (comment)
                           (let-alist comment
                             (list :id .id :author .author.login
                                   :created .createdAt :body .body :url .url)))
                         comments)))))

(cl-defmethod tether--fetch-threads ((repo forge-github-repository) ctx done)
  "Fetch the review threads of CTX's pull request on GitHub REPO; call DONE."
  (tether--query-async
   repo tether-github--threads-query
   `((owner . ,(oref repo owner))
     (name . ,(oref repo name))
     (number . ,(oref (tether-context-pullreq ctx) number)))
   (lambda (result)
     (funcall done
              (if (plist-get result :ok)
                  (let-alist (plist-get result :value)
                    (list :ok t :threads (mapcar #'tether-github--thread
                                                 .repository.pullRequest.reviewThreads)))
                result)))))

(defconst tether-github--ci-query
  '(query (repository [(owner $owner String!) (name $name String!)]
                      (pullRequests [(:edges t) (states OPEN)]
                                    number
                                    (commits [(last 1)]
                                             (nodes (commit oid (statusCheckRollup state)))))))
  "The status of the checks on the head of each open pull request.")

(defun tether-github--ci-state (state)
  "Return tether's CI state for GitHub's StatusState STATE, or nil."
  (pcase state
    ("SUCCESS" 'success)
    ((or "FAILURE" "ERROR") 'failure)
    ((or "PENDING" "EXPECTED") 'running)))

(cl-defmethod tether--comment-outside-hunks-p ((_repo forge-github-repository))
  "GitHub takes comments on the lines of the diff's hunks only."
  nil)

(cl-defmethod tether--ci-url ((repo forge-github-repository) number)
  "Return the CI tab of pull request NUMBER of GitHub REPO."
  (format "%s/pull/%d/checks" (tether--web-repo repo) number))

(cl-defmethod tether--fetch-ci ((repo forge-github-repository) done)
  "Fetch the CI status of the open pull requests of GitHub REPO; call DONE."
  (tether--query-async
   repo tether-github--ci-query
   `((owner . ,(oref repo owner))
     (name . ,(oref repo name)))
   (lambda (result)
     (funcall
      done
      (if (not (plist-get result :ok))
          result
        (list :ok t
              :statuses
              (delq nil
                    (mapcar
                     (lambda (pullreq)
                       (let-alist pullreq
                         (let ((commit (alist-get 'commit (car .commits.nodes))))
                           (when-let* ((state (tether-github--ci-state
                                               (alist-get 'state (alist-get 'statusCheckRollup
                                                                            commit)))))
                             (list :number .number :state state
                                   :sha (alist-get 'oid commit)
                                   :url (tether--ci-url repo .number))))))
                     (let-alist (plist-get result :value)
                       .repository.pullRequests)))))))))

(defconst tether-github--reply-mutation
  '(mutation (addPullRequestReviewThreadReply
              [(input $input AddPullRequestReviewThreadReplyInput!)]
              (comment url)))
  "Reply in a review thread.")

(defun tether-github--resolve-mutation (resolved)
  "Return the mutation resolving a thread, or reopening it unless RESOLVED."
  (if resolved
      '(mutation (resolveReviewThread [(input $input ResolveReviewThreadInput!)]
                                      (thread id isResolved)))
    '(mutation (unresolveReviewThread [(input $input UnresolveReviewThreadInput!)]
                                      (thread id isResolved)))))

(defconst tether-github--delete-mutation
  '(mutation (deletePullRequestReviewComment
              [(input $input DeletePullRequestReviewCommentInput!)]
              clientMutationId))
  "Delete a review comment.")

(defun tether-github--mutate (repo mutation input what)
  "Send MUTATION with INPUT to GitHub REPO; return its data.
WHAT names the action in the error saying GitHub refused it."
  (let ((result (tether--query-call repo mutation `((input . ,input)))))
    (unless (plist-get result :ok)
      (user-error "Tether: GitHub refused to %s: %s" what
                  (plist-get result :message)))
    (plist-get result :value)))

(cl-defmethod tether--reply ((repo forge-github-repository) ctx thread body)
  "Post BODY in THREAD of CTX's pull request on GitHub REPO."
  (let-alist (tether-github--mutate
              repo tether-github--reply-mutation
              `((pullRequestReviewThreadId . ,(tether-thread-id thread))
                (body . ,body))
              "reply")
    (list :url .addPullRequestReviewThreadReply.comment.url
          :message (format "replied on %s (#%d)" (tether--thread-label thread)
                           (oref (tether-context-pullreq ctx) number)))))

(cl-defmethod tether--set-resolved ((repo forge-github-repository) _ctx thread resolved)
  "Resolve THREAD on GitHub REPO, or reopen it unless RESOLVED."
  (tether-github--mutate repo (tether-github--resolve-mutation resolved)
                         `((threadId . ,(tether-thread-id thread)))
                         (if resolved "resolve the thread" "reopen the thread")))

(defconst tether-github--update-mutation
  '(mutation (updatePullRequestReviewComment
              [(input $input UpdatePullRequestReviewCommentInput!)]
              (pullRequestReviewComment url)))
  "Change the text of a review comment.")

(cl-defmethod tether--update-comment ((repo forge-github-repository) _ctx comment body)
  "Replace the text of COMMENT on GitHub REPO by BODY."
  (let-alist (tether-github--mutate repo tether-github--update-mutation
                                    `((pullRequestReviewCommentId . ,(plist-get comment :id))
                                      (body . ,body))
                                    "edit the comment")
    .updatePullRequestReviewComment.pullRequestReviewComment.url))

(cl-defmethod tether--username ((repo forge-github-repository))
  "Return the login of the token's user on GitHub REPO's host."
  (tether-github--viewer repo))

(cl-defmethod tether--delete-comment ((repo forge-github-repository) _ctx comment)
  "Delete COMMENT on GitHub REPO."
  (tether-github--mutate repo tether-github--delete-mutation
                         `((id . ,(plist-get comment :id)))
                         "delete the comment"))

;;;; Reviews

(defconst tether-github--pullreq-id-query
  '(query (repository [(owner $owner String!) (name $name String!)]
                      (pullRequest [(number $number Int!)] id)))
  "The node id of a pull request.")

(defconst tether-github--viewer-query '(query (viewer login))
  "The login of the token's user.")

(defvar tether-github--viewers nil
  "Login of the token's user, by API host.")

(defun tether-github--pr-variables (repo ctx)
  "Return the GraphQL variables naming CTX's pull request on REPO."
  `((owner . ,(oref repo owner))
    (name . ,(oref repo name))
    (number . ,(oref (tether-context-pullreq ctx) number))))

(defun tether-github--pullreq-id (repo ctx)
  "Return the GraphQL node id of CTX's pull request on REPO."
  (or (oref (tether-context-pullreq ctx) their-id)
      (let-alist (tether--query repo tether-github--pullreq-id-query
                                (tether-github--pr-variables repo ctx))
        .repository.pullRequest.id)))

(defun tether-github--viewer (repo)
  "Return the login of the token's user on REPO's host."
  (let ((host (oref repo apihost)))
    (or (cdr (assoc host tether-github--viewers))
        (let ((login (let-alist (tether--query repo tether-github--viewer-query)
                       .viewer.login)))
          (push (cons host login) tether-github--viewers)
          login))))

(defun tether-github--check-not-own (repo ctx verdict)
  "Refuse VERDICT on the user's own pull request of CTX on REPO.
GitHub would refuse it too."
  (when (and (memq verdict '(approve request-changes))
             (equal (oref (tether-context-pullreq ctx) author)
                    (tether-github--viewer repo)))
    (user-error "Tether: GitHub does not let you give %s on your own pull request"
                (tether--verdict-name verdict))))

(defun tether-github--event (verdict)
  "Return GitHub's review event for VERDICT."
  (pcase verdict
    ('approve "APPROVE")
    ('request-changes "REQUEST_CHANGES")
    (_ "COMMENT")))

(cl-defmethod tether--review-state ((repo forge-github-repository) ctx done)
  "Fetch the user's pending review of CTX's PR on GitHub REPO; call DONE."
  (tether--query-async
   repo tether-github--pending-review-query (tether-github--pr-variables repo ctx)
   (lambda (result)
     (funcall done
              (if (plist-get result :ok)
                  (let-alist (plist-get result :value)
                    (let ((review (car .repository.pullRequest.reviews.nodes)))
                      (list :ok t
                            :review (list :id (alist-get 'id review)
                                          :drafts (or (alist-get
                                                       'totalCount
                                                       (alist-get 'comments review))
                                                      0)))))
                result)))))

(cl-defmethod tether--review-start ((repo forge-github-repository) ctx)
  "Create a pending review of CTX's pull request on GitHub REPO."
  (let-alist (tether-github--mutate
              repo '(mutation (addPullRequestReview
                               [(input $input AddPullRequestReviewInput!)]
                               (pullRequestReview id)))
              `((pullRequestId . ,(tether-github--pullreq-id repo ctx)))
              "start a review")
    .addPullRequestReview.pullRequestReview.id))

(cl-defmethod tether--review-submit ((repo forge-github-repository) ctx verdict body)
  "Submit the pending review of CTX's PR on GitHub REPO with VERDICT and BODY.
Without a pending review, give the verdict as a review of its own."
  (tether-github--check-not-own repo ctx verdict)
  (let* ((number (oref (tether-context-pullreq ctx) number))
         (pending (tether-github--pending-review repo number))
         (body (and (not (string-empty-p body)) body)))
    (if pending
        (tether-github--mutate
         repo '(mutation (submitPullRequestReview
                          [(input $input SubmitPullRequestReviewInput!)]
                          (pullRequestReview id)))
         (tether--compact `((pullRequestReviewId . ,(car pending))
                            (event . ,(tether-github--event verdict))
                            (body . ,body)))
         "submit the review")
      (tether-github--mutate
       repo '(mutation (addPullRequestReview
                        [(input $input AddPullRequestReviewInput!)]
                        (pullRequestReview id)))
       (tether--compact `((pullRequestId . ,(tether-github--pullreq-id repo ctx))
                          (event . ,(tether-github--event verdict))
                          (body . ,body)))
       "give the review"))
    (format "submitted the review of #%d as %s%s" number
            (tether--verdict-name verdict)
            (if pending (format " (%d comments)" (cdr pending)) ""))))

(cl-defmethod tether--review-discard ((repo forge-github-repository) ctx)
  "Delete the pending review of CTX's pull request on GitHub REPO."
  (when-let* ((pending (tether-github--pending-review
                        repo (oref (tether-context-pullreq ctx) number))))
    (tether-github--mutate
     repo '(mutation (deletePullRequestReview
                      [(input $input DeletePullRequestReviewInput!)]
                      (pullRequestReview id)))
     `((pullRequestReviewId . ,(car pending)))
     "discard the review")))

(cl-defmethod tether--approve ((repo forge-github-repository) ctx)
  "Approve CTX's pull request on GitHub REPO."
  (tether--review-submit repo ctx 'approve ""))

(cl-defmethod tether--pullreq-ref ((_repo forge-github-repository) number)
  "Return GitHub's ref for the head of pull request NUMBER."
  (format "refs/pull/%d/head" number))

(cl-defmethod tether--pr-prefix ((_repo forge-github-repository))
  "Return the prefix of GitHub pull request numbers."
  "#")

(provide 'tether-github)
;;; tether-github.el ends here
