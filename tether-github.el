;;; tether-github.el --- GitHub backend  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
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

(cl-defmethod tether--pullreq-ref ((_repo forge-github-repository) number)
  "Return GitHub's ref for the head of pull request NUMBER."
  (format "refs/pull/%d/head" number))

(cl-defmethod tether--pr-prefix ((_repo forge-github-repository))
  "Return the prefix of GitHub pull request numbers."
  "#")

(provide 'tether-github)
;;; tether-github.el ends here
