;;; tether-gitlab.el --- GitLab backend  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; Assisted-by: Claude:claude-opus-5-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; GitLab implementations of the per-forge operations.

;;; Code:

(require 'seq)
(require 'tether-client)

(defun tether-gitlab--merge-request (repo number &optional suffix)
  "Return the REST path of merge request NUMBER of REPO, plus SUFFIX."
  (format "/projects/%s/merge_requests/%d%s" (tether--project-id repo) number
          (or suffix "")))

(cl-defmethod tether--fetch-diff-refs ((repo forge-gitlab-repository) pullreq)
  "Return the diff refs of merge request PULLREQ of REPO."
  (let ((number (oref pullreq number)))
    (let-alist (tether--request repo "GET"
                                (tether-gitlab--merge-request repo number))
      ;; Null while GitLab prepares the diff, or when the MR has no
      ;; commits.
      (unless (and .diff_refs.base_sha .diff_refs.head_sha)
        (user-error "Tether: GitLab has not computed the diff of !%s yet; \
retry in a moment" number))
      (list :head .diff_refs.head_sha
            :base .diff_refs.base_sha
            :start .diff_refs.start_sha
            :base-tip nil
            :base-ref .target_branch))))

;;;; Lines

(cl-defmethod tether--comment-outside-hunks-p ((_repo forge-gitlab-repository))
  "GitLab takes comments on any line of a changed file.
An unchanged line outside the hunks has both its line numbers, as in
the hunks."
  t)

;;;; CI

(defconst tether-gitlab--ci-query
  '(query (project [(fullPath $path ID!)]
                   (mergeRequests [(:edges t) (state opened)]
                                  iid
                                  (headPipeline status sha path))))
  "The head pipeline of each open merge request.")

(defun tether-gitlab--ci-state (status)
  "Return tether's CI state for GitLab's pipeline STATUS, or nil."
  (pcase status
    ("SUCCESS" 'success)
    ("FAILED" 'failure)
    ((or "CANCELED" "CANCELING" "SKIPPED" "MANUAL") 'canceled)
    ((or "CREATED" "WAITING_FOR_RESOURCE" "PREPARING" "PENDING" "RUNNING"
         "SCHEDULED")
     'running)))

(cl-defmethod tether--ci-url ((repo forge-gitlab-repository) number)
  "Return the pipelines tab of merge request NUMBER of GitLab REPO.
For a merge request whose head pipeline is unknown."
  (format "%s/-/merge_requests/%d/pipelines" (tether--web-repo repo) number))

(cl-defmethod tether--fetch-ci ((repo forge-gitlab-repository) done)
  "Fetch the head pipelines of the open merge requests of REPO; call DONE."
  (tether--query-async
   repo tether-gitlab--ci-query
   `((path . ,(format "%s/%s" (oref repo owner) (oref repo name))))
   (lambda (result)
     (funcall
      done
      (if (not (plist-get result :ok))
          result
        (list :ok t
              :statuses
              (delq nil
                    (mapcar
                     (lambda (mr)
                       (let-alist mr
                         (when-let* ((state (tether-gitlab--ci-state .headPipeline.status)))
                           (list :number (string-to-number .iid) :state state
                                 :sha .headPipeline.sha
                                 :url (format "https://%s%s" (oref repo githost)
                                              .headPipeline.path)))))
                     (let-alist (plist-get result :value)
                       .project.mergeRequests)))))))))

;;;; Comments

(defun tether-gitlab--line-code (path line)
  "Return GitLab's line_code of LINE in file PATH.
That is the SHA-1 of the path, then the old and new line counters."
  (format "%s_%d_%d" (secure-hash 'sha1 (encode-coding-string path 'utf-8))
          (car (tether-line-code line)) (cdr (tether-line-code line))))

(defun tether-gitlab--range-point (path line)
  "Return one end of a line_range, for LINE of file PATH."
  (tether--compact
   `((line_code . ,(tether-gitlab--line-code path line))
     (type . ,(pcase (tether-line-kind line) (:added "new") (:removed "old")))
     (old_line . ,(tether-line-old line))
     (new_line . ,(tether-line-new line)))))

(defun tether-gitlab--position (ctx target)
  "Return the position of a comment on TARGET of CTX.
GitLab anchors a comment on the last line of its range; a context
line needs both line numbers."
  (let ((path (plist-get target :path))
        (start (plist-get target :start))
        (end (plist-get target :end)))
    (tether--compact
     `((position_type . "text")
       (base_sha . ,(tether-context-base-sha ctx))
       (start_sha . ,(tether-context-start-sha ctx))
       (head_sha . ,(tether-context-head-sha ctx))
       (old_path . ,(or (plist-get target :old-path) path))
       (new_path . ,path)
       (old_line . ,(tether-line-old end))
       (new_line . ,(tether-line-new end))
       (line_range . ,(unless (eq start end)
                        `((start . ,(tether-gitlab--range-point path start))
                          (end . ,(tether-gitlab--range-point path end)))))))))

(defun tether-gitlab--note-url (repo number note)
  "Return the web page of NOTE id on merge request NUMBER of REPO."
  (format "https://%s/%s/%s/-/merge_requests/%d#note_%s"
          (oref repo githost) (oref repo owner) (oref repo name) number note))

(cl-defmethod tether--post-comment ((repo forge-gitlab-repository) ctx target body)
  "Post BODY on TARGET of CTX's merge request on GitLab REPO.
A comment on the whole file is a general note naming the file.
During a draft review, the comment is a draft note."
  (if (plist-get (tether-context-review ctx) :active)
      (tether-gitlab--post-draft repo ctx target body)
    (tether-gitlab--post-now repo ctx target body)))

(defun tether-gitlab--post-draft (repo ctx target body)
  "Add BODY on TARGET of CTX's merge request on GitLab REPO as a draft."
  (let* ((number (oref (tether-context-pullreq ctx) number))
         (result (tether--call
                  repo "POST" (tether-gitlab--merge-request repo number "/draft_notes")
                  nil :payload
                  (if (plist-get target :end)
                      (tether--compact
                       `((note . ,body)
                         (commit_id . ,(tether-context-commit ctx))
                         (position . ,(tether-gitlab--position ctx target))))
                    `((note . ,(format "`%s`\n\n%s" (plist-get target :path)
                                       body)))))))
    (unless (plist-get result :ok)
      (tether--rejected "GitLab" target result))
    (list :message (format "added %s to your draft review of !%d; submit it \
with M-x tether-review-submit" (tether--target-label target) number))))

(defun tether-gitlab--post-now (repo ctx target body)
  "Post BODY on TARGET of CTX's merge request on GitLab REPO, published."
  (let* ((number (oref (tether-context-pullreq ctx) number))
         (file-p (null (plist-get target :end)))
         (result
          (if file-p
              (tether--call repo "POST"
                            (tether-gitlab--merge-request repo number "/notes")
                            nil :payload
                            `((body . ,(format "`%s`\n\n%s"
                                               (plist-get target :path) body))))
            (tether--call repo "POST"
                          (tether-gitlab--merge-request repo number "/discussions")
                          nil :payload
                          (tether--compact
                           `((body . ,body)
                             ;; A single-commit review: GitLab's commit_id,
                             ;; and a position on the commit's own diff.
                             (commit_id . ,(tether-context-commit ctx))
                             (position . ,(tether-gitlab--position ctx target))))))))
    (unless (plist-get result :ok)
      (tether--rejected "GitLab" target result))
    (let* ((value (plist-get result :value))
           (note (if file-p (alist-get 'id value)
                   (alist-get 'id (car (alist-get 'notes value))))))
      (list :url (tether-gitlab--note-url repo number note)
            :message (format "commented on %s (!%d)"
                             (tether--target-label target) number)))))

;;;; Threads

(defun tether-gitlab--thread (repo number discussion)
  "Return the `tether-thread' of DISCUSSION on merge request NUMBER of REPO.
Return nil for a discussion that is not on a line of the diff."
  (let* ((notes (seq-remove (lambda (note) (alist-get 'system note))
                            (alist-get 'notes discussion)))
         (first (car notes))
         (position (alist-get 'position first)))
    (when (and position (equal (alist-get 'type first) "DiffNote"))
      (let-alist position
        (let ((right (and .new_line t)))
          (make-tether-thread
           :id (alist-get 'id discussion)
           :path (or .new_path .old_path)
           :line (if right .new_line .old_line)
           :side (if right 'right 'left)
           :start-line (let-alist .line_range
                         (and (or .start.new_line .start.old_line)
                              (if right .start.new_line .start.old_line)))
           :head-sha .head_sha
           :original-line (if right .new_line .old_line)
           :original-sha .head_sha
           :resolved (alist-get 'resolved first)
           :comments (mapcar (lambda (note)
                               (let-alist note
                                 (list :id .id :author .author.username
                                       :created .created_at :body .body
                                       :url (tether-gitlab--note-url
                                             repo number .id))))
                             notes)))))))

(cl-defmethod tether--fetch-threads ((repo forge-gitlab-repository) ctx done)
  "Fetch the discussions of CTX's merge request on GitLab REPO; call DONE."
  (let ((number (oref (tether-context-pullreq ctx) number)))
    (tether--call-async
     repo "GET" (tether-gitlab--merge-request repo number "/discussions")
     (lambda (result)
       (funcall done
                (if (plist-get result :ok)
                    (list :ok t
                          :threads (delq nil (mapcar (lambda (discussion)
                                                       (tether-gitlab--thread
                                                        repo number discussion))
                                                     (plist-get result :value))))
                  result)))
     :unpaginate t)))

(defun tether-gitlab--refused (result what)
  "Signal that GitLab refused to do WHAT, as told by the failed RESULT."
  (user-error "Tether: GitLab refused to %s: %s" what
              (or (tether--error-text (plist-get result :payload))
                  (plist-get result :message))))

(cl-defmethod tether--reply ((repo forge-gitlab-repository) ctx thread body)
  "Post BODY in THREAD of CTX's merge request on GitLab REPO."
  (let* ((number (oref (tether-context-pullreq ctx) number))
         (result (tether--call
                  repo "POST"
                  (tether-gitlab--merge-request
                   repo number (format "/discussions/%s/notes" (tether-thread-id thread)))
                  nil :payload `((body . ,body)))))
    (unless (plist-get result :ok)
      (tether-gitlab--refused result "reply"))
    (list :url (tether-gitlab--note-url repo number
                                        (alist-get 'id (plist-get result :value)))
          :message (format "replied on %s (!%d)" (tether--thread-label thread)
                           number))))

(cl-defmethod tether--set-resolved ((repo forge-gitlab-repository) ctx thread resolved)
  "Resolve THREAD of CTX's merge request on GitLab REPO, or reopen it.
RESOLVED says which."
  (let ((result (tether--call
                 repo "PUT"
                 (tether-gitlab--merge-request
                  repo (oref (tether-context-pullreq ctx) number)
                  (format "/discussions/%s" (tether-thread-id thread)))
                 ;; nil is sent as JSON false.
                 nil :payload `((resolved . ,(and resolved t))))))
    (unless (plist-get result :ok)
      (tether-gitlab--refused result (if resolved "resolve the thread"
                                       "reopen the thread")))))

(cl-defmethod tether--update-comment ((repo forge-gitlab-repository) ctx comment body)
  "Replace the text of COMMENT of CTX's merge request on GitLab REPO by BODY."
  (let* ((number (oref (tether-context-pullreq ctx) number))
         (result (tether--call
                  repo "PUT"
                  (tether-gitlab--merge-request
                   repo number (format "/notes/%s" (plist-get comment :id)))
                  nil :payload `((body . ,body)))))
    (unless (plist-get result :ok)
      (tether-gitlab--refused result "edit the comment"))
    (tether-gitlab--note-url repo number (plist-get comment :id))))

(cl-defmethod tether--username ((repo forge-gitlab-repository))
  "Return the username of the token's user on GitLab REPO's host."
  (tether-gitlab--user repo))

(cl-defmethod tether--delete-comment ((repo forge-gitlab-repository) ctx comment)
  "Delete COMMENT of CTX's merge request on GitLab REPO."
  (let ((result (tether--call
                 repo "DELETE"
                 (tether-gitlab--merge-request
                  repo (oref (tether-context-pullreq ctx) number)
                  (format "/notes/%s" (plist-get comment :id))))))
    (unless (plist-get result :ok)
      (tether-gitlab--refused result "delete the comment"))))

;;;; Reviews

(defcustom tether-gitlab-request-changes-method 'auto
  "How to request changes on GitLab.
`api': the reviewer_state of bulk_publish; `quick-action': a note
with /request_changes; `auto': the API, then the quick action when
GitLab did not record it (older instances ignore reviewer_state)."
  :type '(choice (const auto) (const api) (const quick-action))
  :group 'tether)

(defcustom tether-gitlab-comment-sets-reviewed nil
  "Whether submitting a review as a comment marks you as having reviewed.
GitHub's comment verdict does not change your reviewer status."
  :type 'boolean
  :group 'tether)

(defvar tether-gitlab--users nil
  "Username of the token's user, by API host.")

(defun tether-gitlab--user (repo)
  "Return the username of the token's user on REPO's host."
  (let ((host (oref repo apihost)))
    (or (cdr (assoc host tether-gitlab--users))
        (let ((name (alist-get 'username (tether--request repo "GET" "/user"))))
          (push (cons host name) tether-gitlab--users)
          name))))

(defun tether-gitlab--reviewer-state (repo number user)
  "Return USER's reviewer state on merge request NUMBER of REPO.
Return nil when USER is not a reviewer, \"unknown\" when GitLab gives
no state."
  (when-let* ((reviewer
               (seq-find (lambda (reviewer)
                           (equal (alist-get 'username (alist-get 'user reviewer)) user))
                         (tether--request
                          repo "GET" (tether-gitlab--merge-request repo number "/reviewers")))))
    (or (alist-get 'state reviewer) "unknown")))

(cl-defmethod tether--review-state ((repo forge-gitlab-repository) ctx done)
  "Count the user's draft notes on CTX's merge request on GitLab REPO.
Call DONE with the result."
  (tether--call-async
   repo "GET" (tether-gitlab--merge-request
               repo (oref (tether-context-pullreq ctx) number) "/draft_notes")
   (lambda (result)
     (funcall done (if (plist-get result :ok)
                       (list :ok t :review (list :drafts (length (plist-get result :value))))
                     result)))
   :unpaginate t))

(cl-defmethod tether--review-start ((_repo forge-gitlab-repository) _ctx)
  "Start a draft review on GitLab: there is nothing to create there."
  nil)

(defun tether-gitlab--publish (repo number body state)
  "Publish the drafts of merge request NUMBER of REPO, with summary BODY.
STATE is the reviewer_state to set, or nil."
  (let ((result (tether--call
                 repo "POST"
                 (tether-gitlab--merge-request repo number "/draft_notes/bulk_publish")
                 nil :payload (tether--compact
                               `((note . ,(and (not (string-empty-p body)) body))
                                 (reviewer_state . ,state))))))
    (unless (plist-get result :ok)
      (tether-gitlab--refused result "publish the review"))))

(defun tether-gitlab--note (repo number body)
  "Post the general note BODY on merge request NUMBER of REPO."
  (let ((result (tether--call repo "POST"
                              (tether-gitlab--merge-request repo number "/notes")
                              nil :payload `((body . ,body)))))
    (unless (plist-get result :ok)
      (tether-gitlab--refused result "post the note"))))

(defun tether-gitlab--request-changes (repo ctx body)
  "Publish the drafts of CTX's merge request on GitLab REPO, as a change request.
GitLab adds you as a reviewer if you are not one, except when you are
the author.  It answers success even when it did not record the
request, so the reviewer state is read back."
  (let* ((number (oref (tether-context-pullreq ctx) number))
         (user (tether-gitlab--user repo))
         (method tether-gitlab-request-changes-method))
    (unless (tether-gitlab--reviewer-state repo number user)
      (when (equal user (oref (tether-context-pullreq ctx) author))
        (user-error "Tether: GitLab does not let the author of !%d request changes"
                    number))
      (unless (y-or-n-p (format "You are not a reviewer of !%d; GitLab will add you.  \
Continue? " number))
        (user-error "Tether: review not submitted")))
    (tether-gitlab--publish repo number body
                            (and (not (eq method 'quick-action)) "requested_changes"))
    (unless (or (eq method 'api)
                (and (eq method 'auto)
                     (equal (tether-gitlab--reviewer-state repo number user)
                            "requested_changes")))
      (tether-gitlab--note repo number "/request_changes"))
    (if (equal (tether-gitlab--reviewer-state repo number user) "requested_changes")
        (format "submitted the review of !%d as a request for changes" number)
      (format "published the review of !%d, but GitLab did not record the \
request for changes" number))))

(cl-defmethod tether--review-submit ((repo forge-gitlab-repository) ctx verdict body)
  "Publish the draft notes of CTX's merge request on GitLab REPO.
VERDICT and the summary BODY decide what else happens."
  (let ((number (oref (tether-context-pullreq ctx) number)))
    (pcase verdict
      ('request-changes (tether-gitlab--request-changes repo ctx body))
      ('approve
       (tether-gitlab--publish repo number body nil)
       (tether--approve repo ctx)
       (format "submitted the review of !%d as an approval" number))
      (_
       (tether-gitlab--publish repo number body
                               (and tether-gitlab-comment-sets-reviewed "reviewed"))
       (format "submitted the review of !%d" number)))))

(cl-defmethod tether--review-discard ((repo forge-gitlab-repository) ctx)
  "Delete the user's draft notes on CTX's merge request on GitLab REPO."
  (let ((number (oref (tether-context-pullreq ctx) number)))
    (dolist (draft (tether--request repo "GET"
                                    (tether-gitlab--merge-request repo number "/draft_notes")))
      (let ((result (tether--call repo "DELETE"
                                  (tether-gitlab--merge-request
                                   repo number
                                   (format "/draft_notes/%s" (alist-get 'id draft))))))
        (unless (plist-get result :ok)
          (tether-gitlab--refused result "delete a draft"))))))

(cl-defmethod tether--approve ((repo forge-gitlab-repository) ctx)
  "Approve CTX's merge request on GitLab REPO, at the head reviewed."
  (let ((result (tether--call
                 repo "POST"
                 (tether-gitlab--merge-request
                  repo (oref (tether-context-pullreq ctx) number) "/approve")
                 nil :payload `((sha . ,(tether-context-head-sha ctx))))))
    (unless (plist-get result :ok)
      (tether-gitlab--refused result "approve"))))

(cl-defmethod tether--pullreq-ref ((_repo forge-gitlab-repository) number)
  "Return GitLab's ref for the head of merge request NUMBER."
  (format "refs/merge-requests/%d/head" number))

(cl-defmethod tether--pr-prefix ((_repo forge-gitlab-repository))
  "Return the prefix of GitLab merge request numbers."
  "!")

(provide 'tether-gitlab)
;;; tether-gitlab.el ends here
