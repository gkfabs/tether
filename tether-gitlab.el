;;; tether-gitlab.el --- GitLab backend  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
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
A comment on the whole file is a general note naming the file."
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
                          `((body . ,body)
                            (position . ,(tether-gitlab--position ctx target)))))))
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
                                 (list :author .author.username
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

(cl-defmethod tether--pullreq-ref ((_repo forge-gitlab-repository) number)
  "Return GitLab's ref for the head of merge request NUMBER."
  (format "refs/merge-requests/%d/head" number))

(cl-defmethod tether--pr-prefix ((_repo forge-gitlab-repository))
  "Return the prefix of GitLab merge request numbers."
  "!")

(provide 'tether-gitlab)
;;; tether-gitlab.el ends here
