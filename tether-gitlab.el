;;; tether-gitlab.el --- GitLab backend  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; GitLab implementations of the per-forge operations (SPEC.md §5.4).

;;; Code:

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
      ;; commits (SPEC.md §3.2 step 6).
      (unless (and .diff_refs.base_sha .diff_refs.head_sha)
        (user-error "Tether: GitLab has not computed the diff of !%s yet; \
retry in a moment" number))
      (list :head .diff_refs.head_sha
            :base .diff_refs.base_sha
            :start .diff_refs.start_sha
            :base-tip nil
            :base-ref .target_branch))))

;;;; Comments (SPEC.md §5.4)

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

(cl-defmethod tether--pullreq-ref ((_repo forge-gitlab-repository) number)
  "Return GitLab's ref for the head of merge request NUMBER."
  (format "refs/merge-requests/%d/head" number))

(cl-defmethod tether--pr-prefix ((_repo forge-gitlab-repository))
  "Return the prefix of GitLab merge request numbers."
  "!")

(provide 'tether-gitlab)
;;; tether-gitlab.el ends here
