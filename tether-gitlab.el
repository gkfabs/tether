;;; tether-gitlab.el --- GitLab backend  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; GitLab implementations of the per-forge operations (SPEC.md §5.4).

;;; Code:

(require 'tether-client)

(defun tether-gitlab--merge-request (repo number)
  "Return the REST path of merge request NUMBER of REPO."
  (format "/projects/%s/merge_requests/%d" (tether--project-id repo) number))

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

(cl-defmethod tether--pullreq-ref ((_repo forge-gitlab-repository) number)
  "Return GitLab's ref for the head of merge request NUMBER."
  (format "refs/merge-requests/%d/head" number))

(cl-defmethod tether--pr-prefix ((_repo forge-gitlab-repository))
  "Return the prefix of GitLab merge request numbers."
  "!")

(provide 'tether-gitlab)
;;; tether-gitlab.el ends here
