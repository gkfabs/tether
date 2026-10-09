;;; tether-github.el --- GitHub backend  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; GitHub implementations of the per-forge operations (SPEC.md §5.3).

;;; Code:

(require 'tether-client)

(defun tether-github--pulls (repo number)
  "Return the REST path of pull request NUMBER of REPO."
  (format "/repos/%s/%s/pulls/%d" (oref repo owner) (oref repo name) number))

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

(cl-defmethod tether--pullreq-ref ((_repo forge-github-repository) number)
  "Return GitHub's ref for the head of pull request NUMBER."
  (format "refs/pull/%d/head" number))

(cl-defmethod tether--pr-prefix ((_repo forge-github-repository))
  "Return the prefix of GitHub pull request numbers."
  "#")

(provide 'tether-github)
;;; tether-github.el ends here
