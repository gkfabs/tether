;;; tether-lines.el --- Map buffer lines to pull request lines  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Runs git for `tether-diff' (SPEC.md §4):
;;
;; - the line map of a file in the pull request diff, built from a git
;;   invocation the user's git configuration cannot change, and cached
;;   in the review context;
;; - the mapping between a buffer line and a PR head line, through
;;   uncommitted and unsaved edits.
;;
;; The lookup functions work on the current buffer, which visits the
;; file (or is a blob buffer of the PR head).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit)
(require 'tether-diff)
(require 'tether-context)

(defconst tether--diff-config
  '("-c" "diff.noprefix=false" "-c" "diff.mnemonicPrefix=false"
    "-c" "color.ui=false" "-c" "diff.relative=false"
    "-c" "core.quotePath=true")
  "Git configuration that would otherwise change the diff output.")

(defconst tether--diff-options
  '("--no-color" "--no-ext-diff" "--no-textconv" "--diff-algorithm=myers"
    "--src-prefix=a/" "--dst-prefix=b/")
  "Options of every git diff whose output tether parses.")

(defun tether--git-output (&rest args)
  "Return the output of git with ARGS, whatever its exit code."
  (with-temp-buffer
    (apply #'magit-git-insert args)
    (buffer-string)))

(defun tether--git-diff (&rest args)
  "Return the output of git diff with tether's fixed options and ARGS."
  (apply #'tether--git-output
         (append tether--diff-config (list "diff") tether--diff-options args)))

(defun tether--range (ctx)
  "Return the base..head range of CTX."
  (format "%s..%s" (tether-context-base-sha ctx) (tether-context-head-sha ctx)))

;;;; Line maps

(defun tether--path-map (ctx)
  "Return CTX's changed files as (NEW-PATH OLD-PATH . STATUS), cached."
  (let ((maps (tether-context-maps ctx)))
    (or (gethash :paths maps)
        (let ((default-directory (tether-context-toplevel ctx)))
          (puthash :paths
                   (tether-diff-parse-name-status
                    (tether--git-diff "--name-status" "-z" "-M" (tether--range ctx)))
                   maps)))))

(defun tether--lfs-pointer-p (ctx path)
  "Return non-nil if PATH is a Git LFS pointer at CTX's head."
  (let ((first (magit-git-string "show" (format "%s:%s" (tether-context-head-sha ctx)
                                                path))))
    (and first (string-prefix-p "version https://git-lfs." first))))

(defun tether--build-file-map (ctx path)
  "Return the line map of PATH in CTX's diff, or nil if PATH is unchanged."
  (when-let* ((entry (assoc path (tether--path-map ctx))))
    (let* ((default-directory (tether-context-toplevel ctx))
           (old-path (cadr entry))
           (maps (tether-diff-parse
                  (apply #'tether--git-diff "-U3" "-M" (tether--range ctx) "--"
                         (delete-dups (list old-path path)))))
           (map (or (seq-find (lambda (m) (equal (tether-file-map-new-path m) path))
                              maps)
                    (car maps))))
      (when (and map (tether--lfs-pointer-p ctx path))
        (setf (tether-file-map-status map) :lfs))
      map)))

(defun tether-file-map (ctx path)
  "Return the line map of PATH in CTX's pull request diff, cached.
PATH is relative to the worktree.  Return nil if the pull request
does not change PATH."
  (let ((maps (tether-context-maps ctx)))
    (pcase (gethash path maps 'none)
      ('none (puthash path (tether--build-file-map ctx path) maps))
      (map map))))

;;;; Buffer lines and head lines

(defun tether--buffer-drift (file)
  "Return the drift from FILE on disk to the current buffer's text."
  (let ((temp (make-temp-file "tether-buffer-")))
    (unwind-protect
        (progn
          (save-restriction
            (widen)
            (let ((coding-system-for-write buffer-file-coding-system))
              (write-region nil nil temp nil 'silent)))
          (tether-diff-parse-drift
           (tether--git-diff "--no-index" "-U0" "--" file temp)))
      (delete-file temp))))

(defun tether--drifts (ctx path)
  "Return the drifts from CTX's head to the buffer visiting PATH.
The head-to-worktree drift comes first, then the worktree-to-buffer
one for unsaved changes.  A tether blob buffer shows the head itself
and has none."
  (unless tether--buffer-pullreq
    (let ((default-directory (tether-context-toplevel ctx)))
      (delq nil
            (list (tether-diff-parse-drift
                   (tether--git-diff "-U0" (tether-context-head-sha ctx) "--" path))
                  (and (buffer-modified-p) buffer-file-name
                       (tether--buffer-drift buffer-file-name)))))))

(defun tether--to-head (drifts line)
  "Map buffer LINE back through DRIFTS to the head, or nil if it changed."
  (cl-loop for drift in (reverse drifts)
           while line
           do (setq line (tether-diff-drift-from drift line))
           finally return line))

(defun tether--from-head (drifts line)
  "Map head LINE forward through DRIFTS to the buffer, or nil if it changed."
  (cl-loop for drift in drifts
           while line
           do (setq line (tether-diff-drift-to drift line))
           finally return line))

(defun tether-head-line (ctx path line)
  "Return the PR head line of LINE of the current buffer visiting PATH.
Return nil when the line changed locally since CTX's head."
  (tether--to-head (tether--drifts ctx path) line))

(defun tether-buffer-line (ctx path line)
  "Return the line of the current buffer showing PR head LINE of PATH.
Return nil when that line changed locally since CTX's head."
  (tether--from-head (tether--drifts ctx path) line))

;;;; Lookup (SPEC.md §4.4)

(defun tether-line-at (ctx path line)
  "Return the `tether-line' of LINE of the current buffer visiting PATH.
Return nil when the line is not in CTX's diff or changed locally."
  (when-let* ((head (tether-head-line ctx path line)))
    (tether-file-map-line (tether-file-map ctx path) head)))

(defun tether-region-at (ctx path beg end)
  "Return (START . END) `tether-line's for lines BEG to END of the buffer.
The current buffer visits PATH.  Signal a `user-error' that says why
when the lines cannot take one comment on CTX's pull request."
  (let* ((drifts (tether--drifts ctx path))
         (head-beg (tether--to-head drifts beg))
         (head-end (tether--to-head drifts end))
         (map (tether-file-map ctx path)))
    (cond
     ((null head-beg)
      (user-error "Tether: %s line %d changed locally since the PR head"
                  path beg))
     ((null head-end)
      (user-error "Tether: %s line %d changed locally since the PR head"
                  path end))
     ((/= (- head-end head-beg) (- end beg))
      (user-error "Tether: %s lines %d-%d changed locally since the PR head"
                  path beg end))
     (t
      (when-let* ((problem (tether-file-map-region-problem map head-beg head-end)))
        (user-error "Tether: %s: %s" path problem))
      (tether-file-map-region map head-beg head-end)))))

(defun tether-commentable-lines (ctx path)
  "Return the lines of the current buffer that can take a comment.
The value is a list of (BUFFER-LINE . KIND), KIND being :added or
:context, for the buffer visiting PATH in CTX's pull request."
  (let ((map (tether-file-map ctx path))
        (drifts (tether--drifts ctx path)))
    (delq nil
          (mapcar (lambda (entry)
                    (when-let* ((line (tether--from-head drifts (car entry))))
                      (cons line (cdr entry))))
                  (tether-file-map-commentable map)))))

(provide 'tether-lines)
;;; tether-lines.el ends here
