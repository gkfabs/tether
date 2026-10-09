;;; tether-comment.el --- Comment on pull requests from file buffers  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `tether-nitpick' comments on the current line or region of a file
;; buffer, `tether-nitpick-file' on the whole file (SPEC.md §6.1).
;;
;; Lines are checked before anything is typed: they must be in the pull
;; request diff, in one hunk, and unchanged locally since the PR head.
;; For now the comment is read from the minibuffer; the comment editor
;; comes with step 6.  When posting fails, the text is put in the kill
;; ring.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'tether-context)
(require 'tether-lines)

(defun tether--buffer-path (ctx)
  "Return the path of the current buffer's file in CTX's worktree."
  (let ((default-directory (tether-context-toplevel ctx)))
    (or (magit-file-relative-name)
        (user-error "Tether: this buffer does not visit a file of %s"
                    (abbreviate-file-name (tether-context-toplevel ctx))))))

(defun tether--buffer-lines ()
  "Return (BEG . END), the lines of the active region or the current line.
A region ending at the start of a line does not include that line."
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (when (and (> end beg)
                   (save-excursion (goto-char end) (bolp)))
          (setq end (1- end)))
        (cons (line-number-at-pos beg t) (line-number-at-pos end t)))
    (let ((line (line-number-at-pos nil t)))
      (cons line line))))

(defun tether--line-target (ctx path lines)
  "Return the comment target for buffer LINES (BEG . END) of PATH in CTX.
Signal a `user-error' saying why when they cannot take a comment."
  (let ((region (tether-region-at ctx path (car lines) (cdr lines)))
        (map (tether-file-map ctx path)))
    (list :path path
          :old-path (tether-file-map-old-path map)
          :start (car region)
          :end (cdr region)
          :side 'right)))

(defun tether--file-target (ctx path)
  "Return the target of a comment on the whole file PATH in CTX."
  (let ((map (tether-file-map ctx path)))
    (unless map
      (user-error "Tether: %s is not changed in %s" path
                  (tether--label (tether-context-repo ctx)
                                 (oref (tether-context-pullreq ctx) number))))
    (list :path path :old-path (tether-file-map-old-path map) :side 'right)))

(defun tether--read-body (target)
  "Read the text of a comment on TARGET."
  (let ((body (string-trim (read-string (format "Comment on %s: "
                                                (tether--target-label target))))))
    (when (string-empty-p body)
      (user-error "Tether: empty comment, nothing posted"))
    body))

(defun tether--current-again (ctx)
  "Return CTX, or a fresh context of its pull request if CTX is stale.
The text may take a while to write; the PR head may have moved."
  (if (and (tether-context-current-p ctx) (tether--context-fresh-p ctx))
      ctx
    (tether--resolve (tether-context-repo ctx) (tether-context-pullreq ctx)
                     (tether-context-toplevel ctx)
                     (tether-context-checked-out ctx))))

(defun tether--post (ctx target body)
  "Post BODY on TARGET of CTX and report it; keep BODY if that fails."
  (condition-case err
      (let ((result (tether--post-comment (tether-context-repo ctx) ctx
                                          target body)))
        (message "Tether: %s%s" (plist-get result :message)
                 (if-let* ((url (plist-get result :url)))
                     (concat " — " url)
                   ""))
        result)
    ((user-error error)
     (kill-new body)
     (signal (car err)
             (list (concat (error-message-string err)
                           "; your comment is in the kill ring"))))))

;;;###autoload
(defun tether-nitpick (&optional pick)
  "Comment on the current line or the active region in its pull request.
The lines must be in the pull request diff, in one hunk, and
unchanged locally since the PR head.  With prefix argument PICK,
choose the pull request."
  (interactive "P")
  (let* ((ctx (tether-context t pick))
         (path (tether--buffer-path ctx))
         (lines (tether--buffer-lines))
         (target (tether--line-target ctx path lines))
         (body (tether--read-body target))
         (ctx (tether--current-again ctx))
         (target (tether--line-target ctx path lines)))
    (deactivate-mark)
    (tether--post ctx target body)))

;;;###autoload
(defun tether-nitpick-file (&optional pick)
  "Comment on the whole file of the current buffer in its pull request.
With prefix argument PICK, choose the pull request."
  (interactive "P")
  (let* ((ctx (tether-context t pick))
         (path (tether--buffer-path ctx))
         (target (tether--file-target ctx path))
         (body (tether--read-body target)))
    (tether--post (tether--current-again ctx) target body)))

(provide 'tether-comment)
;;; tether-comment.el ends here
