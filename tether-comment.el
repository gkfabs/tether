;;; tether-comment.el --- Comment on pull requests from file buffers  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `tether-nitpick' comments on the current line or region of a file
;; buffer or of the PR diff, `tether-suggest' suggests a change of
;; them, `tether-nitpick-file' comments on the whole file.
;;
;; Lines are checked before anything is typed: they must be in the pull
;; request diff, in one hunk, and unchanged locally since the PR head.
;; The comment is written in `tether-comment-mode', a Markdown editor
;; showing the hunk for reference.  Text that was not sent (cancelled,
;; or the buffer killed) comes back with M-p in the next comment.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit)
(require 'tether-context)
(require 'tether-lines)
(require 'tether-nav)
(require 'tether-threads)
(require 'tether-review)
(require 'diff-mode)
(require 'markdown-mode)

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

(defun tether--diff-target (ctx)
  "Return the comment target of the line or region at point in a diff.
The diff must be CTX's PR diff.  Removed lines are commented on the
base side; a region must stay on one side of one hunk."
  (unless (tether--diff-range-of-pr-p ctx)
    (user-error "Tether: this is not the diff of %s; open it with `tether-diff'"
                (tether--label (tether-context-repo ctx)
                               (oref (tether-context-pullreq ctx) number))))
  (let* ((region (use-region-p))
         (end-pos (if region
                      (let ((end (region-end)))
                        (if (and (> end (region-beginning))
                                 (save-excursion (goto-char end) (bolp)))
                            (1- end)
                          end))
                    (point)))
         (beg (tether--diff-line-at (if region (region-beginning) (point))))
         (end (tether--diff-line-at end-pos)))
    (unless (and beg end)
      (user-error "Tether: point is not on a line of a hunk"))
    (unless (eq (plist-get beg :hunk) (plist-get end :hunk))
      (user-error "Tether: the region spans two hunks"))
    (let ((side (if (eq (plist-get end :kind) :removed) 'left 'right)))
      (unless (eq (eq (plist-get beg :kind) :removed) (eq side 'left))
        (user-error "Tether: the region mixes removed and kept lines; \
select lines of one side"))
      (let* ((path (plist-get end :path))
             (map (tether-file-map ctx path))
             (table (and map (if (eq side 'left) (tether-file-map-by-old map)
                               (tether-file-map-by-new map))))
             (key (if (eq side 'left) :old :new))
             (start (and table (gethash (plist-get beg key) table)))
             (last (and table (gethash (plist-get end key) table))))
        (unless (and start last)
          (user-error "Tether: %s line %d is not in the PR diff" path
                      (plist-get end key)))
        (list :path path :old-path (tether-file-map-old-path map)
              :start start :end last :side side)))))

(defun tether--file-target (ctx path)
  "Return the target of a comment on the whole file PATH in CTX."
  (let ((map (tether-file-map ctx path)))
    (unless map
      (user-error "Tether: %s is not changed in %s" path
                  (tether--label (tether-context-repo ctx)
                                 (oref (tether-context-pullreq ctx) number))))
    (list :path path :old-path (tether-file-map-old-path map) :side 'right)))

(defun tether--current-again (ctx)
  "Return CTX, or a fresh context of its pull request if CTX is stale.
The text may take a while to write; the PR head may have moved."
  (if (or (tether-context-commit ctx)
          (and (tether-context-current-p ctx) (tether--context-fresh-p ctx)))
      ctx
    (tether--resolve (tether-context-repo ctx) (tether-context-pullreq ctx)
                     (tether-context-toplevel ctx)
                     (tether-context-checked-out ctx))))

;;;; The comment editor

(defcustom tether-display-buffer-action
  '((display-buffer-below-selected) (window-height . 0.3))
  "How to display the comment editor; see `display-buffer'."
  :type '(cons (repeat function) alist)
  :group 'tether)

(defvar tether--comment-history nil
  "Comments written but not sent, most recent first.")

(defconst tether--comment-history-size 10
  "How many unsent comments `tether--comment-history' keeps.")

(defvar-local tether-comment--ctx nil
  "Review context of the comment being written.")

(defvar-local tether-comment--target nil
  "Target of the comment being written.")

(defvar-local tether-comment--retarget nil
  "Function computing the target again for a newer context.")

(defvar-local tether-comment--origin nil
  "Window the comment was started from.")

(defvar-local tether-comment--sent nil
  "Non-nil once the comment was posted.")

(defvar-local tether-comment--history-index -1
  "Position in `tether--comment-history' while going through it.")

(defvar-keymap tether-comment-mode-map
  "C-c C-c" #'tether-comment-send
  "C-c C-k" #'tether-comment-cancel
  "C-c @" #'tether-comment-mention
  "C-c C-s" #'tether-comment-toggle-suggestion
  "M-p" #'tether-comment-previous
  "M-n" #'tether-comment-next)

(define-derived-mode tether-comment-mode gfm-mode "Tether-Comment"
  "Major mode to write a pull request comment.
\\<tether-comment-mode-map>Send it with \\[tether-comment-send], \
cancel with \\[tether-comment-cancel].  The hunk shown below the text
is for reference and is not sent.

\\{tether-comment-mode-map}"
  (add-hook 'kill-buffer-hook #'tether-comment--save-unsent nil t))

(defun tether-comment--reference-start ()
  "Return the position of the reference part of the editor."
  (or (text-property-any (point-min) (point-max) 'tether-reference t)
      (point-max)))

(defun tether-comment--body ()
  "Return the text to send, without the reference part."
  (string-trim (buffer-substring-no-properties
                (point-min) (tether-comment--reference-start))))

(defun tether-comment--set-body (text)
  "Replace the text to send by TEXT."
  (let ((inhibit-read-only t))
    (delete-region (point-min) (tether-comment--reference-start))
    (goto-char (point-min))
    (insert text "\n")
    (goto-char (point-min))))

(defun tether-comment--save-unsent ()
  "Keep the text in `tether--comment-history' unless it was sent."
  (let ((body (tether-comment--body)))
    (unless (or tether-comment--sent (string-empty-p body))
      (setq tether--comment-history
            (seq-take (cons body (delete body tether--comment-history))
                      tether--comment-history-size))
      (setq tether-comment--sent t))))

(defun tether-comment--header (&optional problem)
  "Return the header line of the editor, with PROBLEM when posting failed."
  (let ((ctx tether-comment--ctx))
    (concat
     (if problem
         (propertize (concat problem "  ") 'face 'error)
       "")
     (substitute-command-keys
      (format "%s on %s, head %s  \\<tether-comment-mode-map>\
\\[tether-comment-send] send  \\[tether-comment-cancel] cancel"
              (tether--target-label tether-comment--target)
              (tether--label (tether-context-repo ctx)
                             (oref (tether-context-pullreq ctx) number))
              (substring (tether-context-head-sha ctx) 0 7))))))

(defun tether-comment--insert-reference (ctx target)
  "Insert the hunk of TARGET in CTX's diff, read-only, below the text.
For a reply, insert the discussion instead."
  (cond
   ((plist-get target :review)
    (save-excursion
      (goto-char (point-max))
      (let ((start (point)))
        (insert (format "\n-- Not sent: the summary of the review; it publishes \
your %s --\n" (tether--drafts-note ctx)))
        (add-text-properties start (point) '(tether-reference t read-only t)))))
   ((plist-get target :thread)
    (let ((thread (plist-get target :thread)))
      (save-excursion
        (goto-char (point-max))
        (let ((start (point)))
          (insert "\n-- Not sent: the discussion, for reference --\n")
          (tether-threads--insert-comments thread)
          (add-text-properties start (point) '(tether-reference t read-only t))))))
   (t (tether-comment--insert-hunk ctx target))))

(defun tether-comment--insert-hunk (ctx target)
  "Insert the hunk of TARGET in CTX's diff, read-only, below the text."
  (when-let* ((end (plist-get target :end))
              (hunk (tether-hunk-text ctx (plist-get target :path)
                                      (tether-line-hunk end))))
    (save-excursion
      (goto-char (point-max))
      (let ((start (point)))
        (insert "\n-- Not sent: the hunk, for reference --\n")
        (dolist (line (split-string (string-trim-right hunk) "\n"))
          (insert (propertize line 'font-lock-face
                              (pcase (string-to-char line)
                                (?+ 'diff-added) (?- 'diff-removed)
                                (?@ 'diff-hunk-header) (_ 'diff-context)))
                  "\n"))
        ;; Not front-sticky: text can be typed above, not inside or
        ;; below.
        (add-text-properties start (point) '(tether-reference t read-only t))))))

(defun tether--buffer-name (ctx target)
  "Return the name of the editor of a comment on TARGET of CTX."
  (let ((repo (tether-context-repo ctx)))
    (format "*tether-comment %s/%s%s %s*" (oref repo owner) (oref repo name)
            (tether--label repo (oref (tether-context-pullreq ctx) number))
            (tether--target-label target))))

(defun tether--edit-comment (ctx target retarget &optional initial)
  "Open the editor of a comment on TARGET of CTX, with INITIAL text.
RETARGET computes the target again for a newer context.  Writing the
same target again shows the editor already open."
  (let* ((name (tether--buffer-name ctx target))
         (existing (get-buffer name))
         (origin (selected-window))
         (buffer (or existing (generate-new-buffer name))))
    (unless existing
      (with-current-buffer buffer
        (tether-comment-mode)
        (setq tether-comment--ctx ctx
              tether-comment--target target
              tether-comment--retarget retarget
              tether-comment--origin origin)
        (setq header-line-format '(:eval (tether-comment--header)))
        (tether-comment--insert-reference ctx target)
        (when initial (tether-comment--set-body initial))
        (goto-char (point-min))))
    (pop-to-buffer buffer tether-display-buffer-action)
    buffer))

(defun tether-comment--close ()
  "Close the editor and go back to where the comment was started."
  (let ((origin tether-comment--origin)
        (buffer (current-buffer)))
    (quit-window nil (get-buffer-window buffer))
    (kill-buffer buffer)
    (when (window-live-p origin)
      (select-window origin))))

(defun tether-comment-send ()
  "Post the comment, then close the editor.
When posting fails the editor stays open, saying why."
  (interactive)
  (let ((body (tether-comment--body)))
    (when (and (string-empty-p body) (not (plist-get tether-comment--target :review)))
      (user-error "Tether: empty comment, nothing posted"))
    (condition-case err
        (let* ((ctx (tether--current-again tether-comment--ctx))
               (target (if (eq ctx tether-comment--ctx)
                           tether-comment--target
                         (funcall tether-comment--retarget ctx)))
               (thread (plist-get target :thread))
               (result (cond
                        ((plist-get target :review)
                         (tether--submit-review ctx (plist-get target :review) body))
                        (thread
                         (tether--reply (tether-context-repo ctx) ctx thread body))
                        (t (tether--post-comment (tether-context-repo ctx) ctx
                                                 target body)))))
          (setq tether-comment--sent t)
          (tether-comment--close)
          (tether-fetch-threads ctx)
          (tether-fetch-review ctx)
          (message "Tether: %s%s" (plist-get result :message)
                   (if-let* ((url (plist-get result :url)))
                       (concat " — " url)
                     ""))
          result)
      (error
       (setq header-line-format (tether-comment--header
                                 (error-message-string err)))
       (signal (car err) (cdr err))))))

(defun tether-comment-cancel ()
  "Close the editor without posting.
The text stays available with \\<tether-comment-mode-map>\\[tether-comment-previous] \
in the next comment."
  (interactive)
  (when (or (string-empty-p (tether-comment--body))
            (y-or-n-p "Discard this comment? "))
    (tether-comment--save-unsent)
    (tether-comment--close)))

(defun tether-comment--cycle (step)
  "Replace the text by the unsent comment STEP away in the history."
  (let ((index (+ tether-comment--history-index step)))
    (cond ((< index 0)
           (setq tether-comment--history-index -1)
           (tether-comment--set-body ""))
          ((>= index (length tether--comment-history))
           (user-error "Tether: no older unsent comment"))
          (t
           (setq tether-comment--history-index index)
           (tether-comment--set-body (nth index tether--comment-history))))))

(defun tether-comment-previous ()
  "Replace the text by the previous unsent comment."
  (interactive)
  (tether-comment--cycle 1))

(defun tether-comment-next ()
  "Replace the text by the next unsent comment."
  (interactive)
  (tether-comment--cycle -1))

(defun tether-comment--people (ctx)
  "Return the logins that can be mentioned in CTX's pull request."
  (delete-dups
   (delq nil (cons (oref (tether-context-pullreq ctx) author)
                   (ignore-errors
                     (mapcar #'cadr (oref (tether-context-repo ctx) assignees)))))))

(defun tether-comment-mention (login)
  "Insert a mention of LOGIN, read with completion."
  (interactive (list (completing-read "Mention: "
                                      (tether-comment--people tether-comment--ctx))))
  (insert "@" login " "))

;;;; Suggestions

(defun tether--suggestion (ctx target)
  "Return a suggestion block replacing TARGET's lines of CTX's PR head.
GitLab's plain block replaces only the line the comment is on, its
last line, so a range says how many lines above it to replace."
  (unless (and (plist-get target :end) (eq (plist-get target :side) 'right))
    (user-error "Tether: a suggestion replaces lines of the PR head"))
  (let* ((beg (tether-line-new (plist-get target :start)))
         (end (tether-line-new (plist-get target :end)))
         (above (- end beg)))
    (format "```suggestion%s\n%s```\n"
            (if (and (cl-typep (tether-context-repo ctx) 'forge-gitlab-repository)
                     (> above 0))
                (format ":-%d+0" above)
              "")
            (tether-head-lines ctx (plist-get target :path) beg end))))

(defun tether-comment-toggle-suggestion ()
  "Add a suggestion block of the target lines, or remove the one there is."
  (interactive)
  (let ((body (buffer-substring-no-properties
               (point-min) (tether-comment--reference-start))))
    (if (string-match "```suggestion[^\n]*\n\\(?:.*\n\\)*?```\n?" body)
        (tether-comment--set-body (string-trim (replace-match "" t t body)))
      (save-excursion
        (goto-char (point-min))
        (insert (tether--suggestion tether-comment--ctx tether-comment--target))))))

;;;; Commands

(defun tether--start-comment (pick &optional suggest)
  "Check the target of a comment at point, then open its editor.
PICK reads the pull request; SUGGEST starts with a suggestion block."
  (let ((ctx (tether-context t pick)))
    (pcase-let
        ((`(,target . ,retarget)
          (if (derived-mode-p 'magit-diff-mode)
              (let ((target (tether--diff-target ctx))
                    (head (tether-context-head-sha ctx)))
                (cons target
                      (lambda (newer)
                        (if (equal (tether-context-head-sha newer) head)
                            target
                          (user-error "Tether: the PR head moved; \
reopen this comment from the new diff")))))
            (let* ((source (current-buffer))
                   (path (tether--buffer-path ctx))
                   (lines (tether--buffer-lines)))
              (cons (tether--line-target ctx path lines)
                    (lambda (newer)
                      (unless (buffer-live-p source)
                        (user-error "Tether: the PR head moved and the file \
buffer is gone; reopen this comment"))
                      (with-current-buffer source
                        (tether--line-target newer path lines))))))))
      (deactivate-mark)
      (tether--edit-comment ctx target retarget
                            (and suggest (tether--suggestion ctx target))))))

;;;###autoload
(defun tether-nitpick (&optional pick)
  "Comment on the current line or the active region in its pull request.
The lines must be in the pull request diff, in one hunk, and
unchanged locally since the PR head.  In the PR diff (`tether-diff'),
removed lines can be commented too.  The comment is written in an
editor.  With prefix argument PICK, choose the pull request."
  (interactive "P")
  (tether--start-comment pick))

;;;###autoload
(defun tether-suggest (&optional pick)
  "Suggest a change of the current line or region in its pull request.
Like `tether-nitpick', with the editor starting with a suggestion
block of the lines, to edit into the proposed text.  With prefix
argument PICK, choose the pull request."
  (interactive "P")
  (tether--start-comment pick t))

;;;###autoload
(defun tether-reply ()
  "Reply in the review thread at point.
On a line with a thread, in the thread list or in `tether-show-thread'.
The reply is written in the comment editor, the discussion below."
  (interactive)
  (let* ((ctx (tether--threads-ctx))
         (target (list :thread (tether--thread-at-point-or-error ctx))))
    (tether--edit-comment ctx target (lambda (_) target))))

;;;###autoload
(defun tether-nitpick-file (&optional pick)
  "Comment on the whole file of the current buffer in its pull request.
With prefix argument PICK, choose the pull request."
  (interactive "P")
  (let* ((ctx (tether-context t pick))
         (path (tether--buffer-path ctx)))
    (tether--edit-comment ctx (tether--file-target ctx path)
                          (lambda (newer) (tether--file-target newer path)))))

(provide 'tether-comment)
;;; tether-comment.el ends here
