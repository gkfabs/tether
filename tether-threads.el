;;; tether-threads.el --- Review threads in files and in a list  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; Assisted-by: Claude:claude-opus-5-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The review threads of a pull request, fetched asynchronously once
;; its context is resolved and after each comment:
;;
;; - in file buffers, a fringe mark and the start of the thread at the
;;   end of its line;
;; - `tether-next-thread' and `tether-previous-thread' go from thread to
;;   thread, across files;
;; - `tether-threads' lists them, by file, with the outdated ones and
;;   the hunk they were written on.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit)
(require 'magit-section)
(require 'diff-mode)
(require 'tether-context)
(require 'tether-lines)
(require 'tether-nav)

(defcustom tether-show-resolved t
  "Whether to show resolved threads in file buffers, dimmed.
`tether-toggle-resolved' switches it."
  :type 'boolean
  :group 'tether)

(defcustom tether-thread-summary t
  "Whether to show the start of a thread at the end of its line."
  :type 'boolean
  :group 'tether)

(defcustom tether-diff-thread-comments t
  "Whether magit commit buffers show review threads in full.
Under their line.  When nil, they show as in files: the start of the
thread at the end of its line.  Resolved threads always show that way."
  :type 'boolean
  :group 'tether)

(defcustom tether-cycle-across-files t
  "Whether `tether-next-thread' goes on to the next file's threads."
  :type 'boolean
  :group 'tether)

(defface tether-thread
  '((t :inherit warning))
  "Fringe mark of a line with an open review thread."
  :group 'tether)

(defface tether-thread-resolved
  '((t :inherit shadow))
  "Fringe mark of a line with a resolved review thread."
  :group 'tether)

(defface tether-thread-summary
  '((t :inherit shadow :slant italic))
  "The start of a review thread, at the end of its line."
  :group 'tether)

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'tether-thread
    [#b01111110 #b11111111 #b11111111 #b11111111 #b01111110 #b00110000
                #b00100000]
    nil 8 'center))

(defvar tether-threads-functions nil
  "Functions called with a context whose threads were just fetched.")

;;;; Fetching

(defvar tether--logged nil
  "Messages already written to *tether-log*.")

(defun tether--log (format-string &rest args)
  "Write FORMAT-STRING with ARGS to the *tether-log* buffer, once."
  (let ((text (apply #'format format-string args)))
    (unless (member text tether--logged)
      (push text tether--logged)
      (with-current-buffer (get-buffer-create "*tether-log*")
        (goto-char (point-max))
        (insert (format-time-string "%F %T  ") text "\n")))))

(defun tether--place-thread (ctx thread)
  "Put THREAD on CTX's head when the forge placed it on another commit.
GitLab gives the commit a discussion's position is on."
  ;; TODO(tether#gitlab-position): assumes GitLab moves the positions of
  ;; discussions forward on each push, so that a discussion on another
  ;; commit than the PR head is rare.  Unverified: thread marks may be
  ;; on the wrong line if this is false.  Each case is written to
  ;; *tether-log*; many entries there mean the assumption is wrong.
  (let ((sha (tether-thread-head-sha thread))
        (head (tether-context-head-sha ctx)))
    (when (and sha (tether-thread-line thread) (not (equal sha head)))
      (tether--log "GitLab discussion %s is on %s, the PR head is %s"
                   (tether-thread-id thread) (substring sha 0 7)
                   (substring head 0 7))
      (let* ((default-directory (tether-context-toplevel ctx))
             (line (and (eq (tether-thread-side thread) 'right)
                        (tether--commit-p sha)
                        (tether-diff-drift-to
                         (tether--drift sha head (tether-thread-path thread))
                         (tether-thread-line thread)))))
        (setf (tether-thread-line thread) line
              (tether-thread-outdated thread) (null line))))
    thread))

(defun tether-fetch-threads (ctx)
  "Fetch the review threads of CTX's pull request in the background.
When they arrive, if CTX is still current, run `tether-threads-functions'."
  ;; A single-commit context is never current: its pull request's
  ;; context holds the threads.  When that one is gone, its next
  ;; resolution fetches them.
  (when-let* ((ctx (if (tether-context-commit ctx)
                       (gethash (tether--context-key ctx) tether--contexts)
                     ctx)))
    (setf (tether-context-threads-state ctx) 'loading)
    (tether--fetch-threads
     (tether-context-repo ctx) ctx
     (lambda (result)
       (when (tether-context-current-p ctx)
         (if (plist-get result :ok)
             (setf (tether-context-threads ctx)
                   (mapcar (lambda (thread) (tether--place-thread ctx thread))
                           (plist-get result :threads))
                   (tether-context-threads-state ctx) 'done)
           (setf (tether-context-threads-state ctx) 'error)
           (message "Tether: cannot fetch the review threads: %s"
                    (plist-get result :message)))
         (run-hook-with-args 'tether-threads-functions ctx))))))

(defun tether--open-threads (ctx)
  "Return the threads of CTX that are neither resolved nor outdated."
  (seq-remove (lambda (thread) (or (tether-thread-resolved thread)
                                   (tether-thread-outdated thread)))
              (tether-context-threads ctx)))

(defun tether--placed-threads (ctx &optional path)
  "Return CTX's threads on a head line, of PATH if given, sorted.
Resolved threads count only when `tether-show-resolved' is non-nil."
  (sort (seq-filter (lambda (thread)
                      (and (tether-thread-line thread)
                           (not (tether-thread-outdated thread))
                           (eq (tether-thread-side thread) 'right)
                           (or tether-show-resolved
                               (not (tether-thread-resolved thread)))
                           (or (null path)
                               (equal (tether-thread-path thread) path))))
                    (tether-context-threads ctx))
        (lambda (a b)
          (or (string< (tether-thread-path a) (tether-thread-path b))
              (and (equal (tether-thread-path a) (tether-thread-path b))
                   (< (tether-thread-line a) (tether-thread-line b)))))))

;;;; In file buffers

(defvar-local tether--thread-overlays nil
  "Overlays showing the review threads of the current buffer.")

(defun tether--comment-lines (comment)
  "Return the lines of COMMENT's body, trimmed, whatever its line ends."
  (split-string (string-trim (or (plist-get comment :body) "")) "\r?\n"))

(defun tether--thread-first-line (thread)
  "Return the author and first line of THREAD's first comment."
  (let ((first (car (tether-thread-comments thread))))
    (format "%s: %s" (plist-get first :author)
            (car (tether--comment-lines first)))))

(defun tether--thread-summary (thread)
  "Return the text shown at the end of THREAD's line."
  (let ((replies (1- (length (tether-thread-comments thread)))))
    (concat "  ◂ " (truncate-string-to-width (tether--thread-first-line thread)
                                             60 nil nil "…")
            (if (> replies 0) (format " (+%d)" replies) "")
            (if (tether-thread-resolved thread) " [resolved]" ""))))

(defun tether--clear-thread-overlays ()
  "Remove the review threads shown in the current buffer."
  (mapc #'delete-overlay tether--thread-overlays)
  (setq tether--thread-overlays nil))

(defun tether--make-thread-overlay (thread)
  "Return an overlay showing THREAD on the current line.
A fringe mark, the first comment on hover and, when
`tether-thread-summary' is non-nil, the start of THREAD at the end of
the line."
  (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put overlay 'tether-thread thread)
    (overlay-put overlay 'priority 10)
    (overlay-put overlay 'help-echo (tether--thread-first-line thread))
    (when (display-graphic-p)
      (overlay-put overlay 'before-string
                   (propertize " " 'display
                               `(left-fringe tether-thread
                                             ,(if (tether-thread-resolved thread)
                                                  'tether-thread-resolved
                                                'tether-thread)))))
    (when tether-thread-summary
      (overlay-put overlay 'after-string
                   (propertize (tether--thread-summary thread)
                               'face 'tether-thread-summary)))
    overlay))

(defun tether--update-thread-overlays (ctx path)
  "Show CTX's review threads on PATH in the current buffer."
  (tether--clear-thread-overlays)
  (dolist (thread (tether--placed-threads ctx path))
    (when-let* ((line (tether-buffer-line ctx path (tether-thread-line thread))))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (forward-line (1- line))
          (push (tether--make-thread-overlay thread) tether--thread-overlays))))))

;;;; In magit commit buffers

(defun tether--head-to-commit-drift (ctx head path)
  "Return the drift of PATH from the PR HEAD to CTX's commit, cached."
  (let ((key (cons :head-drift path))
        (maps (tether-context-maps ctx)))
    (pcase (gethash key maps 'none)
      ('none (puthash key
                      (let ((default-directory (tether-context-toplevel ctx)))
                        (tether--drift head (tether-context-commit ctx) path))
                      maps))
      (drift drift))))

(defun tether--commit-thread-line (ctx head thread)
  "Return (SIDE . LINE) of THREAD in the diff of CTX's commit, or nil.
HEAD is the PR head.  A thread written on the commit is where it was
written; the others follow their head line back to the commit, when
it did not change since."
  (let ((side (tether-thread-side thread)))
    (cond
     ((equal (tether-thread-original-sha thread) (tether-context-commit ctx))
      (and (tether-thread-original-line thread)
           (cons side (tether-thread-original-line thread))))
     ((and (eq side 'right) (tether-thread-line thread)
           (not (tether-thread-outdated thread)))
      (when-let* ((line (tether-diff-drift-to
                         (tether--head-to-commit-drift ctx head
                                                       (tether-thread-path thread))
                         (tether-thread-line thread))))
        (cons 'right line))))))

(defun tether--thread-comments-text (thread)
  "Return THREAD's comments as lines to show under its line."
  (let ((bar (propertize "  ▎ " 'face 'tether-thread)))
    (mapconcat
     (lambda (comment)
       (let ((lines (tether--comment-lines comment)))
         (concat "\n" bar
                 (propertize (or (plist-get comment :author) "?") 'face 'bold)
                 ": " (car lines)
                 (mapconcat (lambda (line) (concat "\n" bar "  " line))
                            (cdr lines) ""))))
     (tether-thread-comments thread) "")))

(defun tether--clear-diff-threads ()
  "Remove the review threads drawn in the current diff buffer.
Found by their property, not a variable: magit resets the buffer's
variables when it shows the buffer again."
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) 'tether-diff-thread t)))

(defun tether--draw-diff-thread (thread)
  "Draw THREAD on the diff line at point."
  (let ((overlay (tether--make-thread-overlay thread)))
    (overlay-put overlay 'tether-diff-thread t)
    (when (and tether-diff-thread-comments (not (tether-thread-resolved thread)))
      (overlay-put overlay 'after-string (tether--thread-comments-text thread)))))

(defun tether--revision-context (sha)
  "Return the cached context of a pull request with commit SHA, or nil."
  (when-let* ((toplevel (magit-toplevel)))
    (catch 'found
      (maphash (lambda (key ctx)
                 (when (and (equal (car key) toplevel)
                            (tether--context-fresh-p ctx)
                            (tether--pr-commit-p ctx sha))
                   (throw 'found ctx)))
               tether--contexts)
      nil)))

(defvar tether-mode)
(defvar global-tether-mode)
(declare-function tether-mode "tether")
(declare-function tether--turn-on "tether")

(defun tether--update-revision-threads ()
  "Show the review threads in a magit commit buffer.
Only for a commit of a pull request tether already resolved; comments
from the buffer then target that commit, as from `tether-review-commit',
and `tether-mode' is on there as in files.  Not for a merge commit:
magit shows it as a combined diff, whose lines tether cannot place.
Never makes a request."
  (when (derived-mode-p 'magit-revision-mode)
    (with-demoted-errors "Tether: %S"
      (tether--clear-diff-threads)
      (let* ((sha magit-buffer-revision-oid)
             (pr-ctx (and-let* ((sha)
                                (ctx (tether--revision-context sha))
                                ((not (magit-rev-verify (concat sha "^2")))))
                       ctx)))
        (if (not pr-ctx)
            (progn (kill-local-variable 'tether--buffer-pullreq)
                   (kill-local-variable 'tether--buffer-commit)
                   (when (bound-and-true-p tether-mode)
                     (tether-mode -1)))
          (setq-local tether--buffer-pullreq (oref (tether-context-pullreq pr-ctx) id))
          (setq-local tether--buffer-commit sha)
          (when (bound-and-true-p global-tether-mode)
            (tether--turn-on))
          (when (eq (tether-context-threads-state pr-ctx) 'done)
            (let ((ctx (tether--commit-context pr-ctx sha))
                  (head (tether-context-head-sha pr-ctx)))
              (dolist (thread (tether-context-threads pr-ctx))
                (when-let* (((or tether-show-resolved
                                 (not (tether-thread-resolved thread))))
                            (at (tether--commit-thread-line ctx head thread))
                            (pos (tether--diff-line-position
                                  (tether-thread-path thread) (car at) (cdr at))))
                  (save-excursion
                    (goto-char pos)
                    (tether--draw-diff-thread thread)))))))))))

(defun tether--refresh-revision-buffers ()
  "Show the review threads again in every magit commit buffer."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'magit-revision-mode)
        (tether--update-revision-threads)))))

;;;; Going from thread to thread

(defun tether--thread-after-p (thread path line)
  "Return non-nil if THREAD comes after head LINE of PATH."
  (let ((other (tether-thread-path thread)))
    (or (string< path other)
        (and (equal path other) (> (tether-thread-line thread) line)))))

(defun tether--go-to-diff-thread (forward)
  "In a magit commit buffer, go to the next thread drawn in it.
Or the previous one unless FORWARD.  Shows its hunk if collapsed."
  (let* ((starts (sort (delete-dups
                        (mapcar #'overlay-start
                                (seq-filter (lambda (overlay)
                                              (overlay-get overlay 'tether-diff-thread))
                                            (overlays-in (point-min) (point-max)))))
                       #'<))
         (pos (or (if forward
                      (seq-find (lambda (start) (> start (line-end-position))) starts)
                    (car (last (seq-filter (lambda (start)
                                             (< start (line-beginning-position)))
                                           starts))))
                  (user-error "Tether: no %s review thread in this commit"
                              (if forward "next" "previous")))))
    (goto-char pos)
    (when (invisible-p pos)
      (let ((hunk (magit-current-section)))
        (magit-section-show hunk)
        (when-let* ((file (oref hunk parent)))
          (magit-section-show file))))
    (when-let* ((thread (seq-some (lambda (overlay) (overlay-get overlay 'tether-thread))
                                  (overlays-at pos))))
      (message "Tether: %s" (tether--thread-first-line thread)))))

(defun tether--go-to-thread (forward)
  "Go to the next thread, or the previous one unless FORWARD.
In a magit commit buffer, only to the threads shown there."
  (if (derived-mode-p 'magit-revision-mode)
      (tether--go-to-diff-thread forward)
    (tether--go-to-pr-thread forward)))

(defun tether--go-to-pr-thread (forward)
  "Go to the next thread of the PR, or the previous one unless FORWARD."
  (let* ((ctx (tether-context t))
         (threads (or (tether--placed-threads ctx)
                      (user-error "Tether: no review thread to go to%s"
                                  (if (eq (tether-context-threads-state ctx) 'done)
                                      "" " (still fetching)"))))
         (at (tether--location ctx))
         (path (or (plist-get at :path) ""))
         (line (or (plist-get at :line) 0))
         (candidates (if forward
                         (seq-filter (lambda (th) (tether--thread-after-p th path line))
                                     threads)
                       (reverse (seq-remove
                                 (lambda (th)
                                   (or (tether--thread-after-p th path line)
                                       (and (equal (tether-thread-path th) path)
                                            (= (tether-thread-line th) line))))
                                 threads))))
         (candidates (if tether-cycle-across-files
                         candidates
                       (seq-filter (lambda (th) (equal (tether-thread-path th) path))
                                   candidates)))
         (thread (or (car candidates)
                     (user-error "Tether: no %s review thread"
                                 (if forward "next" "previous")))))
    (tether--visit-path ctx (tether-thread-path thread) (tether-thread-line thread))
    (message "Tether: %s" (tether--thread-first-line thread))))

;;;###autoload
(defun tether-next-thread ()
  "Go to the next review thread of the pull request.
In a magit commit buffer, to the next one shown in it."
  (interactive)
  (tether--go-to-thread t))

;;;###autoload
(defun tether-previous-thread ()
  "Go to the previous review thread of the pull request.
In a magit commit buffer, to the previous one shown in it."
  (interactive)
  (tether--go-to-thread nil))

;;;; The thread list

(defvar-local tether-threads--ctx nil
  "Review context of the thread list.")

(defvar-local tether-threads--positions nil
  "Where each thread is in the list, as (ID . POSITION).")

(defvar-keymap tether-threads-mode-map
  :parent magit-section-mode-map
  "RET" #'tether-threads-visit
  "d" #'tether-threads-diff
  "b" #'tether-threads-browse
  "SPC" #'tether-show-thread
  "r" #'tether-reply
  "R" #'tether-resolve
  "k" #'tether-delete-comment
  "t" #'tether-topic
  "m" #'tether-status
  "g" #'tether-threads-refresh)

(define-derived-mode tether-threads-mode magit-section-mode "Tether-Threads"
  "The review threads of a pull request, by file.
On a thread, `tether-threads-visit' visits its line,
`tether-threads-diff' shows it in the PR diff and
`tether-threads-browse' in the browser; `tether-threads-refresh'
fetches the threads again.

\\{tether-threads-mode-map}")

(defun tether-threads--insert-comments (thread)
  "Insert the comments of THREAD, indented."
  (dolist (comment (tether-thread-comments thread))
    (insert (propertize (format "    %s, %s\n" (plist-get comment :author)
                                (or (plist-get comment :created) ""))
                        'font-lock-face 'bold))
    (dolist (line (tether--comment-lines comment))
      (insert "      " line "\n")))
  (when-let* ((hunk (and (tether-thread-outdated thread)
                         (tether-thread-diff-hunk thread))))
    (insert (propertize "    The hunk it was written on:\n"
                        'font-lock-face 'bold))
    (dolist (line (split-string hunk "\n"))
      (insert "      "
              (propertize line 'font-lock-face
                          (pcase (string-to-char line)
                            (?+ 'diff-added) (?- 'diff-removed)
                            (_ 'diff-context)))
              "\n"))))

(defun tether-threads--insert-thread (thread)
  "Insert THREAD as a collapsed section."
  (push (cons (tether-thread-id thread) (point)) tether-threads--positions)
  (magit-insert-section (tether-thread thread t)
    (magit-insert-heading
      (format "  %-6s %s%s%s"
              (if (tether-thread-line thread)
                  (format "%sL%d" (if (eq (tether-thread-side thread) 'left) "-" "")
                          (tether-thread-line thread))
                (format "@%s L%s" (substring (or (tether-thread-original-sha thread)
                                                 "unknown")
                                             0 7)
                        (or (tether-thread-original-line thread) "?")))
              (truncate-string-to-width (tether--thread-first-line thread) 70 nil nil "…")
              (let ((replies (1- (length (tether-thread-comments thread)))))
                (if (> replies 0) (format "  +%d" replies) ""))
              (if (tether-thread-resolved thread)
                  (propertize "  resolved" 'font-lock-face 'shadow)
                "")))
    (tether-threads--insert-comments thread)))

(defun tether-threads--render ()
  "Insert the thread list of `tether-threads--ctx'."
  (let* ((ctx tether-threads--ctx)
         (threads (tether-context-threads ctx))
         (state (tether-context-threads-state ctx))
         (outdated (seq-filter #'tether-thread-outdated threads))
         (current (seq-remove #'tether-thread-outdated threads))
         (point (point))
         (inhibit-read-only t))
    (erase-buffer)
    (setq tether-threads--positions nil)
    (magit-insert-section (tether-threads)
      (magit-insert-heading
        (format "%s %s: %d open, %d resolved, %d outdated"
                (tether--label (tether-context-repo ctx)
                               (oref (tether-context-pullreq ctx) number))
                (oref (tether-context-pullreq ctx) title)
                (length (tether--open-threads ctx))
                (seq-count #'tether-thread-resolved threads)
                (length outdated)))
      (pcase state
        ('loading (insert "\n  Fetching the review threads...\n"))
        ('error (insert "\n  The review threads could not be fetched; g to retry.\n"))
        (_ (when (null threads) (insert "\n  No review thread.\n"))))
      (dolist (group (seq-group-by #'tether-thread-path
                                   (sort current
                                         (lambda (a b)
                                           (< (or (tether-thread-line a) 0)
                                              (or (tether-thread-line b) 0))))))
        (insert "\n")
        (magit-insert-section (tether-threads-file (car group))
          (magit-insert-heading (format "%s (%d)" (car group) (length (cdr group))))
          (mapc #'tether-threads--insert-thread (cdr group))))
      (when outdated
        (insert "\n")
        (magit-insert-section (tether-threads-outdated)
          (magit-insert-heading (format "Outdated (%d)" (length outdated)))
          (dolist (thread outdated)
            (magit-insert-section (tether-threads-file (tether-thread-path thread))
              (magit-insert-heading (format " %s" (tether-thread-path thread)))
              (tether-threads--insert-thread thread))))))
    (goto-char (min point (point-max)))))

(defun tether--threads-buffer (ctx)
  "Return the thread list buffer of CTX, created if needed."
  (let ((buffer (get-buffer-create
                 (format "*tether-threads %s*"
                         (tether--label (tether-context-repo ctx)
                                        (oref (tether-context-pullreq ctx)
                                              number))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'tether-threads-mode)
        (tether-threads-mode))
      (setq default-directory (tether-context-toplevel ctx))
      (setq tether-threads--ctx ctx)
      (tether-threads--render))
    buffer))

(declare-function tether-reply "tether-comment")
(declare-function tether-show-thread "tether-comment")

(defun tether--threads-arrived (ctx)
  "Show the threads of CTX in its thread list and magit commit buffers."
  (tether--refresh-revision-buffers)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'tether-threads-mode)
                 tether-threads--ctx
                 (equal (tether--context-key tether-threads--ctx)
                        (tether--context-key ctx)))
        (setq tether-threads--ctx ctx)
        (tether-threads--render)))))

(add-hook 'tether-threads-functions #'tether--threads-arrived)



;;;###autoload
(defun tether-threads (&optional pick)
  "List the review threads of the pull request, by file.
At a line with a thread, go to that thread.  With prefix argument
PICK, choose the pull request."
  (interactive "P")
  (let* ((ctx (tether-context t pick))
         (at (and (not pick) (tether--location ctx))))
    (unless (memq (tether-context-threads-state ctx) '(loading done))
      (tether-fetch-threads ctx))
    (pop-to-buffer (tether--threads-buffer ctx))
    (tether-threads--goto-location ctx at)
    (current-buffer)))

(defun tether-threads--goto-location (ctx at)
  "In the thread list, go to CTX's thread at location AT, if any."
  (when-let* ((thread (and at (seq-find
                                 (lambda (th)
                                   (and (equal (tether-thread-path th)
                                               (plist-get at :path))
                                        (equal (tether-thread-line th)
                                               (plist-get at :line))))
                                 (tether-context-threads ctx))))
                (position (alist-get (tether-thread-id thread)
                                     tether-threads--positions nil nil #'equal)))
      (goto-char position)
      (magit-section-show (magit-current-section))))

(defun tether-threads--at-point ()
  "Return the thread at point in the thread list, or nil."
  (magit-section-value-if 'tether-thread))

(defun tether-threads-visit ()
  "Visit the line of the thread at point, or the file at point."
  (interactive)
  (let ((ctx tether-threads--ctx))
    (if-let* ((thread (tether-threads--at-point)))
        (if (tether-thread-line thread)
            (tether--visit-path ctx (tether-thread-path thread)
                                (and (eq (tether-thread-side thread) 'right)
                                     (tether-thread-line thread)))
          (user-error "Tether: this thread is outdated; its line is gone"))
      (if-let* ((path (magit-section-value-if 'tether-threads-file)))
          (tether--visit-path ctx path nil)
        (user-error "Tether: no thread at point")))))

(defun tether-threads-diff ()
  "Show the thread at point in the PR diff."
  (interactive)
  (let ((thread (or (tether-threads--at-point)
                    (user-error "Tether: no thread at point"))))
    (tether--show-diff tether-threads--ctx (tether-thread-path thread)
                       (and (eq (tether-thread-side thread) 'right)
                            (tether-thread-line thread)))))

(defun tether-threads-browse ()
  "Open the thread at point in the browser."
  (interactive)
  (let ((thread (or (tether-threads--at-point)
                    (user-error "Tether: no thread at point"))))
    (browse-url (or (plist-get (car (last (tether-thread-comments thread))) :url)
                    (user-error "Tether: the forge gave no address for it")))))

(defun tether-threads-refresh ()
  "Fetch the review threads again."
  (interactive)
  (tether-fetch-threads tether-threads--ctx)
  (tether-threads--render))

;;;; One thread

(defvar tether-comment--ctx)
(defvar tether-comment--target)

(defun tether--threads-ctx ()
  "Return the review context of the current buffer, for thread commands."
  (cond ((and (derived-mode-p 'tether-threads-mode) tether-threads--ctx))
        ((and (derived-mode-p 'tether-comment-mode)
              (plist-get tether-comment--target :thread))
         tether-comment--ctx)
        (t (tether-context t))))

(defun tether--thread-at-point (ctx)
  "Return CTX's review thread at point, or nil.
In the thread list, the thread at point; elsewhere, the thread on the
current line."
  (or (and (derived-mode-p 'tether-comment-mode)
           (plist-get tether-comment--target :thread))
      (and (derived-mode-p 'tether-threads-mode) (tether-threads--at-point))
      (seq-some (lambda (overlay) (overlay-get overlay 'tether-thread))
                (overlays-in (line-beginning-position) (line-end-position)))
      (when-let* ((at (tether--location ctx)))
        (seq-find (lambda (thread)
                    (and (equal (tether-thread-path thread) (plist-get at :path))
                         (equal (tether-thread-line thread) (plist-get at :line))
                         (eq (tether-thread-side thread) (plist-get at :side))))
                  (tether-context-threads ctx)))))

(defun tether--thread-at-point-or-error (ctx)
  "Return CTX's review thread at point, or signal a `user-error'."
  (or (tether--thread-at-point ctx)
      (user-error "Tether: no review thread on this line%s"
                  (if (eq (tether-context-threads-state ctx) 'done)
                      "" " (threads still fetching)"))))

(declare-function tether--refresh-buffer "tether")

;;;###autoload
(defun tether-toggle-resolved ()
  "Show or hide resolved review threads in file and magit commit buffers."
  (interactive)
  (setq tether-show-resolved (not tether-show-resolved))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p tether-mode)
        (tether--refresh-buffer))))
  (tether--refresh-revision-buffers)
  (message "Tether: resolved threads %s" (if tether-show-resolved "shown" "hidden")))

;;;###autoload
(defun tether-resolve ()
  "Resolve the review thread at point, or reopen it if it is resolved."
  (interactive)
  (let* ((ctx (tether--threads-ctx))
         (thread (tether--thread-at-point-or-error ctx))
         (resolve (not (tether-thread-resolved thread))))
    (tether--set-resolved (tether-context-repo ctx) ctx thread resolve)
    (message "Tether: %s the thread on %s" (if resolve "resolved" "reopened")
             (tether--thread-label thread))
    (tether-fetch-threads ctx)))

(defun tether--comment-start (comment)
  "Return the first line of COMMENT, shortened."
  (truncate-string-to-width
   (car (tether--comment-lines comment)) 60 nil nil "…"))

(defun tether--comment-choice (comment)
  "Return how to offer COMMENT in a choice."
  (format "%s: %s" (plist-get comment :author) (tether--comment-start comment)))

;;;###autoload
(defun tether-delete-comment ()
  "Delete a comment of the review thread at point, after confirmation.
With several comments, choose which.  The forge only lets you delete
your own."
  (interactive)
  (let* ((ctx (tether--threads-ctx))
         (thread (tether--thread-at-point-or-error ctx))
         (comments (tether-thread-comments thread))
         (comment (if (cdr comments)
                      (let ((choices (mapcar (lambda (c) (cons (tether--comment-choice c) c))
                                             comments)))
                        (cdr (assoc (completing-read "Delete which comment? " choices nil t)
                                    choices)))
                    (car comments))))
    (when (yes-or-no-p (format "Delete %s's comment “%s”? "
                               (plist-get comment :author)
                               (tether--comment-start comment)))
      (tether--delete-comment (tether-context-repo ctx) ctx comment)
      (message "Tether: comment deleted")
      (tether-fetch-threads ctx))))

(provide 'tether-threads)
;;; tether-threads.el ends here
