;;; tether-threads.el --- Review threads in files and in a list  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
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
                         (tether-diff-parse-drift
                          (tether--git-diff "-U0" sha head "--"
                                            (tether-thread-path thread)))
                         (tether-thread-line thread)))))
        (setf (tether-thread-line thread) line
              (tether-thread-outdated thread) (null line))))
    thread))

(defun tether-fetch-threads (ctx)
  "Fetch the review threads of CTX's pull request in the background.
When they arrive, if CTX is still current, run `tether-threads-functions'."
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
       (run-hook-with-args 'tether-threads-functions ctx)))))

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

(defun tether--thread-first-line (thread)
  "Return the author and first line of THREAD's first comment."
  (let ((first (car (tether-thread-comments thread))))
    (format "%s: %s" (plist-get first :author)
            (car (split-string (or (plist-get first :body) "") "\n")))))

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
          (let ((overlay (make-overlay (point) (line-end-position)))
                (face (if (tether-thread-resolved thread)
                          'tether-thread-resolved 'tether-thread)))
            (overlay-put overlay 'tether-thread thread)
            (overlay-put overlay 'priority 10)
            (overlay-put overlay 'help-echo (tether--thread-first-line thread))
            (when (display-graphic-p)
              (overlay-put overlay 'before-string
                           (propertize " " 'display
                                       `(left-fringe tether-thread ,face))))
            (when tether-thread-summary
              (overlay-put overlay 'after-string
                           (propertize (tether--thread-summary thread)
                                       'face 'tether-thread-summary)))
            (push overlay tether--thread-overlays)))))))

;;;; Going from thread to thread

(defun tether--thread-after-p (thread path line)
  "Return non-nil if THREAD comes after head LINE of PATH."
  (let ((other (tether-thread-path thread)))
    (or (string< path other)
        (and (equal path other) (> (tether-thread-line thread) line)))))

(defun tether--go-to-thread (forward)
  "Go to the next thread, or the previous one unless FORWARD."
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
  "Go to the next review thread of the pull request."
  (interactive)
  (tether--go-to-thread t))

;;;###autoload
(defun tether-previous-thread ()
  "Go to the previous review thread of the pull request."
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
    (dolist (line (split-string (string-trim (or (plist-get comment :body) ""))
                                "\n"))
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
  "Show the threads of CTX in its thread list, if open."
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
  "Show or hide resolved review threads in file buffers."
  (interactive)
  (setq tether-show-resolved (not tether-show-resolved))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p tether-mode)
        (tether--refresh-buffer))))
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
   (car (split-string (or (plist-get comment :body) "") "\n")) 60 nil nil "…"))

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
