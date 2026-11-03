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
;; - `tether-thread-next' and `tether-thread-previous' go from thread to
;;   thread, across files;
;; - `tether-thread-list' lists them, by file, with the outdated ones and
;;   the hunk they were written on.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit)
(require 'magit-section)
(require 'transient)
(require 'diff-mode)
(require 'tether-context)
(require 'tether-lines)
(require 'tether-nav)

(defcustom tether-show-resolved t
  "Whether to show resolved threads in file buffers, dimmed.
`tether-thread-toggle-resolved' switches it."
  :type 'boolean
  :group 'tether)

(defcustom tether-thread-summary t
  "Whether to show the start of a thread at the end of its line."
  :type 'boolean
  :group 'tether)

(defcustom tether-diff-thread-comments t
  "Whether the PR diff and magit commit buffers show threads in full.
Under their line.  When nil, they show as in files: the start of the
thread at the end of its line.  Resolved threads always show that way."
  :type 'boolean
  :group 'tether)

(defcustom tether-cycle-across-files t
  "Whether `tether-thread-next' goes on to the next file's threads."
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

;;;; In diff buffers: the PR diff and magit commit buffers

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

(defun tether--draw-diff-threads (threads place)
  "Draw THREADS in the current diff buffer, each on its line.
PLACE returns the (SIDE . LINE) of a thread in the diff, or nil."
  (dolist (thread threads)
    (when-let* (((or tether-show-resolved (not (tether-thread-resolved thread))))
                (at (funcall place thread))
                (pos (tether--diff-line-position (tether-thread-path thread)
                                                 (car at) (cdr at))))
      (save-excursion
        (goto-char pos)
        (tether--draw-diff-thread thread)))))

(defun tether--pr-thread-line (thread)
  "Return (SIDE . LINE) of THREAD in its pull request's diff, or nil."
  (and (tether-thread-line thread)
       (not (tether-thread-outdated thread))
       (cons (tether-thread-side thread) (tether-thread-line thread))))

(defun tether--update-pr-diff-threads ()
  "Show the review threads in a magit diff buffer of a pull request.
The PR diff `tether-visit-diff' opens, or a magit diff of the same range;
in a single-commit review, the threads on that commit.  Never makes
a request."
  (with-demoted-errors "Tether: %S"
    (tether--clear-diff-threads)
    (when-let* ((ctx (tether-context))
                ((tether--diff-range-of-pr-p ctx))
                ;; A single-commit context holds no threads of its own.
                (pr-ctx (if (tether-context-commit ctx)
                            (gethash (tether--context-key ctx) tether--contexts)
                          ctx))
                ((eq (tether-context-threads-state pr-ctx) 'done)))
      (tether--draw-diff-threads
       (tether-context-threads pr-ctx)
       (if (tether-context-commit ctx)
           (let ((head (tether-context-head-sha pr-ctx)))
             (lambda (thread) (tether--commit-thread-line ctx head thread)))
         #'tether--pr-thread-line)))))

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
from the buffer then target that commit, as from `tether-commit-review',
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
              (tether--draw-diff-threads
               (tether-context-threads pr-ctx)
               (lambda (thread) (tether--commit-thread-line ctx head thread))))))))))

(defun tether--update-diff-threads ()
  "Show the review threads in the current magit diff or commit buffer."
  (cond ((derived-mode-p 'magit-revision-mode) (tether--update-revision-threads))
        ((derived-mode-p 'magit-diff-mode) (tether--update-pr-diff-threads))))

(defun tether--refresh-diff-buffers ()
  "Show the review threads again in every magit diff and commit buffer."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (tether--update-diff-threads))))

;;;; Going from thread to thread

(defun tether--thread-after-p (thread path line)
  "Return non-nil if THREAD comes after head LINE of PATH."
  (let ((other (tether-thread-path thread)))
    (or (string< path other)
        (and (equal path other) (> (tether-thread-line thread) line)))))

(defun tether--diff-thread-starts ()
  "Return the start of each line of the current diff buffer with a thread."
  (sort (delete-dups
         (mapcar #'overlay-start
                 (seq-filter (lambda (overlay) (overlay-get overlay 'tether-diff-thread))
                             (overlays-in (point-min) (point-max)))))
        #'<))

(defun tether--go-to-diff-thread (forward)
  "In a diff buffer, go to the next thread drawn in it.
Or the previous one unless FORWARD.  Shows its hunk if collapsed."
  (let* ((starts (tether--diff-thread-starts))
         (pos (or (if forward
                      (seq-find (lambda (start) (> start (line-end-position))) starts)
                    (car (last (seq-filter (lambda (start)
                                             (< start (line-beginning-position)))
                                           starts))))
                  (user-error "Tether: no %s review thread in this buffer"
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
In a diff buffer showing threads, only to the threads shown there."
  (if (or (derived-mode-p 'magit-revision-mode)
          (and (derived-mode-p 'magit-diff-mode) (tether--diff-thread-starts)))
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
(defun tether-thread-next ()
  "Go to the next review thread of the pull request.
In a diff or commit buffer showing threads, to the next one there."
  (interactive)
  (tether--go-to-thread t))

;;;###autoload
(defun tether-thread-previous ()
  "Go to the previous review thread of the pull request.
In a diff or commit buffer showing threads, to the previous one there."
  (interactive)
  (tether--go-to-thread nil))

;;;; The thread list

(defface tether-threads-heading
  '((t :inherit magit-section-heading))
  "Headings of the groups of the thread list: Your turn, Resolved..."
  :group 'tether)

(defface tether-threads-file
  '((t :inherit magit-diff-file-heading))
  "File and line of a thread in the thread list."
  :group 'tether)

(defface tether-threads-code
  '((t :inherit magit-diff-context-highlight :extend t))
  "Background of the code shown under a thread.
The code keeps the colors of its major mode on top of it."
  :group 'tether)

(defface tether-threads-line-number
  '((t :inherit shadow))
  "Line numbers of the code shown under a thread."
  :group 'tether)

(defface tether-threads-author
  '((t :inherit magit-log-author))
  "Author of a comment of a review thread."
  :group 'tether)

(defface tether-threads-date
  '((t :inherit magit-log-date))
  "Date of a comment of a review thread."
  :group 'tether)

(defface tether-threads-comment
  '((t :inherit font-lock-doc-face))
  "Text of a comment of a review thread."
  :group 'tether)

(defvar-local tether-threads--ctx nil
  "Review context of the thread list.")

(defvar-local tether-threads--positions nil
  "Where each thread is in the list, as (ID . POSITION).")

(defvar-local tether-threads--files nil
  "Contents of the files shown in the list, by (REV . PATH).
Each value is a vector of lines, read once per rendering.")

(declare-function tether-comment-edit "tether-comment")

(transient-define-prefix tether-threads-menu ()
  "Act on the thread at point in the thread list."
  [["Thread"
    ("RET" "visit its line" tether-threads-visit)
    ("SPC" "open it, with a reply area" tether-thread-show)
    ("r" "reply" tether-thread-reply)
    ("R" "resolve / reopen" tether-thread-resolve)
    ("e" "edit one of your comments" tether-comment-edit)
    ("k" "delete one of your comments" tether-comment-delete)]
   ["Show"
    ("d" "in the PR diff" tether-threads-diff)
    ("b" "in the browser" tether-threads-browse)
    ("t" "the forge topic" tether-visit-topic)
    ("m" "magit-status" tether-visit-status)]
   ["List"
    ("g" "refresh" tether-threads-refresh)
    ("n" "next thread" magit-section-forward :transient t)
    ("p" "previous thread" magit-section-backward :transient t)]])

(defvar-keymap tether-threads-mode-map
  :parent magit-section-mode-map
  "RET" #'tether-threads-visit
  "d" #'tether-threads-diff
  "b" #'tether-threads-browse
  "SPC" #'tether-thread-show
  "r" #'tether-thread-reply
  "R" #'tether-thread-resolve
  "e" #'tether-comment-edit
  "k" #'tether-comment-delete
  "t" #'tether-visit-topic
  "m" #'tether-visit-status
  "g" #'tether-threads-refresh
  "?" #'tether-threads-menu)

(define-derived-mode tether-threads-mode magit-section-mode "Tether-Threads"
  "The review threads of a pull request, by what they wait for.
\"Your turn\" lists the open threads whose last comment is not yours,
\"Waiting for others\" those where you spoke last; resolved and
outdated threads come after.  Under each thread, the code it is on
and its comments.  \\<tether-threads-mode-map>\\[tether-threads-menu] \
shows every action.

\\{tether-threads-mode-map}"
  (setq header-line-format
        (substitute-command-keys
         "\\<tether-threads-mode-map>  \\[tether-threads-visit] visit  \
\\[tether-thread-show] open  \\[tether-thread-reply] reply  \
\\[tether-thread-resolve] resolve  \\[tether-threads-refresh] refresh  \
\\[tether-threads-menu] all keys")))

;;;;; Text

(defun tether--format-date (iso)
  "Return the date ISO, an ISO 8601 string, as \"Nov 3 11:34\", or nil."
  (when-let* ((time (and iso (condition-case nil (parse-iso8601-time-string iso)
                               (error nil)))))
    (format-time-string (if (equal (format-time-string "%Y" time)
                                   (format-time-string "%Y"))
                            "%b %-d %H:%M"
                          "%b %-d %Y")
                        time)))

(defun tether--format-ago (iso)
  "Return how long ago ISO, an ISO 8601 string, was: \"2h ago\", or nil."
  (when-let* ((time (and iso (condition-case nil (parse-iso8601-time-string iso)
                               (error nil)))))
    (let ((seconds (float-time (time-subtract nil time))))
      (cond ((< seconds 60) "just now")
            ((< seconds 3600) (format "%dm ago" (/ seconds 60)))
            ((< seconds 86400) (format "%dh ago" (/ seconds 3600)))
            ((< seconds (* 30 86400)) (format "%dd ago" (/ seconds 86400)))
            (t (tether--format-date iso))))))

(defun tether--fontify-as (text file)
  "Return TEXT colored by the major mode of a file named FILE.
The mode's hooks do not run: no language server starts for it.
TEXT comes back uncolored when the mode fails."
  (with-temp-buffer
    (insert text)
    (condition-case nil
        (progn
          (let ((buffer-file-name file))
            (delay-mode-hooks (set-auto-mode)))
          (font-lock-ensure)
          ;; `face' is font-lock's own, which the list buffer would
          ;; recompute; `font-lock-face' stays.
          (let ((pos (point-min)))
            (while (< pos (point-max))
              (let ((next (next-single-property-change pos 'face nil (point-max))))
                (when-let* ((face (get-text-property pos 'face)))
                  (put-text-property pos next 'font-lock-face face))
                (setq pos next)))))
      (error nil))
    (buffer-substring (point-min) (point-max))))

;;;;; One thread

(defun tether-threads--insert-comments (thread)
  "Insert the comments of THREAD, indented: author, date and text.
For an outdated thread, also the hunk it was written on."
  (let ((bar (propertize "  ▎ " 'font-lock-face 'tether-thread)))
    (dolist (comment (tether-thread-comments thread))
      (insert bar
              (propertize (or (plist-get comment :author) "?")
                          'font-lock-face 'tether-threads-author)
              (if-let* ((date (tether--format-date (plist-get comment :created))))
                  (concat "  " (propertize date 'font-lock-face 'tether-threads-date))
                "")
              "\n")
      (dolist (line (tether--comment-lines comment))
        (insert bar "  "
                (propertize line 'font-lock-face 'tether-threads-comment)
                "\n"))))
  (when-let* ((hunk (and (tether-thread-outdated thread)
                         (tether-thread-diff-hunk thread))))
    (insert (propertize "    The code when it was written:\n"
                        'font-lock-face 'tether-threads-line-number))
    (dolist (line (split-string hunk "\n"))
      (insert "    "
              (propertize line 'font-lock-face
                          (pcase (string-to-char line)
                            (?+ 'diff-added) (?- 'diff-removed)
                            (_ 'diff-context)))
              "\n"))))

(defun tether-threads--file-lines (rev path)
  "Return the lines of PATH at REV as a vector, read once per rendering."
  (let ((key (cons rev path)))
    (or (cdr (assoc key tether-threads--files))
        (let ((lines (vconcat
                      (with-temp-buffer
                        (magit-git-insert "show" (format "%s:%s" rev path))
                        (split-string (buffer-string) "\n")))))
          (push (cons key lines) tether-threads--files)
          lines))))

(defun tether-threads--insert-code (ctx thread)
  "Insert the code THREAD of CTX is on, a line around it, in its colors."
  (when-let* ((line (tether-thread-line thread))
              (rev (if (eq (tether-thread-side thread) 'left)
                       (tether-context-base-sha ctx)
                     (tether-context-head-sha ctx)))
              (lines (tether-threads--file-lines rev (tether-thread-path thread)))
              (first (or (tether-thread-start-line thread) line))
              (beg (max 1 (1- first)))
              (end (min (1- (length lines)) (1+ line)))
              ((<= beg end)))
    (let* ((text (mapconcat (lambda (n) (aref lines (1- n)))
                            (number-sequence beg end) "\n"))
           (colored (split-string (tether--fontify-as text (tether-thread-path thread))
                                  "\n"))
           (width (length (number-to-string end))))
      (cl-loop for n from beg to end
               for code in colored
               do (let ((here (<= first n line))
                        (start (point)))
                    (insert "    "
                            (propertize (format (format "%%%dd" width) n) 'font-lock-face
                                        (if here 'tether-thread 'tether-threads-line-number))
                            (if here (propertize " ▶ " 'font-lock-face 'tether-thread) "   ")
                            code "\n")
                    (tether-threads--add-face start (point) 'tether-threads-code))))))

(defun tether-threads--add-face (beg end face)
  "Add FACE below the `font-lock-face' of the text from BEG to END."
  (let ((pos beg))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'font-lock-face nil end))
            (faces (ensure-list (get-text-property pos 'font-lock-face))))
        (put-text-property pos next 'font-lock-face (append faces (list face)))
        (setq pos next)))))

(defun tether-threads--heading (thread)
  "Return the heading of THREAD: its place, who spoke last, and when."
  (let* ((comments (tether-thread-comments thread))
         (last (car (last comments)))
         (count (length comments)))
    (concat
     (propertize (format "%s:%s" (tether-thread-path thread)
                         (or (tether-thread-line thread)
                             (tether-thread-original-line thread) "?"))
                 'font-lock-face 'tether-threads-file)
     (if (eq (tether-thread-side thread) 'left)
         (propertize " (removed line)" 'font-lock-face 'tether-threads-line-number)
       "")
     "  "
     (propertize (format "%s%s%s"
                         (or (plist-get last :author) "?")
                         (if-let* ((ago (tether--format-ago (plist-get last :created))))
                             (concat " · " ago)
                           "")
                         (if (> count 1) (format " · %d comments" count) ""))
                 'font-lock-face 'tether-threads-line-number))))

(defun tether-threads--insert-thread (ctx thread hide)
  "Insert THREAD of CTX as a section, hidden if HIDE."
  (push (cons (tether-thread-id thread) (point)) tether-threads--positions)
  (magit-insert-section (tether-thread thread hide)
    (magit-insert-heading (tether-threads--heading thread))
    (tether-threads--insert-code ctx thread)
    (tether-threads--insert-comments thread)
    (insert "\n")))

;;;;; The list

(defun tether-threads--order (threads)
  "Return THREADS sorted by file, then line."
  (sort (copy-sequence threads)
        (lambda (a b)
          (let ((pa (tether-thread-path a)) (pb (tether-thread-path b)))
            (if (equal pa pb)
                (< (or (tether-thread-line a) (tether-thread-original-line a) 0)
                   (or (tether-thread-line b) (tether-thread-original-line b) 0))
              (string< pa pb))))))

(defun tether-threads--groups (ctx)
  "Return the groups of CTX's threads, as (TYPE HEADING HIDE THREADS).
Without the user's login, the open threads make one group."
  (let* ((threads (tether-context-threads ctx))
         (me (condition-case nil (tether--username (tether-context-repo ctx))
               (error nil)))
         (open (seq-remove (lambda (th) (or (tether-thread-resolved th)
                                            (tether-thread-outdated th)))
                           threads))
         (mine (lambda (th) (equal (plist-get (car (last (tether-thread-comments th)))
                                              :author)
                                   me))))
    (append
     (if me
         (list (list 'tether-threads-yours "Your turn" nil (seq-remove mine open))
               (list 'tether-threads-waiting "Waiting for others" nil
                     (seq-filter mine open)))
       (list (list 'tether-threads-open "Open" nil open)))
     (list (list 'tether-threads-resolved "Resolved" t
                 (seq-filter #'tether-thread-resolved threads))
           (list 'tether-threads-outdated "Outdated" t
                 (seq-filter (lambda (th) (and (tether-thread-outdated th)
                                               (not (tether-thread-resolved th))))
                             threads))))))

(defun tether-threads--visibility ()
  "Return which groups and threads of the list are hidden, to keep them so.
An alist of (TYPE-OR-ID . HIDDEN)."
  (let (result)
    (when magit-root-section
      (dolist (group (oref magit-root-section children))
        (push (cons (oref group type) (oref group hidden)) result)
        (dolist (thread (oref group children))
          (when (magit-section-match 'tether-thread thread)
            (push (cons (tether-thread-id (oref thread value)) (oref thread hidden))
                  result)))))
    result))

(defun tether-threads--render ()
  "Insert the thread list of `tether-threads--ctx'."
  (let* ((ctx tether-threads--ctx)
         (threads (tether-context-threads ctx))
         (state (tether-context-threads-state ctx))
         (visibility (tether-threads--visibility))
         (point (point))
         (inhibit-read-only t))
    (erase-buffer)
    (setq tether-threads--positions nil
          tether-threads--files nil)
    (magit-insert-section (tether-threads)
      (magit-insert-heading
        (propertize (format "%s %s"
                            (tether--label (tether-context-repo ctx)
                                           (oref (tether-context-pullreq ctx) number))
                            (oref (tether-context-pullreq ctx) title))
                    'font-lock-face 'tether-threads-heading))
      (insert (propertize
               (format "%d open · %d resolved · %d outdated\n"
                       (length (tether--open-threads ctx))
                       (seq-count #'tether-thread-resolved threads)
                       (seq-count (lambda (th) (and (tether-thread-outdated th)
                                                    (not (tether-thread-resolved th))))
                                  threads))
               'font-lock-face 'tether-threads-line-number))
      (pcase state
        ('loading (insert "\nFetching the review threads...\n"))
        ('error (insert "\nThe review threads could not be fetched; g to retry.\n"))
        (_ (when (null threads) (insert "\nNo review thread.\n"))))
      (pcase-dolist (`(,type ,heading ,hide ,group) (tether-threads--groups ctx))
        (when group
          ;; One blank line between groups: a thread ends with one.
          (unless (looking-back "\n\n" (- (point) 2))
            (insert "\n"))
          (magit-insert-section ((eval type) nil
                                 (alist-get type visibility hide))
            (magit-insert-heading
              (propertize (format "%s (%d)" heading (length group))
                          'font-lock-face 'tether-threads-heading))
            (dolist (thread (tether-threads--order group))
              (tether-threads--insert-thread
               ctx thread
               (alist-get (tether-thread-id thread) visibility
                          ;; Threads waiting for others start folded.
                          (not (memq type '(tether-threads-yours
                                            tether-threads-open)))
                          nil #'equal)))))))
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

(declare-function tether-thread-reply "tether-comment")
(declare-function tether-thread-show "tether-comment")

(defun tether--threads-arrived (ctx)
  "Show the threads of CTX in its thread list and diff buffers."
  (tether--refresh-diff-buffers)
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
(defun tether-thread-list (&optional pick)
  "List the review threads of the pull request, by what they wait for.
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
      ;; The thread, and its group: Resolved and Outdated start folded.
      (let ((section (magit-current-section)))
        (while section
          (magit-section-show section)
          (setq section (oref section parent))))))

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
      (user-error "Tether: no thread at point"))))

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
(defun tether-thread-toggle-resolved ()
  "Show or hide resolved review threads in file and diff buffers."
  (interactive)
  (setq tether-show-resolved (not tether-show-resolved))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p tether-mode)
        (tether--refresh-buffer))))
  (tether--refresh-diff-buffers)
  (message "Tether: resolved threads %s" (if tether-show-resolved "shown" "hidden")))

;;;###autoload
(defun tether-thread-resolve ()
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

(defun tether--choose-comment (comments prompt)
  "Return one of COMMENTS, read with PROMPT when there are several."
  (if (cdr comments)
      (let ((choices (mapcar (lambda (c) (cons (tether--comment-choice c) c))
                             comments)))
        (cdr (assoc (completing-read prompt choices nil t) choices)))
    (car comments)))

;;;###autoload
(defun tether-comment-delete ()
  "Delete a comment of the review thread at point, after confirmation.
With several comments, choose which.  The forge only lets you delete
your own."
  (interactive)
  (let* ((ctx (tether--threads-ctx))
         (thread (tether--thread-at-point-or-error ctx))
         (comment (tether--choose-comment (tether-thread-comments thread)
                                          "Delete which comment? ")))
    (when (yes-or-no-p (format "Delete %s's comment “%s”? "
                               (plist-get comment :author)
                               (tether--comment-start comment)))
      (tether--delete-comment (tether-context-repo ctx) ctx comment)
      (message "Tether: comment deleted")
      (tether-fetch-threads ctx))))

(provide 'tether-threads)
;;; tether-threads.el ends here
