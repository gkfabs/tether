;;; tether.el --- Review PRs and MRs from the file you are reading  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; Maintainer: Fabien Carrion <fabien@carrion.mx>
;; URL: https://github.com/gkfabs/tether
;; Keywords: tools, vc
;; Version: 0.1.0-snapshot
;; Package-Requires: ((emacs "29.1") (compat "31.0") (magit "4.6") (forge "0.6") (ghub "5.3") (transient "0.10") (markdown-mode "2.7"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tether ties the code you are reading to its pull request (GitHub) or
;; merge request (GitLab): comment on the current line or region from
;; the file buffer, and jump between the file, the PR diff, the forge
;; topic, the review threads and the browser while keeping file and
;; line.
;;
;; It is built on Forge: the repository, the pull request and the API
;; token all come from Forge.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'forge)
(require 'transient)
(require 'tether-deps)
(require 'tether-context)
(require 'tether-lines)
(require 'tether-comment)
(require 'tether-nav)
(require 'tether-threads)
(require 'tether-review)

(defgroup tether nil
  "Review pull requests and merge requests from file buffers."
  :group 'tools
  :link '(url-link "https://github.com/gkfabs/tether"))

(defconst tether-version "0.1.0-snapshot"
  "Version of the tether package.")

(defcustom tether-show-commentable t
  "Whether to show which lines can take a comment, in the fringe.
Only when the pull request was already resolved, by any tether
command: opening a file never makes a request."
  :type 'boolean
  :group 'tether)

(defcustom tether-enable-remote nil
  "Whether `global-tether-mode' also turns on in remote (TRAMP) files.
Finding their pull request runs git on the remote host."
  :type 'boolean
  :group 'tether)

(defcustom tether-add-to-dispatch t
  "Whether to add tether to `magit-dispatch' and `forge-dispatch', on @.
Set it before tether is loaded."
  :type 'boolean
  :group 'tether)

(defface tether-commentable
  '((t :inherit shadow))
  "Fringe mark of unchanged lines of the diff that can take a comment."
  :group 'tether)

(defface tether-commentable-added
  '((t :inherit success))
  "Fringe mark of lines the pull request adds or changes."
  :group 'tether)

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'tether-commentable [#b00011000] nil 8 '(center t)))

;;;; The pull request of a buffer, without requests

(defvar tether--branch-prs (make-hash-table :test #'equal)
  "Pull request of each (TOPLEVEL . BRANCH), as (REPO . PULLREQ) or nil.
Filled from forge's database only; cleared when magit refreshes.")

(defun tether--repository-maybe (toplevel)
  "Return the forge repository of TOPLEVEL if forge tracks it, else nil."
  (let ((default-directory toplevel))
    (forge-get-repository :tracked? nil 'notatpt)))

(defun tether--find-branch-pr (toplevel branch)
  "Return (REPO . PULLREQ) of BRANCH in TOPLEVEL from forge's database."
  (when-let* ((branch)
              (repo (tether--repository-maybe toplevel)))
    (let* ((default-directory toplevel)
           (pullreq (or (tether--branch-pullreq branch)
                        (let ((found (tether--upstream-pullreqs repo branch)))
                          (and (not (cdr found)) (car found))))))
      (and pullreq (cons repo pullreq)))))

(defun tether--buffer-pr ()
  "Return (REPO . PULLREQ) of the current buffer, or nil.
Never prompts and never makes a request."
  (when-let* ((toplevel (magit-toplevel)))
    (if tether--buffer-pullreq
        (when-let* ((pullreq (forge-get-pullreq tether--buffer-pullreq)))
          (cons (tether--repository-maybe toplevel) pullreq))
      (let ((key (cons toplevel (magit-get-current-branch))))
        (pcase (gethash key tether--branch-prs 'unknown)
          ('unknown (puthash key (tether--find-branch-pr (car key) (cdr key))
                             tether--branch-prs))
          (found found))))))

;;;; Mode line and fringe

(defvar-local tether--lighter nil
  "What `tether-mode' shows in the mode line.")
(put 'tether--lighter 'risky-local-variable t)

(defvar-local tether--fringe-overlays nil
  "Overlays marking the lines that can take a comment.")

(defun tether--update-lighter (found)
  "Set the mode line of the current buffer for FOUND, (REPO . PULLREQ).
A warning sign means the local branch is not the PR head."
  (setq tether--lighter
        (when found
          (let* ((number (oref (cdr found) number))
                 (ctx (gethash (cons (magit-toplevel) number) tether--contexts)))
            (concat " " (tether--label (car found) number)
                    (and ctx (tether-context-checked-out ctx)
                         (not (eq (car (tether-context-head-state ctx)) 'same))
                         "⚠")
                    (pcase (and ctx (tether-context-threads-state ctx))
                      ('loading "·…")
                      ('done (let ((open (length (tether--open-threads ctx))))
                               (if (> open 0) (format "·%d" open) ""))))
                    (and ctx (tether--review-active-p ctx)
                         (format "✎%d" (or (plist-get (tether-context-review ctx)
                                                      :drafts)
                                           0))))))))

(defun tether--clear-fringe ()
  "Remove the commentable-line overlays of the current buffer."
  (mapc #'delete-overlay tether--fringe-overlays)
  (setq tether--fringe-overlays nil))

(defun tether--update-fringe ()
  "Show which lines of the current buffer can take a comment.
Uses the cached context only, so it never makes a request."
  (tether--clear-fringe)
  (when-let* (((and tether-show-commentable (display-graphic-p)
                    (fboundp 'define-fringe-bitmap)))
              (ctx (tether-context))
              (path (let ((default-directory (tether-context-toplevel ctx)))
                      (magit-file-relative-name))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((current 1))
          (pcase-dolist (`(,line . ,kind) (tether-commentable-lines ctx path))
            (forward-line (- line current))
            (setq current line)
            (let ((overlay (make-overlay (point) (point))))
              (overlay-put overlay 'before-string
                           (propertize " " 'display
                                       `(left-fringe tether-commentable
                                                     ,(if (eq kind :added)
                                                          'tether-commentable-added
                                                        'tether-commentable))))
              (push overlay tether--fringe-overlays))))))))

(defun tether--update-threads ()
  "Show the review threads of the current buffer's file.
Uses the cached context only, so it never makes a request."
  (tether--clear-thread-overlays)
  (when-let* ((ctx (tether-context)))
    ;; Resolved before any buffer was in `tether-mode' (from magit-status,
    ;; say): its threads were never asked for.
    (unless (tether-context-threads-state ctx)
      (tether-fetch-threads ctx))
    (when-let* (((eq (tether-context-threads-state ctx) 'done))
                (path (let ((default-directory (tether-context-toplevel ctx)))
                        (magit-file-relative-name))))
      (tether--update-thread-overlays ctx path))))

(defun tether--refresh-buffer ()
  "Update the mode line, fringe and threads of the current buffer."
  (with-demoted-errors "Tether: %S"
    (tether--update-lighter (tether--buffer-pr))
    (tether--update-fringe)
    (tether--update-threads)
    (force-mode-line-update)))

;;;; Keys and menu

(defvar-keymap tether-prefix-map
  :doc "Tether commands, to bind under a prefix of your choice.
For example: (keymap-set tether-mode-map \"C-c t\" tether-prefix-map)."
  "@" #'tether
  "c" #'tether-nitpick
  "l" #'tether-threads
  "." #'tether-show-thread
  "r" #'tether-reply
  "R" #'tether-resolve
  "k" #'tether-delete-comment
  "H" #'tether-toggle-resolved
  "w" #'tether-review-start
  "W" #'tether-review-submit
  "X" #'tether-review-discard
  "A" #'tether-approve
  "n" #'tether-next-thread
  "p" #'tether-previous-thread
  "S" #'tether-suggest
  "F" #'tether-nitpick-file
  "f" #'tether-visit-file
  "d" #'tether-diff
  "t" #'tether-topic
  "b" #'tether-browse
  "m" #'tether-status
  "v" #'tether-review-pullreq
  "u" #'tether-review-url
  "C" #'tether-review-commit
  "s" #'tether-show-context
  "g" #'tether-refresh)

(defvar-keymap tether-mode-map
  :doc "Keymap of `tether-mode'.
Empty: C-c LETTER keys belong to users and C-c C-LETTER ones to major
modes, so bind `tether-prefix-map' where you like.")

(defun tether--menu-description ()
  "Return the heading of the `tether' menu."
  (if-let* ((ctx (ignore-errors (tether-context))))
      (let ((pullreq (tether-context-pullreq ctx)))
        (format "Tether  %s %s  (head %s)"
                (tether--label (tether-context-repo ctx) (oref pullreq number))
                (oref pullreq title)
                (substring (tether-context-head-sha ctx) 0 7)))
    "Tether"))

;;;###autoload (autoload 'tether "tether" nil t)
(transient-define-prefix tether ()
  "Comment on the pull request of the current buffer and move around it."
  [:description tether--menu-description
   ["Comment"
    ("c" "line or region" tether-nitpick)
    ("S" "suggest a change" tether-suggest)
    ("F" "whole file" tether-nitpick-file)]
   ["Go to"
    ("f" "file at line" tether-visit-file)
    ("d" "PR diff at hunk" tether-diff)
    ("t" "forge topic" tether-topic)
    ("b" "browser at line" tether-browse)
    ("m" "magit-status" tether-status)]
   ["Threads"
    ("l" "list" tether-threads)
    ("." "this line's thread, reply" tether-show-thread)
    ("R" "resolve / reopen" tether-resolve)
    ("k" "delete a comment" tether-delete-comment)
    ("H" "show / hide resolved" tether-toggle-resolved)
    ("n" "next" tether-next-thread :transient t)
    ("p" "previous" tether-previous-thread :transient t)]
   ["Review"
    ("w" "start a review" tether-review-start)
    ("W" "submit the review" tether-review-submit)
    ("X" "discard the review" tether-review-discard)
    ("A" "approve" tether-approve)]
   ["Pull request"
    ("v" "review another" tether-review-pullreq)
    ("u" "review from a URL" tether-review-url)
    ("C" "review one commit" tether-review-commit)
    ("s" "show" tether-show-context)
    ("g" "refresh" tether-refresh)
    ("x" "clear cache" tether-clear-cache)]])

(defun tether--add-to-dispatch (prefix after)
  "Add tether on @ to the transient PREFIX, after the suffix on AFTER."
  (unless (ignore-errors (transient-get-suffix prefix "@"))
    (ignore-errors
      (transient-append-suffix prefix after '("@" "Tether" tether)))))

(when tether-add-to-dispatch
  (tether--add-to-dispatch 'magit-dispatch "N")
  (tether--add-to-dispatch 'forge-dispatch "v t"))

;;;; Modes

(defvar tether-mode)
(defvar global-tether-mode)

(defun tether--under-p (toplevel)
  "Return non-nil if the current buffer's directory is inside TOPLEVEL."
  (string-prefix-p toplevel (expand-file-name default-directory)))

(defun tether--context-resolved (ctx)
  "Update the buffers of CTX's worktree after CTX was resolved.
Also start fetching its review threads."
  (unless (tether-context-threads-state ctx)
    (tether-fetch-threads ctx))
  (unless (tether-context-review ctx)
    (tether-fetch-review ctx))
  (tether--refresh-worktree ctx))

(defun tether--refresh-worktree (ctx)
  "Update the buffers of CTX's worktree."
  (let ((toplevel (tether-context-toplevel ctx)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (tether--under-p toplevel)
          (if tether-mode
              (tether--refresh-buffer)
            (when global-tether-mode
              (tether--turn-on))))))))

(defun tether--magit-refreshed ()
  "Update tether buffers after magit refreshed, as after a checkout."
  (when-let* ((toplevel (magit-toplevel)))
    (clrhash tether--branch-prs)
    (maphash (lambda (key ctx)
               (when (and (equal (car key) toplevel)
                          (not (tether--context-fresh-p ctx)))
                 (remhash key tether--contexts)))
             tether--contexts)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (tether--under-p toplevel)
          (cond ((and tether-mode (not tether--buffer-pullreq)
                      (not (ignore-errors (tether--buffer-pr))))
                 (tether-mode -1))
                (tether-mode (tether--refresh-buffer))
                (global-tether-mode (tether--turn-on))))))))

;;;###autoload
(define-minor-mode tether-mode
  "Tie the current buffer to its pull request.

Shows the pull request in the mode line, with a warning sign when
the local branch is not the PR head and the number of open review
threads.  In file buffers, also marks the lines that can take a
comment and shows the threads, once the PR was resolved.
`global-tether-mode' turns it on in file buffers and `magit-status'.  Commands
are in the `tether' menu and `tether-prefix-map'.

\{tether-prefix-map}"
  :lighter (:eval tether--lighter)
  :keymap tether-mode-map
  :group 'tether
  (if tether-mode
      (progn
        (add-hook 'after-save-hook #'tether--refresh-buffer nil t)
        (add-hook 'magit-post-refresh-hook #'tether--magit-refreshed)
        (tether--refresh-buffer))
    (remove-hook 'after-save-hook #'tether--refresh-buffer t)
    (tether--clear-fringe)
    (tether--clear-thread-overlays)
    (setq tether--lighter nil)))

(defun tether--turn-on ()
  "Turn on `tether-mode' if the buffer belongs to a pull request.
In file buffers and `magit-status'.  Looks in forge's database only:
no request when a file is opened."
  (when (and (not tether-mode)
             (or buffer-file-name tether--buffer-pullreq
                 (derived-mode-p 'magit-status-mode))
             (or tether-enable-remote (not (file-remote-p default-directory)))
             (ignore-errors (tether--buffer-pr)))
    (tether-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-tether-mode tether-mode tether--turn-on
  :group 'tether)

;; Whatever buffer a pull request is resolved from (magit-status, a
;; diff, a file), fetch its threads and update the buffers showing it.
(add-hook 'tether-context-functions #'tether--context-resolved)
(add-hook 'tether-threads-functions #'tether--refresh-worktree)
(add-hook 'tether-review-functions #'tether--refresh-worktree)

(provide 'tether)
;;; tether.el ends here
