;;; tether-nav.el --- Jump between a file, its PR diff, topic and browser  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; Assisted-by: Claude:claude-opus-5-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Every view of a pull request reaches every other one, keeping the
;; file and line where that makes sense:
;;
;; - `tether-visit-file': the file at the line, in the worktree when
;;   the PR is checked out, else the PR head's version (a blob buffer);
;; - `tether-diff': the PR diff, at the hunk of the line;
;; - `tether-topic': forge's buffer of the pull request;
;; - `tether-browse': the PR's diff in the browser, at the line;
;; - `tether-status': magit-status of the worktree;
;; - `tether-review-pullreq': pick any PR, checked out or not, and
;;   open its diff.
;;
;; The line is the PR head's line: from a file buffer it is mapped
;; through local edits, from a diff buffer it is the line at point.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'forge)
(require 'tether-context)
(require 'tether-lines)

(declare-function tether-mode "tether")

;;;; Diff buffers

(defun tether--diff-file-section (path)
  "Return the section of PATH in the current diff buffer, or nil."
  (seq-find (lambda (section)
              (and (magit-section-match 'file section)
                   (equal (oref section value) path)))
            (oref magit-root-section children)))

(defun tether--hunk-lines (hunk function)
  "Call FUNCTION on each line of HUNK until it returns non-nil.
Return that value.  FUNCTION is called with point at the start of the
line, with its kind (:context, :added or :removed) and its line
numbers on the old and new sides, nil on the side it is absent from.
The \"\\ No newline\" marker is not a line: it is skipped."
  (save-excursion
    (goto-char (oref hunk content))
    (let ((old (car (oref hunk from-range)))
          (new (car (oref hunk to-range)))
          result)
      (while (and (not result) (< (point) (oref hunk end)))
        (let ((kind (pcase (char-after)
                      (?+ :added) (?- :removed) (?\\ :marker) (_ :context))))
          (unless (eq kind :marker)
            (setq result (funcall function kind
                                  (and (not (eq kind :added)) old)
                                  (and (not (eq kind :removed)) new)))
            (unless (eq kind :added) (cl-incf old))
            (unless (eq kind :removed) (cl-incf new))))
        (forward-line 1))
      result)))

(defun tether--diff-line-at (pos)
  "Return what the diff line at POS is, as a plist, or nil.
The plist has :path, :old-path, :kind (:context, :added or :removed),
:old and :new (the line numbers on each side, nil when absent) and
:hunk, the hunk section.  POS must be on a line of a hunk."
  (save-excursion
    (goto-char pos)
    (let ((hunk (magit-current-section))
          (target (line-beginning-position)))
      (when (and hunk (magit-section-match 'hunk hunk)
                 (>= target (oref hunk content)))
        (let ((file (oref hunk parent)))
          (tether--hunk-lines
           hunk (lambda (kind old new)
                  (and (= (point) target)
                       (list :path (oref file value)
                             :old-path (or (oref file source) (oref file value))
                             :kind kind :old old :new new :hunk hunk)))))))))

(defun tether--diff-line-position (path side line)
  "Return the start of LINE of PATH in the current diff buffer, or nil.
LINE is on the new side of the diff when SIDE is `right', on the old
side when it is `left'."
  (when-let* ((file (tether--diff-file-section path)))
    (seq-some
     (lambda (hunk)
       (when (magit-section-match 'hunk hunk)
         (pcase-let ((`(,start ,len) (if (eq side 'left)
                                         (oref hunk from-range)
                                       (oref hunk to-range))))
           (and (<= start line) (< line (+ start (max len 1)))
                (tether--hunk-lines
                 hunk (lambda (_kind old new)
                        (and (eql (if (eq side 'left) old new) line)
                             (point))))))))
     (oref file children))))

(defun tether--pr-commit-p (ctx sha)
  "Return non-nil if SHA is one of the commits of CTX's pull request."
  (let ((default-directory (tether-context-toplevel ctx)))
    (and (magit-rev-ancestor-p sha (tether-context-head-sha ctx))
         (not (magit-rev-ancestor-p sha (tether-context-base-sha ctx))))))

(defun tether--diff-range-of-pr-p (ctx)
  "Return non-nil if the current magit diff buffer is CTX's PR diff.
A magit commit buffer is, when CTX reviews that commit alone."
  (if (derived-mode-p 'magit-revision-mode)
      (and (tether-context-commit ctx)
           (equal magit-buffer-revision-oid (tether-context-head-sha ctx)))
    (tether--diff-range-p ctx)))

(defun tether--diff-range-p (ctx)
  "Return non-nil if the current diff buffer's range is CTX's diff."
  (when-let* ((range (and (boundp 'magit-buffer-diff-range)
                          magit-buffer-diff-range))
              ((stringp range))
              ((string-match "\\`\\(.+?\\)\\(\\.\\.\\.?\\)\\(.+\\)\\'" range)))
    (let* ((three-dot (equal (match-string 2 range) "..."))
           (from (magit-rev-parse (match-string 1 range)))
           (to (magit-rev-parse (match-string 3 range))))
      (and (equal to (tether-context-head-sha ctx))
           (equal (if three-dot (magit-merge-base from to) from)
                  (tether-context-base-sha ctx))))))

(defun tether--diff-goto (path line)
  "In the current diff buffer, go to head LINE of PATH, or to PATH.
Return non-nil when LINE was found."
  (if-let* ((pos (and line (tether--diff-line-position path 'right line))))
      (progn (goto-char pos) t)
    (when-let* ((file (tether--diff-file-section path)))
      (goto-char (oref file start))
      nil)))

;;;; Where point is

(defun tether--location (ctx)
  "Return where point is in CTX's pull request, or nil.
The value is a plist with :path, and :line and :side when point is
on a line of the PR head (side `right') or only of its base (side
`left', from a removed line in a diff)."
  (cond
   ((derived-mode-p 'magit-diff-mode)
    (when-let* ((at (tether--diff-line-at (point))))
      (if (eq (plist-get at :kind) :removed)
          (list :path (plist-get at :path) :line (plist-get at :old)
                :side 'left)
        (list :path (plist-get at :path) :line (plist-get at :new)
              :side 'right))))
   ((or buffer-file-name tether--buffer-pullreq)
    (when-let* ((path (let ((default-directory (tether-context-toplevel ctx)))
                        (magit-file-relative-name))))
      (list :path path
            :line (tether-head-line ctx path (line-number-at-pos nil t))
            :side 'right)))))

;;;; Commands

(defun tether--show-diff (ctx &optional path line)
  "Show CTX's PR diff, at head LINE of PATH when given."
  (let ((default-directory (tether-context-toplevel ctx))
        (id (oref (tether-context-pullreq ctx) id)))
    ;; The options of the line map: with others (more context, ignored
    ;; whitespace) the buffer would show lines the forges refuse.
    (magit-diff-range (format "%s..%s" (tether-context-base-sha ctx)
                              (tether-context-head-sha ctx))
                      '("-M" "-U3" "--no-ext-diff"))
    (setq-local tether--buffer-pullreq id)
    (setq-local tether--buffer-commit (tether-context-commit ctx))
    (when path
      (unless (tether--diff-goto path line)
        (message "Tether: %s%s is not in the PR diff" path
                 (if line (format ":%d" line) ""))))))

;;;###autoload
(defun tether-diff (&optional pick)
  "Show the pull request diff, at the hunk of the current line.
With prefix argument PICK, choose the pull request."
  (interactive "P")
  (let* ((ctx (tether-context t pick))
         (at (and (not pick) (tether--location ctx))))
    (tether--show-diff ctx (plist-get at :path)
                       (and (eq (plist-get at :side) 'right)
                            (plist-get at :line)))))

(defconst tether--pullreq-url-regexp
  "\\`\\(https?://.+?\\)\\(?:/-\\)?/\\(?:pull\\|merge_requests\\)/\\([0-9]+\\)\\(?:[/?#].*\\)?\\'"
  "A pull or merge request URL: repository, number, maybe more.")

(defun tether--parse-pullreq-url (url)
  "Return (REPO-URL NUMBER ANCHOR) of the pull request URL, or nil.
ANCHOR is the part after #, if any."
  (when (string-match tether--pullreq-url-regexp url)
    (list (match-string 1 url)
          (string-to-number (match-string 2 url))
          (and (string-match "#\\(.+\\)\\'" url) (match-string 1 url)))))

(defun tether--url-default ()
  "Return a pull request URL from the kill ring or at point, or nil."
  (seq-find (lambda (text)
              (and (stringp text)
                   (tether--parse-pullreq-url (string-trim text))))
            (list (thing-at-point 'url t) (car kill-ring))))

(declare-function tether--open-thread "tether-comment" (ctx thread))

;;;###autoload
(defun tether-review-url (url)
  "Review the pull request at URL, as copied from the browser.
Shows its diff, from the local clone forge knows for the repository;
for a link to a comment, also shows its thread."
  (interactive
   (let ((default (tether--url-default)))
     (list (read-string (if default (format "Pull request URL (%s): " default)
                          "Pull request URL: ")
                        nil nil default))))
  (pcase-let* ((`(,repo-url ,number ,anchor)
                (or (tether--parse-pullreq-url (string-trim url))
                    (user-error "Tether: not a pull or merge request URL: %s" url)))
               (repo (or (forge-get-repository repo-url nil :tracked?)
                         (user-error "Tether: forge does not track %s; run \
`forge-add-repository' in its clone" repo-url)))
               (toplevel (or (forge-get-worktree repo)
                             (user-error "Tether: no clone of %s known to forge; \
open it once, then retry" repo-url)))
               (pullreq (or (forge-get-pullreq repo number)
                            (user-error "Tether: %s is not in forge's database; \
run `forge-pull'" (tether--label repo number))))
               (ctx (let ((default-directory toplevel))
                      (tether--resolve repo pullreq (file-name-as-directory toplevel)
                                       (equal (magit-get "branch"
                                                         (or (magit-get-current-branch) "")
                                                         "pullRequest")
                                              (number-to-string number))))))
    (tether--show-diff ctx)
    (when anchor
      (tether--wait-for-threads ctx)
      (when-let* ((thread (seq-find
                           (lambda (thread)
                             (seq-some (lambda (comment)
                                         (string-suffix-p
                                          (concat "#" anchor)
                                          (or (plist-get comment :url) "")))
                                       (tether-thread-comments thread)))
                           (tether-context-threads ctx))))
        ;; Not at load time: tether-comment requires this file.
        (require 'tether-comment)
        (tether--open-thread ctx thread)))))

;;;###autoload
(defun tether-review-commit (commit)
  "Review COMMIT alone: show its diff, where comments target it.
COMMIT is the one at point in magit, or read.  It must be one of the
pull request's commits.  Neither GitHub nor GitLab documents
comments on a commit that is not the PR head, so this relies on
behavior that may change."
  (interactive
   (list (or (magit-commit-at-point)
             (and (derived-mode-p 'magit-revision-mode) magit-buffer-revision)
             (magit-read-branch-or-commit "Review commit"))))
  (let* ((ctx (let ((tether--buffer-commit nil)) (tether-context t)))
         (default-directory (tether-context-toplevel ctx))
         (sha (or (magit-rev-parse (concat commit "^{commit}"))
                  (user-error "Tether: no commit %s" commit))))
    (unless (tether--pr-commit-p ctx sha)
      (user-error "Tether: %s is not one of the commits of %s" (substring sha 0 7)
                  (tether--label (tether-context-repo ctx)
                                 (oref (tether-context-pullreq ctx) number))))
    (tether--show-diff (tether--commit-context ctx sha))
    (message "Tether: reviewing commit %s of %s alone"
             (substring sha 0 7)
             (tether--label (tether-context-repo ctx)
                            (oref (tether-context-pullreq ctx) number)))))

;;;###autoload
(defun tether-review-pullreq ()
  "Choose a pull request and show its diff, whatever the branch.
Its commits are fetched when missing; from the diff, comment with
`tether-nitpick' and open files with `tether-visit-file'."
  (interactive)
  (tether--show-diff (tether-context t t)))

(defun tether--visit-blob (ctx path line)
  "Show PATH as in CTX's PR head, at LINE.
The buffer belongs to the PR whatever the branch, so comments work
from it."
  (let* ((toplevel (tether-context-toplevel ctx))
         (buffer (let ((default-directory toplevel))
                   (magit-find-file-noselect (tether-context-head-sha ctx)
                                             (expand-file-name path toplevel)))))
    (pop-to-buffer-same-window buffer)
    (setq-local tether--buffer-pullreq (oref (tether-context-pullreq ctx) id))
    (tether-mode 1)
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))))

;;;###autoload
(defun tether-visit-file (&optional head)
  "Visit the file at point's line of the pull request.
From a diff, visit the worktree file when the PR is checked out,
else the PR head's version.  With prefix argument HEAD, always visit
the PR head's version."
  (interactive "P")
  (let* ((ctx (tether-context t))
         (at (or (tether--location ctx)
                 (user-error "Tether: point is not on a line of a file")))
         (path (plist-get at :path))
         (line (if (eq (plist-get at :side) 'left)
                   ;; A removed line: the head line that follows it.
                   (when-let* ((l (gethash (plist-get at :line)
                                           (tether-file-map-by-old
                                            (tether-file-map ctx path)))))
                     (cdr (tether-line-code l)))
                 (plist-get at :line))))
    (tether--visit-path ctx path line head)))

(defun tether--visit-path (ctx path line &optional head)
  "Visit PATH at head LINE of CTX's pull request.
The worktree file when the PR is checked out, mapped through local
edits; else, or with HEAD non-nil, the PR head's version."
  (if (or head (not (tether-context-checked-out ctx)))
      (tether--visit-blob ctx path line)
    (find-file (expand-file-name path (tether-context-toplevel ctx)))
    (when line
      (let ((target (or (tether-buffer-line ctx path line) line)))
        (goto-char (point-min))
        (forward-line (1- target))))))

;;;###autoload
(defun tether-topic ()
  "Show forge's buffer of the pull request."
  (interactive)
  (forge-visit-pullreq (tether-context-pullreq (tether-context t))))

;;;###autoload
(defun tether-status ()
  "Show `magit-status' of the pull request's worktree."
  (interactive)
  (magit-status-setup-buffer (tether-context-toplevel (tether-context t))))

;;;; Browser

(cl-defgeneric tether--browse-url (repo ctx path line side)
  "Return the web page of CTX's PR diff on REPO at LINE of PATH.
SIDE is `right' or `left'.  Without LINE, the page of the file;
without PATH, the PR's diff.")

(defun tether--web-repo (repo)
  "Return the web address of REPO."
  (format "https://%s/%s/%s" (oref repo githost) (oref repo owner)
          (oref repo name)))

(cl-defmethod tether--browse-url ((repo forge-github-repository) ctx path line side)
  "Return the files page of the PR on GitHub REPO, at LINE of PATH.
CTX gives the PR; SIDE selects the R (head) or L (base) anchor."
  (concat (format "%s/pull/%d/files" (tether--web-repo repo)
                  (oref (tether-context-pullreq ctx) number))
          (when path
            (concat "#diff-" (secure-hash 'sha256 (encode-coding-string path 'utf-8))
                    (when line (format "%s%d" (if (eq side 'left) "L" "R") line))))))

(cl-defmethod tether--browse-url ((repo forge-gitlab-repository) ctx path line side)
  "Return the diffs page of the MR on GitLab REPO, at LINE of PATH.
CTX gives the MR.  GitLab's line anchor is the line_code, so it is
only available for lines in the diff; SIDE selects the side."
  (let* ((map (and path (tether-file-map ctx path)))
         (diff-line (and map line
                         (gethash line (if (eq side 'left)
                                           (tether-file-map-by-old map)
                                         (tether-file-map-by-new map))))))
    (concat (format "%s/-/merge_requests/%d/diffs" (tether--web-repo repo)
                    (oref (tether-context-pullreq ctx) number))
            (cond (diff-line (concat "#" (tether-gitlab--line-code path diff-line)))
                  (path (concat "#" (secure-hash 'sha1 (encode-coding-string
                                                        path 'utf-8))))))))

;;;###autoload
(defun tether-browse ()
  "Open the pull request's diff in the browser, at the current line."
  (interactive)
  (let* ((ctx (tether-context t))
         (at (tether--location ctx)))
    (browse-url (tether--browse-url (tether-context-repo ctx) ctx
                                    (plist-get at :path) (plist-get at :line)
                                    (plist-get at :side)))))

(provide 'tether-nav)
;;; tether-nav.el ends here
