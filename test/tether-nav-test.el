;;; tether-nav-test.el --- Tests for navigation and tether-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The PR diff buffer, jumps between views, browser URLs, commenting
;; from the diff, and `tether-mode', on real git repositories with
;; forge's database and the API stubbed.
;;
;; In the test PR, a.el changes line 2 ("two" -> "TWO") and b.el is
;; added.

;;; Code:

(require 'ert)
(require 'tether)
(require 'tether-test-helpers)

(defun tether-nav-test--context (repo checked-out base head)
  "Return a context of PR 7 of REPO, diff BASE..HEAD, for the worktree.
CHECKED-OUT says whether the PR branch is the current one."
  (let ((ctx (tether-context--make
              :repo repo :pullreq (tether-test-pullreq)
              :toplevel (file-name-as-directory default-directory)
              :base-sha base :start-sha base :head-sha head
              :maps (make-hash-table :test #'equal)
              :checked-out checked-out :head-state '(same 0)
              :local-head (magit-rev-parse "HEAD")
              :local-branch (magit-get-current-branch)
              :generation 0 :fetched-at (float-time))))
    (puthash (cons (tether-context-toplevel ctx) 7) ctx tether--contexts)
    ctx))

(defmacro tether-nav-test-with (repo &rest body)
  "Run BODY in the test clone, PR commits fetched, `ctx' bound.
REPO is the forge repository.  The current buffer's PR is found
through `tether--buffer-pullreq' or the stubbed branch lookup."
  (declare (indent 1) (debug t))
  `(tether-test-with-repos
     (tether-test-git "fetch" "-q" "origin" "refs/pull/7/head")
     (let* ((tether--contexts (make-hash-table :test #'equal))
            (tether--branch-prs (make-hash-table :test #'equal))
            (repo ,repo)
            (pullreq (tether-test-pullreq)))
       (cl-letf (((symbol-function 'tether--repository) (lambda (_) repo))
                 ((symbol-function 'tether--repository-maybe) (lambda (_) repo))
                 ((symbol-function 'forge-get-pullreq) (lambda (&rest _) pullreq))
                 ((symbol-function 'tether--branch-pullreq)
                  (lambda (branch) (and (equal branch "feature") pullreq))))
         (unwind-protect
             (let ((ctx (tether-nav-test--context repo nil base head)))
               ,@body)
           (dolist (buffer (buffer-list))
             (when (with-current-buffer buffer
                     (string-prefix-p root (expand-file-name default-directory)))
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))))))

(defun tether-nav-test--line-text ()
  "Return the text of the current line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

;;;; The PR diff

(ert-deftest tether-nav-test-diff-at-hunk ()
  "The diff opens at the head line, and knows what each line is."
  (tether-nav-test-with (tether-test-github-repo)
    (tether--show-diff ctx "a.el" 2)
    (should (derived-mode-p 'magit-diff-mode))
    (should (equal tether--buffer-pullreq "pr-7"))
    (should (tether--diff-range-of-pr-p ctx))
    (should (equal (tether-nav-test--line-text) "+TWO"))
    (let ((at (tether--diff-line-at (point))))
      (should (equal (list (plist-get at :path) (plist-get at :kind)
                           (plist-get at :old) (plist-get at :new))
                     '("a.el" :added nil 2))))
    (forward-line -1)
    (let ((at (tether--diff-line-at (point))))
      (should (equal (tether-nav-test--line-text) "-two"))
      (should (equal (list (plist-get at :kind) (plist-get at :old)
                           (plist-get at :new))
                     '(:removed 2 nil))))
    (should (equal (tether--location ctx) '(:path "a.el" :line 2 :side left)))))

(ert-deftest tether-nav-test-other-diff ()
  "Commenting refuses a diff that is not the PR's."
  (tether-nav-test-with (tether-test-github-repo)
    (magit-diff-range (format "%s..%s" main2 head))
    (should-not (tether--diff-range-of-pr-p ctx))
    (should-error (tether--diff-target ctx) :type 'user-error)))

(ert-deftest tether-nav-test-comment-removed-line ()
  "From the PR diff, a removed line is commented on the base side."
  (tether-nav-test-with (tether-test-gitlab-repo)
    (tether--show-diff ctx "a.el" 2)
    (forward-line -1)
    (let ((target (tether--diff-target ctx)))
      (should (eq (plist-get target :side) 'left))
      (should (equal (tether--target-label target) "a.el:2 (removed)"))
      (let ((position (tether-gitlab--position ctx target)))
        (should (equal (alist-get 'old_line position) 2))
        (should-not (alist-get 'new_line position))))
    ;; A region from the removed line to the added one mixes sides.
    (let ((transient-mark-mode t))
      (push-mark (point) t t)
      (forward-line 1)
      (end-of-line)
      (should-error (tether--diff-target ctx) :type 'user-error))))

;;;; Jumps

(ert-deftest tether-nav-test-visit-file-not-checked-out ()
  "Not on the PR branch: the PR head's version opens, tied to the PR."
  (tether-nav-test-with (tether-test-github-repo)
    (tether--show-diff ctx "a.el" 2)
    (cl-letf (((symbol-function 'tether-context) (lambda (&rest _) ctx)))
      (tether-visit-file))
    (should (equal (tether-nav-test--line-text) "TWO"))
    (should (equal tether--buffer-pullreq "pr-7"))
    (should tether-mode)
    (should-not buffer-file-name)))

(ert-deftest tether-nav-test-visit-file-checked-out ()
  "On the PR branch: the worktree file, at the line moved by local edits."
  (tether-nav-test-with (tether-test-github-repo)
    (tether-test-git "checkout" "-q" "-b" "feature" head)
    (with-temp-file "a.el" (insert "zero\none\nTWO\nthree\n"))
    (let ((ctx (tether-nav-test--context repo t base head)))
      (tether--show-diff ctx "a.el" 2)
      (cl-letf (((symbol-function 'tether-context) (lambda (&rest _) ctx)))
        (tether-visit-file))
      (should (equal (file-name-nondirectory buffer-file-name) "a.el"))
      (should (= (line-number-at-pos) 3))
      (should (equal (tether-nav-test--line-text) "TWO")))))

(ert-deftest tether-nav-test-browse-urls ()
  (tether-nav-test-with (tether-test-github-repo)
    (should (equal (tether--browse-url repo ctx "a.el" 2 'right)
                   "https://github.com/o/r/pull/7/files#diff-44fc29f1d4a972e6c8891d29a93cfd5eb62f53b30cd3522f39b9dde940d49507R2"))
    (should (equal (tether--browse-url repo ctx nil nil nil)
                   "https://github.com/o/r/pull/7/files"))
    (let* ((gitlab (tether-test-gitlab-repo))
           (ctx (tether-nav-test--context gitlab nil base head)))
      ;; GitLab anchors lines by line_code: added line 2 follows removed
      ;; old line 2, so its old counter is 3.
      (should (equal (tether--browse-url gitlab ctx "a.el" 2 'right)
                     "https://gitlab.com/g/sub/p/-/merge_requests/7/diffs#71766e750b2dfb36f9320b14c24813ff49022960_3_2"))
      (should (equal (tether--browse-url gitlab ctx "a.el" 2 'left)
                     "https://gitlab.com/g/sub/p/-/merge_requests/7/diffs#71766e750b2dfb36f9320b14c24813ff49022960_2_2"))
      ;; Not in the diff: the file's anchor.
      (should (equal (tether--browse-url gitlab ctx "a.el" 30 'right)
                     "https://gitlab.com/g/sub/p/-/merge_requests/7/diffs#71766e750b2dfb36f9320b14c24813ff49022960")))))

;;;; tether-mode

(ert-deftest tether-nav-test-mode ()
  "The mode line names the PR, warns when behind, and the fringe marks."
  (tether-nav-test-with (tether-test-github-repo)
    (tether-test-git "checkout" "-q" "-b" "feature" pr1)
    (tether-test-git "config" "branch.feature.pullRequest" "7")
    (let ((buffer (find-file-noselect (expand-file-name "a.el"))))
      (with-current-buffer buffer
        (tether--turn-on)
        (should tether-mode)
        (should (equal tether--lighter " #7"))
        (should-not tether--fringe-overlays)
        ;; Once resolved: one commit behind the PR head, and marks.
        (let ((ctx (tether-nav-test--context repo t base head)))
          (setf (tether-context-head-state ctx) '(behind 1))
          (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
            (run-hook-with-args 'tether-context-functions ctx)))
        (should (equal tether--lighter " #7⚠"))
        (should (= (length tether--fringe-overlays) 3))))))

(ert-deftest tether-nav-test-mode-follows-branch ()
  "Checking out a branch without a PR turns the mode off."
  (tether-nav-test-with (tether-test-github-repo)
    (tether-test-git "checkout" "-q" "-b" "feature" head)
    (let ((buffer (find-file-noselect (expand-file-name "a.el"))))
      (with-current-buffer buffer
        (tether--turn-on)
        (should tether-mode)
        (tether-test-git "checkout" "-q" "main")
        (tether--magit-refreshed)
        (should-not tether-mode)))))

(ert-deftest tether-nav-test-menu ()
  (should (commandp 'tether))
  (with-temp-buffer
    (should (equal (tether--menu-description) "Tether"))))

(provide 'tether-nav-test)
;;; tether-nav-test.el ends here
