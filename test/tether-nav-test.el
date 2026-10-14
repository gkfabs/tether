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
              ;; Fetched, so that resolving it makes no request.
              :threads-state 'done
              :review (list :drafts 0)
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

(ert-deftest tether-nav-test-parse-url ()
  "Pull and merge request URLs, with or without more after the number."
  (should (equal (tether--parse-pullreq-url
                  "https://gitlab.com/ayolab/sub/proj/-/merge_requests/540/diffs#note_12")
                 '("https://gitlab.com/ayolab/sub/proj" 540 "note_12")))
  (should (equal (tether--parse-pullreq-url "https://gitlab.com/a/b/-/merge_requests/7")
                 '("https://gitlab.com/a/b" 7 nil)))
  (should (equal (tether--parse-pullreq-url
                  "https://github.com/o/r/pull/12/files#discussion_r99")
                 '("https://github.com/o/r" 12 "discussion_r99")))
  (should (equal (tether--parse-pullreq-url "https://ghe.example.com/o/r/pull/3")
                 '("https://ghe.example.com/o/r" 3 nil)))
  (should-not (tether--parse-pullreq-url "https://github.com/o/r/issues/12")))

;;;; Single-commit review

(ert-deftest tether-nav-test-review-commit ()
  "A commit of the PR alone: its diff, and comments target it."
  (tether-nav-test-with (tether-test-gitlab-repo)
    (cl-letf (((symbol-function 'tether-context--1) (lambda (&rest _) ctx)))
      ;; Not a commit of the PR.
      (should-error (tether-review-commit main2) :type 'user-error)
      ;; pr1 changes a.el; the PR head also adds b.el.
      (tether-review-commit pr1)
      (should (equal tether--buffer-commit pr1))
      (let ((commit-ctx (tether-context t)))
        (should (equal (tether-context-head-sha commit-ctx) pr1))
        (should (equal (tether-context-base-sha commit-ctx) base))
        (should (tether--diff-range-of-pr-p commit-ctx))
        (should (string-match-p (format "commit %s only" (substring pr1 0 7))
                                (tether--describe commit-ctx)))
        (should (tether-file-map commit-ctx "a.el"))
        (should-not (tether-file-map commit-ctx "b.el"))
        (tether--diff-goto "a.el" 2)
        (tether-test-with-handler (lambda (_) '(:value ((id . "d") (notes ((id . 1))))))
          (let ((target (tether--diff-target commit-ctx)))
            (tether--post-comment repo commit-ctx target "On this commit"))
          (let ((payload (plist-get (car tether-test-requests) :payload)))
            (should (equal (alist-get 'commit_id payload) pr1))
            (should (equal (alist-get 'head_sha (alist-get 'position payload)) pr1))
            (should (equal (alist-get 'base_sha (alist-get 'position payload))
                           base))))))))

;;;; Threads in magit's commit buffer

(defun tether-nav-test--thread (id path line side &rest slots)
  "Return thread ID on LINE of PATH, SIDE, with SLOTS and two comments."
  (apply #'make-tether-thread
         :id id :path path :line line :side side
         :comments (list (list :id (concat id "a") :author "alice"
                               :body (concat id " first\r\nsecond line"))
                         (list :id (concat id "b") :author "bob" :body "Agreed"))
         slots))

(defun tether-nav-test--drawn ()
  "Return (LINE-TEXT THREAD-ID AFTER-STRING) of each thread drawn, sorted.
Found in the buffer, not in a variable, so that leftovers count."
  (sort (mapcar (lambda (overlay)
                  (save-excursion
                    (goto-char (overlay-start overlay))
                    (list (tether-nav-test--line-text)
                          (tether-thread-id (overlay-get overlay 'tether-thread))
                          (substring-no-properties
                           (or (overlay-get overlay 'after-string) "")))))
                (seq-filter (lambda (overlay) (overlay-get overlay 'tether-diff-thread))
                            (overlays-in (point-min) (point-max))))
        (lambda (a b) (string< (cadr a) (cadr b)))))

(defun tether-nav-test--drawn-ids ()
  "Return (LINE-TEXT THREAD-ID) of each thread drawn, sorted."
  (mapcar (lambda (x) (list (car x) (cadr x))) (tether-nav-test--drawn)))

(defmacro tether-nav-test-with-commit-threads (repo &rest body)
  "Run BODY like `tether-nav-test-with' REPO, with threads on PR 7.
The PR's threads:
- T1 on head line a.el:2, unchanged since pr1;
- T2 written on pr1's removed line a.el:2;
- T3 on b.el, which pr1 does not touch;
- T4 written on pr1, outdated on the PR head;
- T5 resolved, on a.el:2."
  (declare (indent 1) (debug t))
  `(tether-nav-test-with ,repo
     (setf (tether-context-threads ctx)
           (list (tether-nav-test--thread "T1" "a.el" 2 'right)
                 (tether-nav-test--thread "T2" "a.el" 2 'left
                                          :original-sha pr1 :original-line 2)
                 (tether-nav-test--thread "T3" "b.el" 1 'right)
                 (tether-nav-test--thread "T4" "a.el" nil 'right :outdated t
                                          :original-sha pr1 :original-line 2)
                 (tether-nav-test--thread "T5" "a.el" 2 'right :resolved t)))
     (let ((magit-refresh-buffer-hook (list #'tether--update-revision-threads)))
       ,@body)))

(ert-deftest tether-nav-test-commit-threads ()
  "Magit's commit buffer of a PR commit shows the threads on its lines."
  (tether-nav-test-with-commit-threads (tether-test-gitlab-repo)
    (set-buffer (magit-revision-setup-buffer pr1 nil nil))
    (should (equal tether--buffer-commit pr1))
    (should (equal tether--buffer-pullreq "pr-7"))
    (should (equal (tether-nav-test--drawn-ids)
                   '(("+TWO" "T1") ("-two" "T2") ("+TWO" "T4") ("+TWO" "T5"))))
    (let ((drawn (tether-nav-test--drawn)))
      ;; Open threads in full, without the CR of CRLF bodies.
      (should (equal (nth 2 (car drawn))
                     "\n  ▎ alice: T1 first\n  ▎   second line\n  ▎ bob: Agreed"))
      ;; Resolved ones as a summary.
      (should (equal (nth 2 (nth 3 drawn))
                     "  ◂ alice: T5 first (+1) [resolved]")))
    ;; Thread commands find the thread of the line.
    (should (tether--diff-goto "a.el" 2))
    (should (member (tether-thread-id (tether--thread-at-point (tether-context t)))
                    '("T1" "T4" "T5")))
    (let ((tether-show-resolved nil))
      (tether--update-revision-threads)
      (should-not (assoc "T5" (mapcar #'cdr (tether-nav-test--drawn-ids)))))
    (let ((tether-diff-thread-comments nil))
      (tether--update-revision-threads)
      (should (equal (nth 2 (car (tether-nav-test--drawn)))
                     "  ◂ alice: T1 first (+1)")))))

(ert-deftest tether-nav-test-commit-threads-next ()
  "Next and previous thread stay in magit's commit buffer."
  (tether-nav-test-with-commit-threads (tether-test-gitlab-repo)
    (let ((buffer (magit-revision-setup-buffer pr1 nil nil)))
      (set-buffer buffer)
      (goto-char (point-min))
      (tether-next-thread)
      (should (eq (current-buffer) buffer))
      (should (equal (tether-nav-test--line-text) "-two"))
      ;; T1, T4 and T5 share a line: one stop.
      (tether-next-thread)
      (should (equal (tether-nav-test--line-text) "+TWO"))
      (should-error (tether-next-thread) :type 'user-error)
      (tether-previous-thread)
      (should (equal (tether-nav-test--line-text) "-two"))
      (should-error (tether-previous-thread) :type 'user-error)
      ;; A collapsed file is expanded.
      (goto-char (point-min))
      (magit-section-hide (seq-find (lambda (section)
                                      (magit-section-match 'file section))
                                    (oref magit-root-section children)))
      (tether-next-thread)
      (should-not (invisible-p (point)))
      (should (equal (tether-nav-test--line-text) "-two"))
      (should (eq (current-buffer) buffer)))))

(ert-deftest tether-nav-test-commit-threads-once ()
  "Each thread is drawn once: buffer shown again, refreshed, threads again."
  (tether-nav-test-with-commit-threads (tether-test-gitlab-repo)
    (set-buffer (magit-revision-setup-buffer pr1 nil nil))
    (should (= (length (tether-nav-test--drawn)) 4))
    ;; Showing it again runs the major mode again, which resets the
    ;; buffer's variables.
    (set-buffer (magit-revision-setup-buffer pr1 nil nil))
    (magit-refresh-buffer)
    (magit-refresh-buffer)
    (run-hook-with-args 'tether-threads-functions ctx)
    (run-hook-with-args 'tether-threads-functions ctx)
    (should (= (length (tether-nav-test--drawn)) 4))))

(ert-deftest tether-nav-test-commit-threads-other-commit ()
  "A commit outside the PR: no threads, and no ties to the PR."
  (tether-nav-test-with-commit-threads (tether-test-gitlab-repo)
    (set-buffer (magit-revision-setup-buffer pr1 nil nil))
    (should (tether-nav-test--drawn))
    ;; The same buffer, moved to another commit.
    (set-buffer (magit-revision-setup-buffer main2 nil nil))
    (should-not tether--buffer-pullreq)
    (should-not tether--buffer-commit)
    (should-not (tether-nav-test--drawn))))

(ert-deftest tether-nav-test-commit-threads-comment ()
  "A comment from magit's commit buffer targets that commit."
  (tether-nav-test-with-commit-threads (tether-test-gitlab-repo)
    (set-buffer (magit-revision-setup-buffer pr1 nil nil))
    (should (tether--diff-goto "a.el" 2))
    (let ((commit-ctx (tether-context t)))
      (should (equal (tether-context-head-sha commit-ctx) pr1))
      (should (tether--diff-range-of-pr-p commit-ctx))
      (tether-test-with-handler (lambda (_) '(:value ((id . "d") (notes ((id . 1))))))
        (tether--post-comment repo commit-ctx (tether--diff-target commit-ctx)
                              "On this commit")
        (let ((payload (plist-get (car tether-test-requests) :payload)))
          (should (equal (alist-get 'commit_id payload) pr1))
          (should (equal (alist-get 'new_line (alist-get 'position payload)) 2)))))))

(ert-deftest tether-nav-test-commit-threads-refetch ()
  "Threads fetched again from a commit's context update its PR and buffer."
  (tether-nav-test-with-commit-threads (tether-test-gitlab-repo)
    (set-buffer (magit-revision-setup-buffer pr1 nil nil))
    (cl-letf (((symbol-function 'tether--fetch-threads)
               (lambda (_repo _ctx callback)
                 (funcall callback
                          (list :ok t
                                :threads (list (tether-nav-test--thread
                                                "T9" "a.el" 2 'right)))))))
      (tether-fetch-threads (tether-context t)))
    (should (equal (mapcar #'tether-thread-id (tether-context-threads ctx)) '("T9")))
    (should (equal (tether-nav-test--drawn-ids) '(("+TWO" "T9"))))))

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
        (should (memq #'tether--update-revision-threads
                      (default-value 'magit-refresh-buffer-hook)))
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
