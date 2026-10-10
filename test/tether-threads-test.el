;;; tether-threads-test.el --- Tests for review threads  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fetching and normalizing threads on both forges, placing GitLab
;; discussions made on another commit, showing threads in files and
;; in the thread list, and going from thread to thread.
;;
;; In the test PR, a.el changes line 2 ("two" -> "TWO") and b.el is
;; added.

;;; Code:

(require 'ert)
(require 'tether)
(require 'tether-test-helpers)

(cl-defun tether-threads-test--thread (path line &key (author "alice")
                                            (body "Off by one?") resolved
                                            outdated head-sha (side 'right)
                                            (id (format "%s:%s" path line)))
  "Return a thread on LINE of PATH."
  (make-tether-thread :id id :path path :line line :side side
                      :head-sha head-sha :original-line line
                      :resolved resolved :outdated outdated
                      :comments (list (list :author author :body body
                                            :url "https://x/1"))))

(defun tether-threads-test--context (repo base head threads)
  "Return the cached context of PR 7 of REPO on BASE..HEAD with THREADS."
  (let ((ctx (tether-context--make
              :repo repo :pullreq (tether-test-pullreq)
              :toplevel (file-name-as-directory default-directory)
              :base-sha base :start-sha base :head-sha head
              :maps (make-hash-table :test #'equal)
              :checked-out t :head-state '(same 0)
              :local-head (magit-rev-parse "HEAD")
              :local-branch (magit-get-current-branch)
              :threads threads :threads-state 'done
              :generation 0 :fetched-at (float-time))))
    (puthash (cons (tether-context-toplevel ctx) 7) ctx tether--contexts)
    ctx))

(defmacro tether-threads-test-with (&rest body)
  "Run BODY on the PR branch of the test clone, `repo' and `pullreq' bound."
  (declare (indent 0) (debug t))
  `(tether-test-with-repos
     (tether-test-git "fetch" "-q" "origin" "refs/pull/7/head")
     (tether-test-git "checkout" "-q" "-b" "feature" head)
     (tether-test-git "config" "branch.feature.pullRequest" "7")
     (let* ((tether--contexts (make-hash-table :test #'equal))
            (tether--branch-prs (make-hash-table :test #'equal))
            (repo (tether-test-github-repo))
            (pullreq (tether-test-pullreq)))
       (cl-letf (((symbol-function 'tether--repository) (lambda (_) repo))
                 ((symbol-function 'tether--repository-maybe) (lambda (_) repo))
                 ((symbol-function 'forge-get-pullreq) (lambda (&rest _) pullreq))
                 ((symbol-function 'tether--branch-pullreq) (lambda (_) pullreq)))
         (unwind-protect (progn ,@body)
           (dolist (buffer (buffer-list))
             (when (with-current-buffer buffer
                     (string-prefix-p root (expand-file-name default-directory)))
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))))))

;;;; Normalizing

(ert-deftest tether-threads-test-github-thread ()
  (let ((thread (tether-github--thread
                 '((id . "T1") (isResolved . t) (isOutdated) (path . "a.el")
                   (line . 2) (startLine . 1) (originalLine . 2)
                   (diffSide . "RIGHT")
                   (comments ((body . "Why?") (createdAt . "2026-10-01")
                              (url . "https://github.com/o/r/pull/7#r1")
                              (diffHunk . "@@ -1,3 +1,3 @@")
                              (author (login . "alice"))
                              (originalCommit (oid . "abc1234")))
                             ((body . "Because") (author (login . "bob"))))))))
    (should (equal (tether-thread-id thread) "T1"))
    (should (equal (list (tether-thread-path thread) (tether-thread-line thread)
                         (tether-thread-start-line thread) (tether-thread-side thread))
                   '("a.el" 2 1 right)))
    (should (tether-thread-resolved thread))
    (should-not (tether-thread-outdated thread))
    (should (equal (tether-thread-original-sha thread) "abc1234"))
    (should (equal (mapcar (lambda (c) (plist-get c :author))
                           (tether-thread-comments thread))
                   '("alice" "bob")))))

(ert-deftest tether-threads-test-gitlab-thread ()
  "Only discussions on a line of the diff; system notes are dropped."
  (let* ((repo (tether-test-gitlab-repo))
         (thread (tether-gitlab--thread
                  repo 7
                  '((id . "d1")
                    (notes ((id . 10) (type . "DiffNote") (body . "Hm")
                            (author (username . "carol")) (resolved . t)
                            (position (head_sha . "H") (new_path . "a.el")
                                      (old_path . "a.el") (old_line . 2)
                                      (new_line)))
                           ((id . 11) (system . t) (body . "changed the line"))
                           ((id . 12) (type . "DiffNote") (body . "Ok")
                            (author (username . "dave"))))))))
    (should (equal (list (tether-thread-path thread) (tether-thread-line thread)
                         (tether-thread-side thread) (tether-thread-head-sha thread))
                   '("a.el" 2 left "H")))
    (should (tether-thread-resolved thread))
    (should (equal (plist-get (car (tether-thread-comments thread)) :url)
                   "https://gitlab.com/g/sub/p/-/merge_requests/7#note_10"))
    (should (= (length (tether-thread-comments thread)) 2))
    (should-not (tether-gitlab--thread
                 repo 7 '((id . "d2") (notes ((id . 13) (type . "DiscussionNote")
                                              (body . "General"))))))))

(ert-deftest tether-threads-test-fetch ()
  "Fetched threads are stored and announced; a stale context ignores them."
  (tether-threads-test-with
    (let ((ctx (tether-threads-test--context repo base head nil))
          (announced nil))
      (setf (tether-context-threads-state ctx) nil)
      (tether-test-with-handler
          (let ((answer
                 '((repository
                    (pullRequest
                     (reviewThreads
                      ((id . "T1") (path . "a.el") (line . 2)
                       (diffSide . "RIGHT")
                       (comments ((body . "x") (author (login . "a")))))))))))
            (lambda (_) (list :value answer)))
        (let ((tether-threads-functions (list (lambda (c) (setq announced c)))))
          (tether-fetch-threads ctx)
          (should (eq announced ctx))
          (should (eq (tether-context-threads-state ctx) 'done))
          (should (equal (mapcar #'tether-thread-id (tether-context-threads ctx))
                         '("T1")))
          ;; Replaced meanwhile: the answer is dropped.
          (setq announced nil)
          (tether-threads-test--context repo base head nil)
          (tether-fetch-threads ctx)
          (should-not announced))))))

(ert-deftest tether-threads-test-gitlab-other-commit ()
  "A discussion on another commit is moved to the head, or outdated."
  (tether-threads-test-with
    (let ((ctx (tether-threads-test--context repo base head nil))
          (tether--logged nil))
      ;; Written on pr1: a.el did not change since, line 2 stays line 2.
      (let ((thread (tether--place-thread
                     ctx (tether-threads-test--thread "a.el" 2 :head-sha pr1))))
        (should (equal (tether-thread-line thread) 2))
        (should-not (tether-thread-outdated thread)))
      ;; Written on base: line 2 changed since, so it is outdated.
      (let ((thread (tether--place-thread
                     ctx (tether-threads-test--thread "a.el" 2 :head-sha base))))
        (should-not (tether-thread-line thread))
        (should (tether-thread-outdated thread)))
      (with-current-buffer "*tether-log*"
        (should (string-match-p "GitLab discussion a.el:2 is on" (buffer-string))))
      (kill-buffer "*tether-log*"))))

;;;; Showing

(ert-deftest tether-threads-test-file-buffer ()
  "Open threads show on their line; resolved ones only when asked."
  (tether-threads-test-with
    (tether-threads-test--context
     repo base head
     (list (tether-threads-test--thread "a.el" 2)
           (tether-threads-test--thread "a.el" 3 :resolved t :body "Fine")
           (tether-threads-test--thread "b.el" 1)))
    (with-current-buffer (find-file-noselect (expand-file-name "a.el"))
      (tether-mode 1)
      (should (equal tether--lighter " #7·2"))
      (should (= (length tether--thread-overlays) 1))
      (let ((overlay (car tether--thread-overlays)))
        (should (= (line-number-at-pos (overlay-start overlay)) 2))
        (should (string-match-p "◂ alice: Off by one\\?"
                                (overlay-get overlay 'after-string))))
      (let ((tether-show-resolved t))
        (tether--refresh-buffer)
        (should (= (length tether--thread-overlays) 2))))))

(ert-deftest tether-threads-test-resolved-before-mode ()
  "A PR resolved before any buffer was in `tether-mode' still gets its threads.
As when `tether-show-context' runs from magit-status first."
  (should (memq #'tether--context-resolved tether-context-functions))
  (tether-threads-test-with
    (let ((ctx (tether-threads-test--context repo base head nil)))
      (setf (tether-context-threads-state ctx) nil)
      (tether-test-with-handler
          (let ((answer
                 '((repository
                    (pullRequest
                     (reviewThreads
                      ((id . "T1") (path . "a.el") (line . 2)
                       (diffSide . "RIGHT")
                       (comments ((body . "Hm") (author (login . "a")))))))))))
            (lambda (_) (list :value answer)))
        (with-current-buffer (find-file-noselect (expand-file-name "a.el"))
          (tether-mode 1)
          (should (eq (tether-context-threads-state ctx) 'done))
          (should (equal tether--lighter " #7·1"))
          (should (= (length tether--thread-overlays) 1)))))))

(ert-deftest tether-threads-test-magit-status ()
  "magit-status shows the pull request and its open threads too."
  (tether-threads-test-with
    (tether-threads-test--context
     repo base head (list (tether-threads-test--thread "a.el" 2)
                          (tether-threads-test--thread "a.el" 3)))
    (let ((global-tether-mode t))
      (with-current-buffer (magit-status-setup-buffer default-directory)
        (tether--turn-on)
        (should tether-mode)
        (should (equal tether--lighter " #7·2"))
        (should-not tether--thread-overlays)))))

(ert-deftest tether-threads-test-describe ()
  "`tether-show-context' says how many threads are open."
  (tether-threads-test-with
    (let ((ctx (tether-threads-test--context
                repo base head
                (list (tether-threads-test--thread "a.el" 2)
                      (tether-threads-test--thread "a.el" 3 :resolved t)))))
      (should (string-suffix-p "; checked out; 1 open thread" (tether--describe ctx)))
      (setf (tether-context-threads-state ctx) 'loading)
      (should (string-suffix-p "; threads still fetching" (tether--describe ctx))))))

(ert-deftest tether-threads-test-show-thread ()
  "The thread of the current line, every comment, in a buffer below."
  (tether-threads-test-with
    (let ((thread (tether-threads-test--thread "a.el" 2 :body "A long first comment")))
      (setf (tether-thread-comments thread)
            (append (tether-thread-comments thread)
                    (list (list :author "bob" :body "Line one\nline two"))))
      (tether-threads-test--context repo base head (list thread)))
    (set-buffer (find-file-noselect (expand-file-name "a.el")))
    (goto-char (point-min))
    (should-error (tether-show-thread) :type 'user-error)
    (forward-line 1)
    (tether-show-thread)
    (with-current-buffer "*tether-thread*"
      (should (derived-mode-p 'tether-thread-mode))
      (let ((text (buffer-string)))
        (should (string-match-p "^#7 a.el:2  2 comments" text))
        (should (string-match-p "A long first comment" text))
        (should (string-match-p "bob" text))
        (should (string-match-p "line two" text))))
    (kill-buffer "*tether-thread*")))

(ert-deftest tether-threads-test-next-previous ()
  (tether-threads-test-with
    (tether-threads-test--context
     repo base head
     (list (tether-threads-test--thread "a.el" 1)
           (tether-threads-test--thread "a.el" 3)
           (tether-threads-test--thread "b.el" 1)))
    (set-buffer (find-file-noselect (expand-file-name "a.el")))
    (goto-char (point-min))
    (forward-line 1)
    (tether-next-thread)
    (should (= (line-number-at-pos) 3))
    (tether-next-thread)
    (should (equal (file-name-nondirectory buffer-file-name) "b.el"))
    (should-error (tether-next-thread) :type 'user-error)
    (tether-previous-thread)
    (should (equal (file-name-nondirectory buffer-file-name) "a.el"))
    (should (= (line-number-at-pos) 3))
    (let ((tether-cycle-across-files nil))
      (tether-previous-thread)
      (should (= (line-number-at-pos) 1))
      (should-error (tether-previous-thread) :type 'user-error))))

(ert-deftest tether-threads-test-list ()
  "The list groups by file, keeps outdated threads apart, and visits."
  (tether-threads-test-with
    (let ((ctx (tether-threads-test--context
                repo base head
                (list (tether-threads-test--thread "a.el" 2)
                      (tether-threads-test--thread "a.el" 3 :resolved t)
                      (let ((thread (tether-threads-test--thread
                                     "a.el" nil :outdated t :body "Old")))
                        (setf (tether-thread-diff-hunk thread) "@@ -1 +1 @@\n-x\n+y")
                        thread)))))
      (set-buffer (with-current-buffer (find-file-noselect (expand-file-name "a.el"))
                    (goto-char (point-min))
                    (forward-line 1)
                    (tether-threads)))
      (should (derived-mode-p 'tether-threads-mode))
      (should (equal (buffer-name) "*tether-threads #7*"))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "#7 Fix parsing: 1 open, 1 resolved, 1 outdated" text))
        (should (string-match-p "^a.el (2)$" text))
        (should (string-match-p "^Outdated (1)$" text)))
      ;; Point is on the thread of line 2, shown.
      (should (equal (tether-thread-line (tether-threads--at-point)) 2))
      (should (string-match-p "Off by one"
                              (buffer-substring-no-properties (point) (point-max))))
      (tether-threads-visit)
      (should (equal (file-name-nondirectory buffer-file-name) "a.el"))
      (should (= (line-number-at-pos) 2))
      (ignore ctx))))

(provide 'tether-threads-test)
;;; tether-threads-test.el ends here
