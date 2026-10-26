;;; tether-lines-test.el --- Tests for tether-lines  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Line maps and buffer-to-head mapping against real git repositories.

;;; Code:

(require 'ert)
(require 'tether-lines)
(require 'tether-test-helpers)

(defun tether-lines-test--context (base head)
  "Return a context for the diff BASE..HEAD of the current worktree."
  (tether-context--make :toplevel (file-name-as-directory default-directory)
                        :base-sha base :head-sha head
                        :maps (make-hash-table :test #'equal)))

(defmacro tether-lines-test-at-head (&rest body)
  "Run BODY in the test clone with the PR head checked out.
In the PR, a.el changes line 2 (\"two\" -> \"TWO\") and b.el is added."
  (declare (indent 0) (debug t))
  `(tether-test-with-repos
     (tether-test-git "fetch" "-q" "origin" "refs/pull/7/head")
     (tether-test-git "checkout" "-q" head)
     (let ((ctx (tether-lines-test--context base head)))
       ,@body)))

(defmacro tether-lines-test-visiting (file &rest body)
  "Run BODY in a buffer visiting FILE, killed afterwards."
  (declare (indent 1) (debug t))
  `(let ((buffer (find-file-noselect (expand-file-name ,file))))
     (unwind-protect
         (with-current-buffer buffer ,@body)
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer))))

(defun tether-lines-test--kind (ctx path line)
  "Return the kind of buffer LINE of PATH in CTX, or nil."
  (when-let* ((l (tether-line-at ctx path line)))
    (tether-line-kind l)))

(ert-deftest tether-lines-test-file-maps ()
  (tether-lines-test-at-head
    (should (eq (tether-file-map-status (tether-file-map ctx "a.el")) :modified))
    (should (eq (tether-file-map-status (tether-file-map ctx "b.el")) :added))
    ;; c.el changed on main, not in the PR.
    (should-not (tether-file-map ctx "c.el"))
    (should (equal (tether--path-map ctx)
                   '(("a.el" "a.el" . :modified) ("b.el" "b.el" . :added))))))

(ert-deftest tether-lines-test-unchanged-worktree ()
  (tether-lines-test-at-head
    (tether-lines-test-visiting "a.el"
      (should (eq (tether-lines-test--kind ctx "a.el" 1) :context))
      (should (eq (tether-lines-test--kind ctx "a.el" 2) :added))
      (should (equal (tether-commentable-lines ctx "a.el")
                     '((1 . :context) (2 . :added) (3 . :context)))))))

(ert-deftest tether-lines-test-uncommitted-edit ()
  "A saved line inserted above shifts the mapping by one."
  (tether-lines-test-at-head
    (with-temp-file "a.el" (insert "zero\none\nTWO\nthree\n"))
    (tether-lines-test-visiting "a.el"
      (should (= (tether-head-line ctx "a.el" 3) 2))
      (should (eq (tether-lines-test--kind ctx "a.el" 3) :added))
      (should-not (tether-head-line ctx "a.el" 1))
      (should (= (tether-buffer-line ctx "a.el" 2) 3))
      (should (equal (tether-commentable-lines ctx "a.el")
                     '((2 . :context) (3 . :added) (4 . :context))))
      (let ((err (should-error (tether-region-at ctx "a.el" 1 2)
                               :type 'user-error)))
        (should (string-match-p "a.el line 1 changed locally since the PR head"
                                (cadr err)))))))

(ert-deftest tether-lines-test-unsaved-edit ()
  "Unsaved buffer changes count too."
  (tether-lines-test-at-head
    (tether-lines-test-visiting "a.el"
      (goto-char (point-min))
      (insert "unsaved\n")
      (should (buffer-modified-p))
      (should (= (tether-head-line ctx "a.el" 3) 2))
      (let ((region (tether-region-at ctx "a.el" 2 4)))
        (should (equal (list (tether-line-new (car region))
                             (tether-line-new (cdr region)))
                       '(1 3))))
      ;; A local insertion inside a region makes it span different lines.
      (goto-char (point-max))
      (forward-line -1)
      (insert "inside\n")
      (let ((err (should-error (tether-region-at ctx "a.el" 2 5)
                               :type 'user-error)))
        (should (string-match-p "lines 2-5 changed locally" (cadr err)))))))

(ert-deftest tether-lines-test-not-in-diff ()
  (tether-lines-test-at-head
    (tether-lines-test-visiting "c.el"
      (let ((err (should-error (tether-region-at ctx "c.el" 1 1)
                               :type 'user-error)))
        (should (string-match-p "c.el: the file is not changed in the pull request"
                                (cadr err)))))))

(ert-deftest tether-lines-test-outside-hunks ()
  "GitLab takes any line of a changed file; GitHub only those of the hunks."
  (tether-test-with-repos
    (let* ((lines (mapconcat #'number-to-string (number-sequence 1 20) "\n"))
           (base (tether-test-commit "long.el" (concat lines "\n")))
           (head (tether-test-commit "long.el" (concat (string-replace "\n10\n" "\nTEN\n" lines)
                                                       "\n")))
           (ctx (tether-lines-test--context base head)))
      (tether-lines-test-visiting "long.el"
        (setf (tether-context-repo ctx) (tether-test-github-repo))
        (should (string-match-p "line 1 is not in the pull request diff"
                                (cadr (should-error (tether-region-at ctx "long.el" 1 2)
                                                    :type 'user-error))))
        (setf (tether-context-repo ctx) (tether-test-gitlab-repo))
        (let ((region (tether-region-at ctx "long.el" 1 12)))
          (should (equal (tether-line-old (car region)) 1))
          (should (equal (tether-line-kind (cdr region)) :context))
          (should (equal (tether-line-new (cdr region)) 12)))
        (should (eq (tether-line-kind (tether-line-at ctx "long.el" 18)) :context))))))

(ert-deftest tether-lines-test-blob-buffer ()
  "A tether blob buffer shows the head: lines map as they are."
  (tether-lines-test-at-head
    (with-temp-file "a.el" (insert "local\nchanges\neverywhere\n"))
    (with-temp-buffer
      (setq tether--buffer-pullreq "pr-7")
      (should (= (tether-head-line ctx "a.el" 2) 2))
      (should (eq (tether-lines-test--kind ctx "a.el" 2) :added)))))

(ert-deftest tether-lines-test-rename ()
  "A renamed file is diffed with both paths, so the rename is kept."
  (tether-lines-test-at-head
    (tether-test-git "mv" "a.el" "moved.el")
    (with-temp-file "moved.el" (insert "one\nTWO\nthree\nfour\n"))
    (tether-test-git "add" "moved.el")
    (tether-test-git "commit" "-q" "-m" "Rename")
    (let* ((renamed (tether-test-git "rev-parse" "HEAD"))
           (ctx (tether-lines-test--context head renamed))
           (map (tether-file-map ctx "moved.el")))
      (should (eq (tether-file-map-status map) :renamed))
      (should (equal (tether-file-map-old-path map) "a.el"))
      (should (eq (tether-line-kind (tether-file-map-line map 4)) :added)))))

(ert-deftest tether-lines-test-user-git-config ()
  "The user's diff configuration does not change the line map."
  (tether-lines-test-at-head
    (dolist (setting '(("diff.noprefix" "true") ("diff.mnemonicPrefix" "true")
                       ("color.ui" "always") ("color.diff" "always")
                       ("diff.algorithm" "histogram") ("diff.relative" "true")
                       ("core.quotePath" "false")))
      (apply #'tether-test-git "config" setting))
    (tether-lines-test-visiting "a.el"
      (should (eq (tether-lines-test--kind ctx "a.el" 2) :added))
      (should (equal (tether-file-map-old-path (tether-file-map ctx "a.el"))
                     "a.el")))))

(provide 'tether-lines-test)
;;; tether-lines-test.el ends here
