;;; tether-comment-test.el --- Tests for the comment editor  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `tether-nitpick', `tether-suggest' and `tether-nitpick-file' end to
;; end: from a file buffer of a git repository to the request posted,
;; through the editor, with forge's database and the API stubbed.
;;
;; In the test PR, a.el changes line 2 ("two" -> "TWO").

;;; Code:

(require 'ert)
(require 'tether)
(require 'tether-test-helpers)

(defvar tether-comment-test--post-error nil
  "When non-nil, the error the stubbed API answers posts with.")

(defun tether-comment-test--handler (main2 head)
  "Return an API handler for PR 7 with base tip MAIN2 and HEAD.
Answers GitHub and GitLab requests, with no pending review."
  (lambda (request)
    (let ((method (plist-get request :method))
          (resource (plist-get request :resource)))
      (cond
       ((tether-test-graphql-operation request)
        '(:value ((repository (pullRequest (reviews (nodes)))))))
       ((and (equal method "GET") (string-suffix-p "/discussions" resource))
        '(:value nil))
       ((and (equal method "GET") (string-prefix-p "/projects/" resource))
        `(:value ((target_branch . "main")
                  (diff_refs . ((base_sha . ,(magit-merge-base main2 head))
                                (start_sha . ,main2) (head_sha . ,head))))))
       ((equal method "GET")
        `(:value ((base . ((sha . ,main2) (ref . "main")))
                  (head . ((sha . ,head))))))
       (tether-comment-test--post-error
        `(:error ,tether-comment-test--post-error nil))
       ((string-prefix-p "/projects/" resource)
        '(:value ((id . "d") (notes . (((id . 1)))))))
       (t '(:value ((html_url . "https://github.com/o/r/pull/7#c"))))))))

(defmacro tether-comment-test-with (repo &rest body)
  "Run BODY in a buffer visiting a.el on the PR branch of REPO's PR 7."
  (declare (indent 1) (debug t))
  `(tether-test-with-repos
     (tether-test-git "fetch" "-q" "origin" "refs/pull/7/head")
     (tether-test-git "checkout" "-q" "-b" "feature" head)
     (let* ((repo ,repo)
            (pullreq (tether-test-pullreq))
            (tether--contexts (make-hash-table :test #'equal))
            (tether--comment-history nil)
            (tether-comment-test--post-error nil)
            (transient-mark-mode t)
            (buffer (find-file-noselect (expand-file-name "a.el"))))
       (unwind-protect
           (cl-letf (((symbol-function 'tether--repository) (lambda (_) repo))
                     ((symbol-function 'tether--branch-pullreq)
                      (lambda (_) pullreq)))
             (tether-test-with-handler (tether-comment-test--handler main2 head)
               (with-current-buffer buffer
                 ,@body)))
         (dolist (b (buffer-list))
           (when (string-prefix-p "*tether-comment" (buffer-name b))
             (with-current-buffer b (setq tether-comment--sent t))
             (kill-buffer b)))
         (kill-buffer buffer)))))

(defun tether-comment-test--goto-line (line)
  "Go to LINE of the current buffer."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun tether-comment-test--payload ()
  "Return the payload of the last request that had one.
Sending is followed by fetching the threads again."
  (seq-some (lambda (request) (plist-get request :payload))
            tether-test-requests))

(ert-deftest tether-comment-test-nitpick ()
  "The editor opens on the checked line, shows the hunk, and posts."
  (tether-comment-test-with (tether-test-github-repo)
    (tether-comment-test--goto-line 2)
    (tether-nitpick)
    (should (derived-mode-p 'tether-comment-mode))
    (should (equal (buffer-name) "*tether-comment o/r#7 a.el:2*"))
    (should (string-match-p "^\\+TWO$" (buffer-string)))
    (should (equal (tether-comment--body) ""))
    (insert "Nice\n\nreally")
    (let ((editor (current-buffer)))
      (tether-comment-send)
      (should-not (buffer-live-p editor)))
    (should (equal (tether-comment-test--payload)
                   `((body . "Nice\n\nreally") (commit_id . ,head)
                     (path . "a.el") (line . 2) (side . "RIGHT"))))))

(ert-deftest tether-comment-test-region ()
  "A region ending at the start of a line excludes that line."
  (tether-comment-test-with (tether-test-github-repo)
    (push-mark (point-min) t t)
    (goto-char (point-max))
    (tether-nitpick)
    (should (equal (buffer-name) "*tether-comment o/r#7 a.el:1-3*"))
    (insert "x")
    (tether-comment-send)
    (should (equal (alist-get 'start_line (tether-comment-test--payload)) 1))
    (should (equal (alist-get 'line (tether-comment-test--payload)) 3))))

(ert-deftest tether-comment-test-same-target ()
  "Commenting the same line again shows the editor already open."
  (tether-comment-test-with (tether-test-github-repo)
    (tether-comment-test--goto-line 2)
    (let ((editor (progn (tether-nitpick) (current-buffer))))
      (insert "draft")
      (should (eq (with-current-buffer buffer
                    (tether-comment-test--goto-line 2)
                    (tether-nitpick))
                  editor))
      (should (equal (tether-comment--body) "draft")))))

(ert-deftest tether-comment-test-unsent-text ()
  "Cancelled or killed text comes back with M-p."
  (tether-comment-test-with (tether-test-github-repo)
    (tether-comment-test--goto-line 2)
    (tether-nitpick)
    (insert "first thought")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
      (tether-comment-cancel))
    (set-buffer (with-current-buffer buffer (tether-nitpick)))
    (insert "second thought")
    (kill-buffer (current-buffer))
    (should (equal tether--comment-history '("second thought" "first thought")))
    (set-buffer (with-current-buffer buffer (tether-nitpick)))
    (tether-comment-previous)
    (should (equal (tether-comment--body) "second thought"))
    (tether-comment-previous)
    (should (equal (tether-comment--body) "first thought"))
    (tether-comment-next)
    (tether-comment-next)
    (should (equal (tether-comment--body) ""))))

(ert-deftest tether-comment-test-failed-post ()
  "When posting fails, the editor stays open with its text and says why."
  (tether-comment-test-with (tether-test-github-repo)
    (tether-comment-test--goto-line 2)
    (tether-nitpick)
    (insert "Keep me")
    (setq tether-comment-test--post-error '(error http 500 ((message . "Oops"))))
    (let ((editor (current-buffer)))
      (should-error (tether-comment-send) :type 'user-error)
      (should (buffer-live-p editor))
      (with-current-buffer editor
        (should (equal (tether-comment--body) "Keep me"))
        (should (string-match-p "GitHub rejected a.el:2: Oops"
                                (format "%s" header-line-format)))))))

(ert-deftest tether-comment-test-head-moved ()
  "If the context changed while writing, the target is computed again."
  (tether-comment-test-with (tether-test-github-repo)
    (tether-comment-test--goto-line 2)
    (tether-nitpick)
    (insert "x")
    ;; A local commit moves HEAD: the context is resolved again.
    (let ((default-directory (file-name-directory (buffer-file-name buffer))))
      (tether-test-commit "c.el" "local\n"))
    (tether-comment-send)
    (should (= (cl-count "GET" tether-test-requests
                         :key (lambda (r) (plist-get r :method)) :test #'equal)
               2))
    (should (equal (alist-get 'line (tether-comment-test--payload)) 2))))

(ert-deftest tether-comment-test-suggest-github ()
  (tether-comment-test-with (tether-test-github-repo)
    (tether-comment-test--goto-line 2)
    (tether-suggest)
    (should (equal (tether-comment--body) "```suggestion\nTWO\n```"))
    ;; Toggled off, and on again.
    (tether-comment-toggle-suggestion)
    (should (equal (tether-comment--body) ""))
    (tether-comment-toggle-suggestion)
    (should (equal (tether-comment--body) "```suggestion\nTWO\n```"))))

(ert-deftest tether-comment-test-suggest-gitlab-region ()
  "GitLab replaces only the commented line unless told how many above."
  (tether-comment-test-with (tether-test-gitlab-repo)
    (push-mark (point-min) t t)
    (goto-char (point-max))
    (tether-suggest)
    (should (equal (tether-comment--body)
                   "```suggestion:-2+0\none\nTWO\nthree\n```"))))

(ert-deftest tether-comment-test-mention ()
  (tether-comment-test-with (tether-test-github-repo)
    (tether-comment-test--goto-line 2)
    (tether-nitpick)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (member "alice" collection))
                 "alice")))
      (call-interactively #'tether-comment-mention))
    (should (equal (tether-comment--body) "@alice"))))

(ert-deftest tether-comment-test-file ()
  "A comment on the whole file has no hunk to show."
  (tether-comment-test-with (tether-test-github-repo)
    (tether-nitpick-file)
    (should (equal (buffer-name) "*tether-comment o/r#7 a.el*"))
    (should (equal (buffer-string) ""))
    (insert "Split it")
    (tether-comment-send)
    (should (equal (alist-get 'subject_type (tether-comment-test--payload))
                   "file"))))

(provide 'tether-comment-test)
;;; tether-comment-test.el ends here
