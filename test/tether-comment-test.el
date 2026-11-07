;;; tether-comment-test.el --- Tests for the comment editor  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `tether-comment-line', `tether-comment-suggest' and `tether-comment-file' end to
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
       ((and (equal method "GET")
             (or (string-suffix-p "/discussions" resource)
                 (string-suffix-p "/draft_notes" resource)))
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
    (tether-comment-line)
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
    (tether-comment-line)
    (should (equal (buffer-name) "*tether-comment o/r#7 a.el:1-3*"))
    (insert "x")
    (tether-comment-send)
    (should (equal (alist-get 'start_line (tether-comment-test--payload)) 1))
    (should (equal (alist-get 'line (tether-comment-test--payload)) 3))))

(ert-deftest tether-comment-test-same-target ()
  "Commenting the same line again shows the editor already open."
  (tether-comment-test-with (tether-test-github-repo)
    (tether-comment-test--goto-line 2)
    (let ((editor (progn (tether-comment-line) (current-buffer))))
      (insert "draft")
      (should (eq (with-current-buffer buffer
                    (tether-comment-test--goto-line 2)
                    (tether-comment-line))
                  editor))
      (should (equal (tether-comment--body) "draft")))))

(ert-deftest tether-comment-test-unsent-text ()
  "Cancelled or killed text comes back with M-p."
  (tether-comment-test-with (tether-test-github-repo)
    (tether-comment-test--goto-line 2)
    (tether-comment-line)
    (insert "first thought")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
      (tether-comment-cancel))
    (set-buffer (with-current-buffer buffer (tether-comment-line)))
    (insert "second thought")
    (kill-buffer (current-buffer))
    (should (equal tether--comment-history '("second thought" "first thought")))
    (set-buffer (with-current-buffer buffer (tether-comment-line)))
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
    (tether-comment-line)
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
    (tether-comment-line)
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
    (tether-comment-suggest)
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
    (tether-comment-suggest)
    (should (equal (tether-comment--body)
                   "```suggestion:-2+0\none\nTWO\nthree\n```"))))

(ert-deftest tether-comment-test-mention ()
  (tether-comment-test-with (tether-test-github-repo)
    (tether-comment-test--goto-line 2)
    (tether-comment-line)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (member "alice" collection))
                 "alice")))
      (call-interactively #'tether-comment-mention))
    (should (equal (tether-comment--body) "@alice"))))

(ert-deftest tether-comment-test-file ()
  "A comment on the whole file has no hunk to show."
  (tether-comment-test-with (tether-test-github-repo)
    (tether-comment-file)
    (should (equal (buffer-name) "*tether-comment o/r#7 a.el*"))
    (should (equal (buffer-string) ""))
    (insert "Split it")
    (tether-comment-send)
    (should (equal (alist-get 'subject_type (tether-comment-test--payload))
                   "file"))))

(defun tether-comment-test--new-file-hunk (count)
  "Return the hunk of a new file of COUNT lines, \"line 1\"..."
  (concat (format "@@ -0,0 +1,%d @@\n" count)
          (mapconcat (lambda (n) (format "+line %d" n)) (number-sequence 1 count) "\n")
          "\n"))

(ert-deftest tether-comment-test-hunk-excerpt ()
  "A big hunk is cut to the lines near the comment, with ⋮ for the rest."
  (let ((excerpt (split-string (tether-comment--hunk-excerpt
                                (tether-comment-test--new-file-hunk 3223) 905 906 'right)
                               "\n")))
    (should (equal (car excerpt) "@@ -0,0 +1,3223 @@"))
    (should (equal (nth 1 excerpt) "⋮"))
    (should (equal (nth 2 excerpt) "+line 895"))
    (should (equal (car (last excerpt 2)) "+line 916"))
    (should (equal (car (last excerpt)) "⋮"))
    (should (= (length excerpt) (+ 1 1 22 1))))
  ;; At the start of the hunk: nothing cut before.
  (should (equal (nth 1 (split-string (tether-comment--hunk-excerpt
                                       (tether-comment-test--new-file-hunk 100) 1 1 'right)
                                      "\n"))
                 "+line 1"))
  ;; A small hunk stays whole.
  (let ((hunk "@@ -1,3 +1,3 @@\n one\n-two\n+TWO\n three\n"))
    (should (equal (tether-comment--hunk-excerpt hunk 2 2 'right) hunk))))

(ert-deftest tether-comment-test-hunk-excerpt-sides ()
  "Line numbers follow the side: the base for removed lines, the head else."
  (let* ((hunk (concat "@@ -1,60 +1,30 @@\n"
                       (mapconcat (lambda (n) (format "-old %d" n)) (number-sequence 1 60) "\n")
                       "\n"
                       (mapconcat (lambda (n) (format "+new %d" n)) (number-sequence 1 30) "\n")))
         (left (tether-comment--hunk-excerpt hunk 50 50 'left))
         (right (tether-comment--hunk-excerpt hunk 5 5 'right)))
    (should (string-search "-old 50\n" left))
    (should-not (string-search "-old 30\n" left))
    (should (string-search "+new 5\n" right))
    (should-not (string-search "+new 20\n" right))))

(ert-deftest tether-comment-test-lines-excerpt ()
  "Lines outside the diff: a long selection keeps its first and last lines."
  (let ((text (mapconcat #'number-to-string (number-sequence 1 100) "\n")))
    (should (equal (split-string (tether-comment--lines-excerpt text) "\n")
                   (append (mapcar #'number-to-string (number-sequence 1 10))
                           '("⋮")
                           (mapcar #'number-to-string (number-sequence 91 100))))))
  (should (equal (tether-comment--lines-excerpt "a\nb") "a\nb")))

(ert-deftest tether-comment-test-big-new-file ()
  "Commenting on a big new file keeps the editor small, so it stays fast."
  (cl-letf (((symbol-function 'tether-hunk-text)
             (lambda (&rest _) (tether-comment-test--new-file-hunk 3223))))
    (with-temp-buffer
      (tether-comment-mode)
      (let ((line (make-tether-line :kind :added :new 905 :hunk 0)))
        (tether-comment--insert-hunk nil (list :path "doc.md" :start line :end line
                                               :side 'right)))
      (should (< (buffer-size) 1000))
      (should (string-search "+line 905\n" (buffer-string))))))

(provide 'tether-comment-test)
;;; tether-comment-test.el ends here
