;;; tether-post-test.el --- Tests for posting comments  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Payloads and flows of `tether--post-comment' for GitHub and GitLab
;;, with the API stubbed, and `tether-nitpick'
;; end to end on a git repository.

;;; Code:

(require 'ert)
(require 'tether-comment)
(require 'tether-test-helpers)

(defconst tether-post-test--fixtures
  (expand-file-name "fixtures/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory of the diff fixtures.")

(defun tether-post-test--map ()
  "Return the line map of the modified.diff fixture (mod.txt)."
  (car (tether-diff-parse
        (with-temp-buffer
          (insert-file-contents
           (expand-file-name "modified.diff" tether-post-test--fixtures))
          (buffer-string)))))

(defun tether-post-test--target (beg end)
  "Return a target on head lines BEG to END of mod.txt."
  (let ((region (tether-file-map-region (tether-post-test--map) beg end)))
    (list :path "mod.txt" :old-path "mod.txt"
          :start (car region) :end (cdr region) :side 'right)))

(defun tether-post-test--ctx (repo)
  "Return a review context of PR 7 of REPO."
  (tether-context--make :repo repo :pullreq (tether-test-pullreq)
                        :base-sha "BASE" :start-sha "START" :head-sha "HEAD"))

(defconst tether-post-test--mod-sha1 "e38a819144626727f7eb5e7cb2d90777ebc6565b"
  "SHA-1 of \"mod.txt\", computed outside Emacs.")

;;;; GitLab

(ert-deftest tether-post-test-gitlab-position-line ()
  "An added line has only new_line; no line_range for one line."
  (should (equal (tether-gitlab--position
                  (tether-post-test--ctx (tether-test-gitlab-repo))
                  (tether-post-test--target 3 3))
                 '((position_type . "text") (base_sha . "BASE")
                   (start_sha . "START") (head_sha . "HEAD")
                   (old_path . "mod.txt") (new_path . "mod.txt")
                   (new_line . 3)))))

(ert-deftest tether-post-test-gitlab-position-context ()
  "A context line needs both line numbers."
  (let ((position (tether-gitlab--position
                   (tether-post-test--ctx (tether-test-gitlab-repo))
                   (tether-post-test--target 10 10))))
    (should (equal (alist-get 'old_line position) 11))
    (should (equal (alist-get 'new_line position) 10))))

(ert-deftest tether-post-test-gitlab-line-range ()
  "A range is anchored on its last line, with line codes at both ends.
Added line 3 follows removed old line 3, so its old counter is 4, as
GitLab's diff parser computes it."
  (let* ((position (tether-gitlab--position
                    (tether-post-test--ctx (tether-test-gitlab-repo))
                    (tether-post-test--target 3 4)))
         (range (alist-get 'line_range position)))
    (should (equal (alist-get 'old_line position) 4))
    (should (equal (alist-get 'new_line position) 4))
    (should (equal (alist-get 'start range)
                   `((line_code . ,(concat tether-post-test--mod-sha1 "_4_3"))
                     (type . "new") (new_line . 3))))
    (should (equal (alist-get 'end range)
                   `((line_code . ,(concat tether-post-test--mod-sha1 "_4_4"))
                     (old_line . 4) (new_line . 4))))))

(ert-deftest tether-post-test-gitlab-post ()
  (let ((repo (tether-test-gitlab-repo)))
    (tether-test-with-handler
        (lambda (request)
          (should (equal (plist-get request :resource)
                         "/projects/g%2Fsub%2Fp/merge_requests/7/discussions"))
          '(:value ((id . "d1") (notes . (((id . 99)))))))
      (let ((result (tether--post-comment repo (tether-post-test--ctx repo)
                                          (tether-post-test--target 3 3) "Typo")))
        (should (equal (plist-get result :url)
                       "https://gitlab.com/g/sub/p/-/merge_requests/7#note_99"))
        (should (equal (plist-get result :message) "commented on mod.txt:3 (!7)"))
        (let ((payload (plist-get (car tether-test-requests) :payload)))
          (should (equal (alist-get 'body payload) "Typo"))
          (should (equal (alist-get 'new_line (alist-get 'position payload)) 3)))))))

(ert-deftest tether-post-test-gitlab-file-comment ()
  "A comment on the whole file is a general note naming it."
  (let ((repo (tether-test-gitlab-repo)))
    (tether-test-with-handler
        (lambda (request)
          (should (equal (plist-get request :resource)
                         "/projects/g%2Fsub%2Fp/merge_requests/7/notes"))
          '(:value ((id . 5))))
      (tether--post-comment repo (tether-post-test--ctx repo)
                            '(:path "mod.txt" :side right) "Split it")
      (should (equal (alist-get 'body (plist-get (car tether-test-requests)
                                                 :payload))
                     "`mod.txt`\n\nSplit it")))))

(ert-deftest tether-post-test-gitlab-rejected ()
  (let ((repo (tether-test-gitlab-repo)))
    (tether-test-with-handler
        (lambda (_)
          '(:error (error http 400 ((message . "400 Bad request - Note {:line_code=>[\"can't be blank\"]}")))
                   nil))
      (let ((err (should-error
                  (tether--post-comment repo (tether-post-test--ctx repo)
                                        (tether-post-test--target 3 3) "x")
                  :type 'user-error)))
        (should (string-match-p
                 "GitLab rejected mod.txt:3: .*line_code.* (its diff and git's differ here)"
                 (cadr err)))))))

;;;; GitHub

(defun tether-post-test--github (pending &optional rest-error)
  "Return a GitHub API handler.
PENDING is a list of answers to successive pending-review queries:
nil for none, or (ID . COMMENTS).  REST-ERROR, if non-nil, is the
error of the REST comment request."
  (lambda (request)
    (pcase (or (tether-test-graphql-operation request)
               (plist-get request :method))
      ('repository
       (let ((review (pop pending)))
         `(:value ((repository
                    (pullRequest
                     (reviews
                      (nodes ,@(and review
                                    `(((id . ,(car review))
                                       (comments (totalCount . ,(cdr review))))))))))))))
      ('addPullRequestReviewThread
       '(:value ((addPullRequestReviewThread
                  (thread (id . "T1")
                          (comments (nodes ((url . "https://github.com/o/r/pull/7#r1")))))))))
      ("POST"
       (if rest-error
           `(:error ,rest-error nil)
         '(:value ((html_url . "https://github.com/o/r/pull/7#discussion_r2"))))))))

(ert-deftest tether-post-test-github-payloads ()
  (let ((ctx (tether-post-test--ctx (tether-test-github-repo))))
    (should (equal (tether-github--comment-payload
                    ctx (tether-post-test--target 3 3) "b")
                   '((body . "b") (commit_id . "HEAD") (path . "mod.txt")
                     (line . 3) (side . "RIGHT"))))
    (should (equal (tether-github--comment-payload
                    ctx (tether-post-test--target 2 4) "b")
                   '((body . "b") (commit_id . "HEAD") (path . "mod.txt")
                     (line . 4) (side . "RIGHT")
                     (start_line . 2) (start_side . "RIGHT"))))
    (should (equal (tether-github--comment-payload
                    ctx '(:path "mod.txt" :side right) "b")
                   '((body . "b") (commit_id . "HEAD") (path . "mod.txt")
                     (subject_type . "file"))))
    (should (equal (tether-github--thread-input "R" (tether-post-test--target 2 4) "b")
                   '((pullRequestReviewId . "R") (path . "mod.txt") (body . "b")
                     (subjectType . "LINE") (line . 4) (side . "RIGHT")
                     (startLine . 2) (startSide . "RIGHT"))))))

(ert-deftest tether-post-test-github-post ()
  (let ((repo (tether-test-github-repo)))
    (tether-test-with-handler (tether-post-test--github '(nil))
      (let ((result (tether--post-comment repo (tether-post-test--ctx repo)
                                          (tether-post-test--target 3 3) "b")))
        (should (equal (plist-get result :url)
                       "https://github.com/o/r/pull/7#discussion_r2"))
        (should (equal (plist-get result :message) "commented on mod.txt:3 (#7)"))
        (should (equal (plist-get (car tether-test-requests) :resource)
                       "/repos/o/r/pulls/7/comments"))
        ;; The pending-review check asked for PR 7 of o/r.
        (should (equal (plist-get (cadr tether-test-requests) :variables)
                       '((owner . "o") (name . "r") (number . 7))))))))

(ert-deftest tether-post-test-github-pending-review ()
  "With a pending review, the comment is added to it."
  (let ((repo (tether-test-github-repo)))
    (tether-test-with-handler (tether-post-test--github '(("R1" . 2)))
      (let ((result (tether--post-comment repo (tether-post-test--ctx repo)
                                          (tether-post-test--target 3 3) "b")))
        (should (equal (plist-get result :message)
                       "added mod.txt:3 to your pending review of #7 (3 comments); \
submit it on GitHub"))
        (should (equal (plist-get result :url) "https://github.com/o/r/pull/7#r1"))
        (should (equal (alist-get 'pullRequestReviewId
                                  (alist-get 'input (plist-get (car tether-test-requests)
                                                               :variables)))
                       "R1"))
        (should-not (cl-some (lambda (r) (plist-get r :method))
                             tether-test-requests))))))

(ert-deftest tether-post-test-github-pending-review-race ()
  "A pending review created after the check is found on the 422."
  (let ((repo (tether-test-github-repo)))
    (tether-test-with-handler
        (tether-post-test--github
         '(nil ("R2" . 0))
         '(error http 422 ((message . "Unprocessable Entity")
                           (errors . ("user_id can only have one pending review per pull request")))))
      (should (string-prefix-p
               "added mod.txt:3 to your pending review of #7 (1 comments)"
               (plist-get (tether--post-comment repo (tether-post-test--ctx repo)
                                                (tether-post-test--target 3 3) "b")
                          :message))))))

(ert-deftest tether-post-test-github-rejected ()
  (let ((repo (tether-test-github-repo)))
    (tether-test-with-handler
        (tether-post-test--github
         '(nil)
         '(error http 422 ((message . "Validation Failed")
                           (errors . ("pull_request_review_thread.line must be part of the diff")))))
      (let ((err (should-error
                  (tether--post-comment repo (tether-post-test--ctx repo)
                                        (tether-post-test--target 3 3) "b")
                  :type 'user-error)))
        (should (string-match-p
                 "GitHub rejected mod.txt:3: Validation Failed; .*part of the diff.* \
(its diff and git's differ here)"
                 (cadr err)))))))

(defun tether-post-test--graphql (query)
  "Return the GraphQL text of QUERY with whitespace normalized."
  (replace-regexp-in-string
   "[ \t\n]+" " " (gsexp-encode (ghub--graphql-prepare-query query))))

(ert-deftest tether-post-test-github-graphql-text ()
  "The queries encode to the GraphQL GitHub documents."
  (should (equal (tether-post-test--graphql tether-github--pending-review-query)
                 "query ($owner: String!,$name: String!,$number: Int!) { \
repository (owner: $owner,name: $name) { pullRequest (number: $number) { \
reviews (states: PENDING,first: 1) { nodes { id comments { totalCount } } } } } }"))
  (should (equal (tether-post-test--graphql tether-github--add-thread-mutation)
                 "mutation ($input: AddPullRequestReviewThreadInput!) { \
addPullRequestReviewThread (input: $input) { thread { id comments (first: 1) \
{ nodes { url } } } } }")))

;;;; tether-nitpick

(defun tether-post-test--nitpick-handler (main2 head)
  "Return a GitHub handler for PR 7 with base tip MAIN2 and HEAD."
  (let ((github (tether-post-test--github '(nil nil))))
    (lambda (request)
      (if (equal (plist-get request :method) "GET")
          `(:value ((base . ((sha . ,main2) (ref . "main")))
                    (head . ((sha . ,head)))))
        (funcall github request)))))

(ert-deftest tether-post-test-nitpick ()
  "From a file buffer: the line or region is checked, read, and posted."
  (tether-test-with-repos
    (tether-test-git "fetch" "-q" "origin" "refs/pull/7/head")
    (tether-test-git "checkout" "-q" "-b" "feature" head)
    (let* ((pullreq (tether-test-pullreq))
           (repo (tether-test-github-repo))
           (tether--contexts (make-hash-table :test #'equal))
           (transient-mark-mode t)
           (buffer (find-file-noselect (expand-file-name "a.el"))))
      (unwind-protect
          (cl-letf (((symbol-function 'tether--repository) (lambda (_) repo))
                    ((symbol-function 'tether--branch-pullreq)
                     (lambda (_) pullreq))
                    ((symbol-function 'read-string) (lambda (&rest _) " Nice ")))
            (tether-test-with-handler (tether-post-test--nitpick-handler main2 head)
              (with-current-buffer buffer
                ;; Line 2 was changed by the PR.
                (goto-char (point-min))
                (forward-line 1)
                (tether-nitpick)
                (should (equal (plist-get (car tether-test-requests) :payload)
                               `((body . "Nice") (commit_id . ,head)
                                 (path . "a.el") (line . 2) (side . "RIGHT"))))
                ;; Lines 1 and 3 are context: a region over all three,
                ;; ending at the start of line 4, which is excluded.
                (push-mark (point-min) t t)
                (goto-char (point-max))
                (tether-nitpick)
                (let ((payload (plist-get (car tether-test-requests) :payload)))
                  (should (equal (alist-get 'start_line payload) 1))
                  (should (equal (alist-get 'line payload) 3))))))
        (kill-buffer buffer)))))

(ert-deftest tether-post-test-nitpick-keeps-text ()
  "When posting fails, the comment is in the kill ring."
  (let* ((repo (tether-test-github-repo))
         (ctx (tether-post-test--ctx repo))
         (kill-ring nil))
    (tether-test-with-handler
        (tether-post-test--github '(nil) '(error http 500 ((message . "Oops"))))
      (let ((err (should-error (tether--post ctx (tether-post-test--target 3 3)
                                             "Long careful text")
                               :type 'user-error)))
        (should (string-match-p "your comment is in the kill ring" (cadr err)))
        (should (equal (car kill-ring) "Long careful text"))))))

(provide 'tether-post-test)
;;; tether-post-test.el ends here
