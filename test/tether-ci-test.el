;;; tether-ci-test.el --- Tests for the CI status of pull requests  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fetching the CI status on both forges, showing it at the end of the
;; pull request lines and in the mode line, fetching it only on `g',
;; and opening it in the browser.

;;; Code:

(require 'ert)
(require 'tether)
(require 'tether-test-helpers)

(defmacro tether-ci-test-with (&rest body)
  "Run BODY with no CI status known and nothing being fetched."
  (declare (indent 0) (debug t))
  `(let ((tether-ci--statuses (make-hash-table :test #'equal))
         (tether-ci--fetching nil)
         (tether-ci-functions nil)
         (tether-ci-status t))
     ,@body))

(defmacro tether-ci-test-with-list (numbers &rest body)
  "Run BODY in a magit buffer listing the pull requests NUMBERS."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (magit-section-mode)
     (let ((inhibit-read-only t))
       (magit-insert-section (root)
         (insert "Head: main\n\n")
         (magit-insert-section (pullreqs)
           (magit-insert-heading "Pull requests")
           (dolist (number ,numbers)
             (magit-insert-section (pullreq (tether-test-pullreq :number number))
               (insert (format "#%d Fix parsing\n" number)))))))
     ,@body))

(defun tether-ci-test--status (number state)
  "Return a CI status of pull request NUMBER in STATE."
  (list :number number :state state :sha "0123456789abcdef"
        :url (format "https://ci/%d" number)))

(defun tether-ci-test--drawn ()
  "Return (LINE-TEXT . MARK) of each CI mark drawn, in buffer order."
  (mapcar (lambda (overlay)
            (save-excursion
              (goto-char (overlay-start overlay))
              (cons (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position))
                    (substring-no-properties (overlay-get overlay 'after-string)))))
          (sort (seq-filter (lambda (overlay) (overlay-get overlay 'tether-ci))
                            (overlays-in (point-min) (point-max)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

;;;; Fetching

(ert-deftest tether-ci-test-github ()
  "GitHub: one query for the checks of every open pull request."
  (tether-ci-test-with
    (tether-test-with-handler
        (lambda (_)
          '(:value ((repository
                     (pullRequests
                      ((number . 7)
                       (commits (nodes ((commit (oid . "aaa1111")
                                                (statusCheckRollup (state . "SUCCESS")))))))
                      ((number . 8)
                       (commits (nodes ((commit (oid . "bbb2222")
                                                (statusCheckRollup (state . "ERROR")))))))
                      ((number . 9)
                       (commits (nodes ((commit (oid . "ccc3333")
                                                (statusCheckRollup (state . "PENDING")))))))
                      ;; No checks at all.
                      ((number . 10)
                       (commits (nodes ((commit (oid . "ddd4444")
                                                (statusCheckRollup)))))))))))
      (tether-ci-fetch (tether-test-github-repo))
      (should (= (length tether-test-requests) 1))
      (let ((request (car tether-test-requests)))
        (should (equal (tether-test-graphql-operation request) 'repository))
        (should (equal (plist-get request :variables) '((owner . "o") (name . "r")))))
      (should (equal (gethash '("repo-id" . 7) tether-ci--statuses)
                     '(:number 7 :state success :sha "aaa1111"
                       :url "https://github.com/o/r/pull/7/checks")))
      (should (eq (plist-get (gethash '("repo-id" . 8) tether-ci--statuses) :state)
                  'failure))
      (should (eq (plist-get (gethash '("repo-id" . 9) tether-ci--statuses) :state)
                  'running))
      (should-not (gethash '("repo-id" . 10) tether-ci--statuses))
      (should-not tether-ci--fetching))))

(ert-deftest tether-ci-test-gitlab ()
  "GitLab: one query for the head pipeline of every open merge request."
  (tether-ci-test-with
    (tether-test-with-handler
        (lambda (_)
          ;; As ghub passes it: the response's data member, key included.
          '(:value (data
                    (project
                     (mergeRequests
                      ((iid . "7")
                       (headPipeline (status . "FAILED") (sha . "aaa1111")
                                     (path . "/g/sub/p/-/pipelines/42")))
                      ((iid . "8")
                       (headPipeline (status . "MANUAL") (sha . "bbb2222")
                                     (path . "/g/sub/p/-/pipelines/43")))
                      ;; No pipeline.
                      ((iid . "9") (headPipeline)))))))
      (tether-ci-fetch (tether-test-gitlab-repo))
      (should (equal (plist-get (car tether-test-requests) :variables)
                     '((path . "g/sub/p"))))
      (should (equal (gethash '("repo-id" . 7) tether-ci--statuses)
                     '(:number 7 :state failure :sha "aaa1111"
                       :url "https://gitlab.com/g/sub/p/-/pipelines/42")))
      (should (eq (plist-get (gethash '("repo-id" . 8) tether-ci--statuses) :state)
                  'canceled))
      (should-not (gethash '("repo-id" . 9) tether-ci--statuses)))))

(ert-deftest tether-ci-test-replaces ()
  "A new answer replaces the statuses of the repository: merged PRs go."
  (tether-ci-test-with
    (tether-ci--store "repo-id" (list (tether-ci-test--status 7 'success)
                                      (tether-ci-test--status 8 'running)))
    (tether-ci--store "other" (list (tether-ci-test--status 7 'failure)))
    (tether-ci--store "repo-id" (list (tether-ci-test--status 8 'success)))
    (should-not (gethash '("repo-id" . 7) tether-ci--statuses))
    (should (eq (plist-get (gethash '("repo-id" . 8) tether-ci--statuses) :state)
                'success))
    (should (gethash '("other" . 7) tether-ci--statuses))))

(ert-deftest tether-ci-test-error ()
  "A failed query says so and keeps what was known."
  (tether-ci-test-with
    (tether-ci--store "repo-id" (list (tether-ci-test--status 7 'success)))
    (tether-test-with-handler
        (lambda (_) '(:error (error "boom") nil))
      (tether-ci-fetch (tether-test-github-repo)))
    (should-not tether-ci--fetching)
    (should (gethash '("repo-id" . 7) tether-ci--statuses))))

(ert-deftest tether-ci-test-fetching-once ()
  "No second request while the first one is on its way."
  (tether-ci-test-with
    (let ((tether-ci--fetching (list "repo-id")))
      (tether-test-with-handler (lambda (_) (error "Unexpected request"))
        (tether-ci-fetch (tether-test-github-repo))
        (should-not tether-test-requests)))))

(ert-deftest tether-ci-test-refresh ()
  "`tether-refresh' fetches the CI status of its repository too."
  (tether-ci-test-with
    (let ((fetched nil))
      (cl-letf (((symbol-function 'tether--fetch-ci)
                 (lambda (repo _done) (push (oref repo owner) fetched))))
        (run-hook-with-args 'tether-refresh-functions
                            (tether-context--make :repo (tether-test-github-repo)))
        (should (equal fetched '("o")))
        (let ((tether-ci-status nil)
              (tether-ci--fetching nil))
          (run-hook-with-args 'tether-refresh-functions
                              (tether-context--make :repo (tether-test-github-repo)))
          (should (equal fetched '("o"))))))))

;;;; In lists of pull requests

(ert-deftest tether-ci-test-draw ()
  "The mark goes at the end of the line of each PR whose CI is known."
  (tether-ci-test-with
    (tether-ci--store "repo-id" (list (tether-ci-test--status 7 'success)
                                      (tether-ci-test--status 9 'failure)))
    (tether-ci-test-with-list '(7 8 9)
      (tether-ci--draw)
      (should (equal (tether-ci-test--drawn)
                     '(("#7 Fix parsing" . " ✓") ("#9 Fix parsing" . " ✗"))))
      ;; Drawn again: replaced, not added.
      (tether-ci--draw)
      (should (= (length (tether-ci-test--drawn)) 2))
      (should (equal (get-text-property 0 'help-echo (tether-ci-mark "repo-id" 7))
                     "CI: success on 0123456")))))

(ert-deftest tether-ci-test-g-fetches ()
  "Only `g' fetches: one request per repository.  Other refreshes draw."
  (tether-ci-test-with
    (tether-ci-test-with-list '(7 8)
      (tether-test-with-handler
          (lambda (_)
            '(:value ((repository
                       (pullRequests
                        ((number . 7)
                         (commits (nodes ((commit (oid . "aaa1111")
                                                  (statusCheckRollup (state . "SUCCESS"))))))))))))
        (cl-letf (((symbol-function 'forge-get-repository)
                   (lambda (&rest _) (tether-test-github-repo))))
          ;; A refresh after a commit, say.
          (let ((this-command 'magit-commit-create))
            (tether-ci--magit-refreshed))
          (should-not tether-test-requests)
          (let ((this-command 'magit-refresh)
                (tether-ci-status nil))
            (tether-ci--magit-refreshed))
          (should-not tether-test-requests)
          (let ((this-command 'magit-refresh))
            (tether-ci--magit-refreshed))
          (should (= (length tether-test-requests) 1))
          (should (equal (tether-ci-test--drawn) '(("#7 Fix parsing" . " ✓")))))))))

(ert-deftest tether-ci-test-arrives-later ()
  "An answer arriving later is drawn in the buffers listing PRs."
  (tether-ci-test-with
    (let (pending)
      (cl-letf (((symbol-function 'tether--fetch-ci)
                 (lambda (_repo done) (setq pending done))))
        (tether-ci-test-with-list '(7)
          (tether-ci-fetch (tether-test-github-repo))
          (should-not (tether-ci-test--drawn))
          (funcall pending (list :ok t :statuses (list (tether-ci-test--status 7 'running))))
          (should (equal (tether-ci-test--drawn) '(("#7 Fix parsing" . " ●")))))))))

;;;; Mode line and browser

(ert-deftest tether-ci-test-lighter ()
  "The mode line shows the CI mark of the buffer's PR."
  (tether-ci-test-with
    (let ((tether--contexts (make-hash-table :test #'equal)))
      (with-temp-buffer
        (tether--update-lighter (cons (tether-test-github-repo) (tether-test-pullreq)))
        (should (equal tether--lighter " #7"))
        (tether-ci--store "repo-id" (list (tether-ci-test--status 7 'failure)))
        (tether--update-lighter (cons (tether-test-github-repo) (tether-test-pullreq)))
        (should (equal (substring-no-properties tether--lighter) " #7✗"))))))

(ert-deftest tether-ci-test-browse ()
  "`tether-browse-ci' opens the PR at point: its pipeline, else its CI tab."
  (tether-ci-test-with
    (let (opened (repo (tether-test-gitlab-repo)))
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url)))
                ((symbol-function 'forge-get-repository) (lambda (&rest _) repo)))
        (tether-ci--store "repo-id" (list (tether-ci-test--status 7 'success)))
        (tether-ci-test-with-list '(7 8)
          (goto-char (point-min))
          (search-forward "#7")
          (tether-browse-ci)
          (should (equal opened "https://ci/7"))
          (search-forward "#8")
          (tether-browse-ci)
          (should (equal opened "https://gitlab.com/g/sub/p/-/merge_requests/8/pipelines"))
          (setq repo (tether-test-github-repo))
          (tether-browse-ci)
          (should (equal opened "https://github.com/o/r/pull/8/checks")))))))

(provide 'tether-ci-test)
;;; tether-ci-test.el ends here
