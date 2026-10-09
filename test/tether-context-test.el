;;; tether-context-test.el --- Tests for tether-context  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Context resolution against real git repositories, with forge's
;; database and the API stubbed.

;;; Code:

(require 'ert)
(require 'tether-context)
(require 'tether-test-helpers)

(defun tether-context-test-github-response (base-tip head)
  "Return the GitHub API answer for PR 7 with BASE-TIP and HEAD."
  `(("/repos/o/r/pulls/7"
     . ((number . 7)
        (base . ((sha . ,base-tip) (ref . "main")))
        (head . ((sha . ,head) (ref . "feature")))))))

(defun tether-context-test-gitlab-response (base start head)
  "Return the GitLab API answer for MR 7 with diff refs BASE START HEAD."
  `(("/projects/g%2Fsub%2Fp/merge_requests/7"
     . ((iid . 7)
        (target_branch . "main")
        (diff_refs . ((base_sha . ,base) (start_sha . ,start)
                      (head_sha . ,head)))))))

(defmacro tether-context-test-with (repo branch-pullreqs &rest body)
  "Run BODY with forge stubbed to REPO and BRANCH-PULLREQS.
BRANCH-PULLREQS is an alist (BRANCH . PULLREQ) standing for the
branch.BRANCH.pullRequest git config.  The cache starts empty and
missing commits are fetched without asking."
  (declare (indent 2) (debug t))
  `(let ((tether--contexts (make-hash-table :test #'equal))
         (tether--offered-branches nil)
         (tether-auto-fetch t)
         (repo ,repo)
         (branch-pullreqs ,branch-pullreqs))
     (cl-letf (((symbol-function 'tether--repository) (lambda (_) repo))
               ((symbol-function 'tether--branch-pullreq)
                (lambda (branch) (cdr (assoc branch branch-pullreqs))))
               ((symbol-function 'tether--open-pullreqs) (lambda (_) nil)))
       ,@body)))

(defmacro tether-context-test-picking (pullreq &rest body)
  "Run BODY with PULLREQ as the answer when a pull request is read."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'tether--read-pullreq)
              (lambda (_prompt) ,pullreq)))
     ,@body))

(ert-deftest tether-context-test-github-not-checked-out ()
  "Review from main: the PR commits are fetched, the base is computed."
  (tether-test-with-repos
    (tether-context-test-with (tether-test-github-repo) nil
      (tether-context-test-picking (tether-test-pullreq)
        (tether-test-with-api (tether-context-test-github-response main2 head)
          (should-not (tether--commit-p head))
          (let ((ctx (tether-context t t)))
            ;; GitHub's diff base is the merge-base of base tip and head.
            (should (equal (tether-context-base-sha ctx) base))
            (should (equal (tether-context-start-sha ctx) base))
            (should (equal (tether-context-head-sha ctx) head))
            (should (equal (tether-context-base-ref ctx) "main"))
            (should-not (tether-context-checked-out ctx))
            (should (equal (tether-test-git "rev-parse" "refs/tether/7") head))
            (should (equal (plist-get (car tether-test-requests) :resource)
                           "/repos/o/r/pulls/7"))
            (should (equal (plist-get (car tether-test-requests) :host)
                           "api.github.com"))))))))

(ert-deftest tether-context-test-cache ()
  "One request per PR until HEAD moves or a refresh."
  (tether-test-with-repos
    (tether-context-test-with (tether-test-github-repo) nil
      (tether-context-test-picking (tether-test-pullreq)
        (tether-test-with-api (tether-context-test-github-response main2 head)
          (let ((first (tether-context t t)))
            (should (eq (tether-context t t) first))
            (should (= (length tether-test-requests) 1))
            (should (tether-context-current-p first))
            ;; HEAD moves: resolved again, with a newer generation.
            (tether-test-commit "d.el" "local\n")
            (let ((second (tether-context t t)))
              (should-not (eq second first))
              (should (> (tether-context-generation second)
                         (tether-context-generation first)))
              (should-not (tether-context-current-p first))
              (should (= (length tether-test-requests) 2))
              ;; `tether-refresh' ignores the fresh cache.
              (let ((tether--refreshing t))
                (should-not (eq (tether-context t t) second)))
              (should (= (length tether-test-requests) 3)))))))))

(ert-deftest tether-context-test-checked-out-behind ()
  "On the PR branch, one commit behind the PR head (SPEC §3.5)."
  (tether-test-with-repos
    (let ((pullreq (tether-test-pullreq)))
      (tether-context-test-with (tether-test-github-repo)
          `(("feature" . ,pullreq))
        (tether-test-git "fetch" "-q" "origin" "refs/pull/7/head")
        (tether-test-git "checkout" "-q" "-b" "feature" pr1)
        (tether-test-with-api (tether-context-test-github-response main2 head)
          (let ((ctx (tether-context t)))
            (should (tether-context-checked-out ctx))
            (should (equal (tether-context-head-state ctx) '(behind 1)))
            (should (string-match-p "local branch is 1 commit behind #7"
                                    (tether--describe ctx)))))))))

(ert-deftest tether-context-test-forge-db-behind ()
  "forge's head-rev older than the API's head is reported."
  (tether-test-with-repos
    (let ((pullreq (tether-test-pullreq :head-rev pr1)))
      (tether-context-test-with (tether-test-github-repo)
          `(("feature" . ,pullreq))
        (tether-test-git "fetch" "-q" "origin" "refs/pull/7/head")
        (tether-test-git "checkout" "-q" "-b" "feature" head)
        (tether-test-with-api (tether-context-test-github-response main2 head)
          (let ((ctx (tether-context t)))
            (should (equal (tether-context-head-state ctx) '(same 0)))
            (should (equal (tether--notes ctx)
                           '("forge's data for #7 is behind, run `forge-pull'")))))))))

(ert-deftest tether-context-test-upstream-fallback ()
  "A branch without branch.X.pullRequest is matched by its upstream."
  (tether-test-with-repos
    (let ((pullreq (tether-test-pullreq))
          (other (tether-test-pullreq :number 8 :head-ref "other"))
          (asked nil))
      (tether-context-test-with (tether-test-github-repo) nil
        (tether-test-git "fetch" "-q" "origin" "refs/pull/7/head")
        (tether-test-git "update-ref" "refs/remotes/origin/feature" head)
        (tether-test-git "checkout" "-q" "-b" "mine" head)
        (tether-test-git "branch" "-q" "--set-upstream-to=origin/feature")
        (cl-letf (((symbol-function 'tether--open-pullreqs)
                   (lambda (_) (list other pullreq)))
                  ((symbol-function 'forge-get-repository)
                   (lambda (&rest _) (tether-test-github-repo)))
                  ((symbol-function 'y-or-n-p)
                   (lambda (prompt) (setq asked prompt) t)))
          (tether-test-with-api (tether-context-test-github-response main2 head)
            (let ((ctx (tether-context t)))
              (should (eq (tether-context-pullreq ctx) pullreq))
              (should (tether-context-checked-out ctx))
              ;; Offered to record it, and recorded it.
              (should (string-match-p "Record #7 as the pull request of mine"
                                      asked))
              (should (equal (tether-test-git "config" "branch.mine.pullRequest")
                             "7")))))))))

(ert-deftest tether-context-test-no-pullreq ()
  (tether-test-with-repos
    (tether-context-test-with (tether-test-github-repo) nil
      (let ((err (should-error (tether-context t) :type 'user-error)))
        (should (string-match-p "no pull request for branch main; pick one with C-u"
                                (cadr err)))))))

(ert-deftest tether-context-test-gitlab ()
  "GitLab's diff refs are used as given."
  (tether-test-with-repos
    (tether-context-test-with (tether-test-gitlab-repo) nil
      (tether-context-test-picking (tether-test-pullreq)
        (tether-test-with-api (tether-context-test-gitlab-response base main2 head)
          (let ((ctx (tether-context t t)))
            (should (equal (tether-context-base-sha ctx) base))
            (should (equal (tether-context-start-sha ctx) main2))
            (should (equal (tether-context-head-sha ctx) head))
            (should (equal (tether-test-git "rev-parse" "refs/tether/7") head))
            (should (string-prefix-p "!7 " (tether--describe ctx)))))))))

(ert-deftest tether-context-test-gitlab-diff-not-ready ()
  (tether-test-with-repos
    (tether-context-test-with (tether-test-gitlab-repo) nil
      (tether-context-test-picking (tether-test-pullreq)
        (tether-test-with-api '(("/projects/g%2Fsub%2Fp/merge_requests/7"
                                 . ((iid . 7) (diff_refs . nil))))
          (let ((err (should-error (tether-context t t) :type 'user-error)))
            (should (string-match-p "has not computed the diff of !7"
                                    (cadr err))))
          (should (zerop (hash-table-count tether--contexts))))))))

(ert-deftest tether-context-test-fetch-declined ()
  (tether-test-with-repos
    (tether-context-test-with (tether-test-github-repo) nil
      (tether-context-test-picking (tether-test-pullreq)
        (tether-test-with-api (tether-context-test-github-response main2 head)
          (let ((tether-auto-fetch nil))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
              (should-error (tether-context t t) :type 'user-error)
              (should-not (tether--commit-p head)))))))))

(ert-deftest tether-context-test-clear-cache ()
  "Clearing forgets contexts and deletes the fetched refs."
  (tether-test-with-repos
    (tether-context-test-with (tether-test-github-repo) nil
      (tether-context-test-picking (tether-test-pullreq)
        (tether-test-with-api (tether-context-test-github-response main2 head)
          (tether-context t t)
          (tether-clear-cache)
          (should (zerop (hash-table-count tether--contexts)))
          (should (equal (tether-test-git "for-each-ref" "refs/tether/") "")))))))

(provide 'tether-context-test)
;;; tether-context-test.el ends here
