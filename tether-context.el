;;; tether-context.el --- The pull request a buffer belongs to  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; Assisted-by: Claude:claude-opus-5-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The review context: the forge repository, the pull
;; request, and the commits its diff is made of, according to the API.
;; It is resolved once per worktree and pull request, then cached until
;; HEAD moves, the branch changes, `tether-context-ttl' passes, or
;; `tether-refresh' is called.
;;
;; `(tether-context)' only returns a cached context and never prompts or
;; touches the network; `(tether-context t)' resolves one, which may
;; ask which pull request to use, fetch missing commits, and make one
;; API request.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit)
(require 'forge)
(require 'tether-client)
(require 'tether-github)
(require 'tether-gitlab)

(defcustom tether-auto-fetch nil
  "Whether to fetch missing pull request commits without asking."
  :type 'boolean
  :group 'tether)

(defcustom tether-context-ttl 600
  "Seconds after which a review context is resolved again."
  :type 'number
  :group 'tether)

(cl-defstruct (tether-context (:constructor tether-context--make)
                              (:copier tether-context--copy))
  "What tether knows about the pull request of a worktree."
  repo          ; forge repository
  pullreq       ; forge pull request
  toplevel      ; worktree, with a trailing slash
  base-sha      ; diff base
  start-sha     ; GitLab start_sha; base-sha on GitHub
  head-sha      ; PR head according to the API
  base-ref      ; target branch name
  diff-refs     ; plist returned by `tether--fetch-diff-refs'
  maps          ; hash: path -> line map (step 3)
  threads       ; list of `tether-thread'
  threads-state ; nil (not fetched), loading, done or error
  review        ; plist (:active :id :drafts) of the user's draft review,
                ; nil until fetched
  commit        ; for a single-commit review, the commit; else nil
  checked-out   ; non-nil when the current branch is the PR's
  head-state    ; (STATE COUNT): same, behind, ahead, other or none
  local-head    ; HEAD when resolved, to notice it moving
  local-branch  ; current branch when resolved
  generation    ; increases with each resolution
  fetched-at)   ; `float-time' of the resolution

(defvar tether--contexts (make-hash-table :test #'equal)
  "Review contexts by (TOPLEVEL . PR-NUMBER).")

(defvar tether--generation 0
  "Generation of the most recently resolved context.")

(defvar-local tether--buffer-pullreq nil
  "Id of the pull request of a buffer that is not tied to a branch.
Set in the diff and blob buffers tether opens, so they belong to
their PR whatever the branch.")
;; magit re-runs the major mode when it refreshes these buffers.
(put 'tether--buffer-pullreq 'permanent-local t)

(defvar-local tether--buffer-commit nil
  "The commit a diff buffer of a single-commit review shows.")
(put 'tether--buffer-commit 'permanent-local t)

(defvar tether--commit-contexts (make-hash-table :test #'equal)
  "Single-commit review contexts, by (CONTEXT-KEY . COMMIT).")

(defvar tether-context-functions nil
  "Functions called with each newly resolved review context.")

(defvar tether--offered-branches nil
  "Branches for which recording the pull request was already offered.")

(defvar tether--refreshing nil
  "Non-nil while `tether-refresh' runs: resolve without the cache.")

;;;; Finding the pull request

(defun tether--label (repo number)
  "Return how REPO's forge writes pull request NUMBER, like #12 or !12."
  (format "%s%d" (tether--pr-prefix repo) number))

(defun tether--repository (toplevel)
  "Return the forge repository of TOPLEVEL, which forge must track."
  (let ((default-directory toplevel))
    (or (forge-get-repository :tracked? nil 'notatpt)
        (user-error "Tether: forge does not track this repository; \
run `forge-add-repository' first"))))

(defun tether--branch-pullreq (branch)
  "Return the pull request recorded in BRANCH's git config, or nil."
  (forge-get-pullreq :branch branch))

(defun tether--open-pullreqs (repo)
  "Return the open pull requests of REPO in forge's database."
  (seq-filter (lambda (pullreq) (eq (oref pullreq state) 'open))
              (oref repo pullreqs)))

(defun tether--upstream-pullreqs (repo branch)
  "Return REPO's open pull requests whose head is BRANCH's upstream.
For branches that have no branch.BRANCH.pullRequest in their git
config, such as branches you created yourself."
  (when-let* ((upstream (magit-get-upstream-branch branch))
              (split (magit-split-branch-name upstream))
              (url (magit-get "remote" (car split) "url"))
              (head (forge-get-repository url (car split) :stub?)))
    (let ((head-repo (format "%s/%s" (oref head owner) (oref head name))))
      (seq-filter (lambda (pullreq)
                    (and (equal (oref pullreq head-ref) (cdr split))
                         (stringp (oref pullreq head-repo))
                         (string-equal-ignore-case (oref pullreq head-repo)
                                                   head-repo)))
                  (tether--open-pullreqs repo)))))

(defun tether--choose-pullreq (repo pullreqs)
  "Return one of PULLREQS of REPO, asking when there are several."
  (if (cdr pullreqs)
      (let ((choices
             (mapcar (lambda (pullreq)
                       (cons (format "%s %s"
                                     (tether--label repo (oref pullreq number))
                                     (oref pullreq title))
                             pullreq))
                     pullreqs)))
        (cdr (assoc (completing-read "Pull request: " choices nil t)
                    choices)))
    (car pullreqs)))

(defun tether--offer-branch-config (repo branch pullreq)
  "Offer once to record PULLREQ of REPO as BRANCH's pull request."
  (unless (member branch tether--offered-branches)
    (push branch tether--offered-branches)
    (let ((number (number-to-string (oref pullreq number))))
      (when (y-or-n-p
             (format "Record %s as the pull request of %s \
(git config branch.%s.pullRequest)? "
                     (tether--label repo (oref pullreq number)) branch branch))
        (magit-call-git "config" (format "branch.%s.pullRequest" branch)
                        number)))))

(defun tether--read-pullreq (prompt)
  "Read a pull request with PROMPT and return it."
  (forge-get-pullreq (forge-read-pullreq prompt)))

(defun tether--current-pullreq (repo pick)
  "Return (PULLREQ . CHECKED-OUT) for the current buffer of REPO.
CHECKED-OUT is non-nil when the PR is the current branch's.  With
PICK non-nil, read the pull request instead."
  (let ((at-point (and (derived-mode-p 'forge-topic-mode 'magit-mode)
                       (forge-current-pullreq)))
        (branch (magit-get-current-branch)))
    (cond
     (pick (cons (tether--read-pullreq "Pull request: ") nil))
     (tether--buffer-pullreq
      (cons (forge-get-pullreq tether--buffer-pullreq) nil))
     (at-point (cons at-point nil))
     ((not branch)
      (cons (tether--read-pullreq "HEAD is detached; pull request: ") nil))
     (t
      (let ((pullreq (tether--branch-pullreq branch)))
        (unless pullreq
          (setq pullreq (tether--choose-pullreq
                         repo (tether--upstream-pullreqs repo branch)))
          (when pullreq
            (tether--offer-branch-config repo branch pullreq)))
        (unless pullreq
          (user-error "Tether: no pull request for branch %s in the forge \
database; run `forge-pull' if it is new, pick one with C-u, or set \
branch.%s.pullRequest" branch branch))
        (cons pullreq t))))))

;;;; Commits

(defun tether--commit-p (sha)
  "Return non-nil if commit SHA exists locally."
  (and sha (magit-rev-verify (concat sha "^{commit}"))))

(defun tether--private-ref (number)
  "Return the private ref holding the head of PR NUMBER."
  (format "refs/tether/%d" number))

(defun tether--ensure-commits (repo number refs)
  "Fetch the commits in REFS of PR NUMBER of REPO that are missing.
Fetch by ref name first, since servers may refuse to send a commit
by its id; keep the head under `tether--private-ref' so it is not
garbage-collected."
  (let ((needed (delq nil (list (plist-get refs :head)
                                (plist-get refs :base)
                                (plist-get refs :base-tip))))
        (label (tether--label repo number)))
    (unless (seq-every-p #'tether--commit-p needed)
      (let ((remote (or (oref repo remote)
                        (user-error "Tether: no git remote for %s/%s"
                                    (oref repo owner) (oref repo name)))))
        (unless (or tether-auto-fetch
                    (y-or-n-p (format "Commits of %s are missing; fetch them \
from %s? " label remote)))
          (user-error "Tether: commits of %s are not available locally"
                      label))
        (apply #'magit-call-git "fetch" remote
               (format "+%s:%s" (tether--pullreq-ref repo number)
                       (tether--private-ref number))
               (delq nil (list (plist-get refs :base-ref))))
        (dolist (sha needed)
          (unless (tether--commit-p sha)
            (magit-call-git "fetch" remote sha)))
        (when-let* ((missing (seq-remove #'tether--commit-p needed)))
          (user-error "Tether: commit %s of %s is not reachable on %s; \
run `forge-pull' and retry" (substring (car missing) 0 7) label remote))))))

(defun tether--count-commits (from to)
  "Return the number of commits in FROM..TO."
  (string-to-number
   (or (magit-git-string "rev-list" "--count" (format "%s..%s" from to)) "0")))

(defun tether--head-state (local-head head)
  "Return (STATE COUNT) for HEAD at LOCAL-HEAD and the PR head HEAD."
  (cond ((null local-head) (list 'none 0))
        ((equal local-head head) (list 'same 0))
        ((magit-rev-ancestor-p local-head head)
         (list 'behind (tether--count-commits local-head head)))
        ((magit-rev-ancestor-p head local-head)
         (list 'ahead (tether--count-commits head local-head)))
        (t (list 'other 0))))

;;;; Resolution and cache

(defun tether--context-key (ctx)
  "Return the cache key of CTX."
  (cons (tether-context-toplevel ctx)
        (oref (tether-context-pullreq ctx) number)))

(defun tether--context-fresh-p (ctx)
  "Return non-nil if CTX can still be used."
  (let ((default-directory (tether-context-toplevel ctx)))
    (and (< (- (float-time) (tether-context-fetched-at ctx)) tether-context-ttl)
         (equal (magit-rev-parse "HEAD") (tether-context-local-head ctx))
         (equal (magit-get-current-branch)
                (tether-context-local-branch ctx)))))

(defun tether-context-current-p (ctx)
  "Return non-nil if CTX is still the cached context of its PR.
Asynchronous replies check this and drop their result otherwise."
  (eq ctx (gethash (tether--context-key ctx) tether--contexts)))

(defun tether--notes (ctx)
  "Return what the user should know about CTX, as a list of strings."
  (let* ((repo (tether-context-repo ctx))
         (pullreq (tether-context-pullreq ctx))
         (label (tether--label repo (oref pullreq number)))
         (db-head (oref pullreq head-rev))
         notes)
    (when (and db-head (not (equal db-head (tether-context-head-sha ctx))))
      (push (format "forge's data for %s is behind, run `forge-pull'" label)
            notes))
    (when (tether-context-checked-out ctx)
      (pcase (tether-context-head-state ctx)
        (`(behind ,n)
         (push (format "local branch is %d commit%s behind %s; comments \
target the PR head" n (if (= n 1) "" "s") label)
               notes))
        (`(ahead ,n)
         (push (format "local branch has %d commit%s not in %s; comments \
target the PR head" n (if (= n 1) "" "s") label)
               notes))
        (`(other ,_)
         (push (format "HEAD is not the head of %s; comments target the \
PR head" label)
               notes))))
    (nreverse notes)))

(defun tether--build-context (repo pullreq toplevel checked-out)
  "Resolve the context of PULLREQ of REPO in worktree TOPLEVEL.
CHECKED-OUT is non-nil when PULLREQ is the current branch's."
  (let* ((default-directory toplevel)
         (number (oref pullreq number))
         (refs (tether--fetch-diff-refs repo pullreq))
         (head (plist-get refs :head)))
    (tether--ensure-commits repo number refs)
    (let ((base (or (plist-get refs :base)
                    (magit-merge-base (plist-get refs :base-tip) head)
                    (user-error "Tether: %s has no common ancestor with %s"
                                (tether--label repo number)
                                (plist-get refs :base-ref))))
          (local-head (magit-rev-parse "HEAD")))
      (tether-context--make
       :repo repo
       :pullreq pullreq
       :toplevel toplevel
       :base-sha base
       :start-sha (or (plist-get refs :start) base)
       :head-sha head
       :base-ref (plist-get refs :base-ref)
       :diff-refs refs
       :maps (make-hash-table :test #'equal)
       :checked-out checked-out
       :head-state (tether--head-state local-head head)
       :local-head local-head
       :local-branch (magit-get-current-branch)
       :generation (cl-incf tether--generation)
       :fetched-at (float-time)))))

(defun tether--resolve (repo pullreq toplevel checked-out)
  "Return the context of PULLREQ of REPO in TOPLEVEL, from the cache if fresh.
CHECKED-OUT is non-nil when PULLREQ is the current branch's."
  (let* ((key (cons toplevel (oref pullreq number)))
         (cached (gethash key tether--contexts)))
    (if (and cached
             (not tether--refreshing)
             (tether--context-fresh-p cached))
        cached
      (let ((ctx (tether--build-context repo pullreq toplevel checked-out)))
        (puthash key ctx tether--contexts)
        (run-hook-with-args 'tether-context-functions ctx)
        (when-let* ((notes (tether--notes ctx)))
          (message "Tether: %s" (string-join notes "; ")))
        ctx))))

(defun tether--cached-context ()
  "Return the fresh cached context of the current buffer, or nil.
Never prompts and never makes a request."
  (when-let* ((toplevel (magit-toplevel))
              (number
               (if tether--buffer-pullreq
                   (and-let* ((pullreq (forge-get-pullreq tether--buffer-pullreq)))
                     (oref pullreq number))
                 (and-let* ((branch (magit-get-current-branch))
                            (n (magit-get "branch" branch "pullRequest")))
                   (string-to-number n))))
              (ctx (gethash (cons toplevel number) tether--contexts)))
    (and (tether--context-fresh-p ctx) ctx)))

(defun tether--commit-context (ctx commit)
  "Return the context reviewing COMMIT alone, within CTX's pull request.
Its diff is COMMIT^..COMMIT; comments target COMMIT."
  (let ((key (cons (tether--context-key ctx) commit)))
    (or (let ((cached (gethash key tether--commit-contexts)))
          (and cached
               (equal (tether-context-generation cached)
                      (tether-context-generation ctx))
               cached))
        (let* ((default-directory (tether-context-toplevel ctx))
               (parent (or (magit-rev-parse (concat commit "^"))
                           (user-error "Tether: %s has no parent" commit)))
               (copy (tether-context--copy ctx)))
          (setf (tether-context-commit copy) commit
                (tether-context-base-sha copy) parent
                (tether-context-start-sha copy) parent
                (tether-context-head-sha copy) commit
                (tether-context-maps copy) (make-hash-table :test #'equal))
          (puthash key copy tether--commit-contexts)))))

(defun tether-context (&optional demand pick)
  "Return the review context of the current buffer.
Without DEMAND, return the cached context or nil, without prompting
or making requests.  With DEMAND, resolve it, signaling a
`user-error' that says what to do when that is not possible.  With
PICK, read the pull request instead of using the current one."
  (let ((ctx (tether-context--1 demand pick)))
    (if (and ctx tether--buffer-commit (not pick))
        (tether--commit-context ctx tether--buffer-commit)
      ctx)))

(defun tether-context--1 (demand pick)
  "Return the review context of the current buffer's pull request.
DEMAND and PICK are as for `tether-context'."
  (if (not (or demand pick))
      (tether--cached-context)
    (let* ((toplevel (or (magit-toplevel)
                         (user-error "Tether: not in a Git repository")))
           (repo (tether--repository toplevel))
           (found (tether--current-pullreq repo pick))
           (pullreq (car found)))
      (unless (equal (oref pullreq repository) (oref repo id))
        (user-error "Tether: %s belongs to another repository than %s/%s"
                    (tether--label repo (oref pullreq number))
                    (oref repo owner) (oref repo name)))
      (tether--resolve repo pullreq toplevel (cdr found)))))

;;;; Commands

(defun tether--threads-note (ctx)
  "Return what to say about the review threads of CTX, or nil."
  (pcase (tether-context-threads-state ctx)
    ('done (let ((open (seq-count (lambda (thread)
                                   (not (or (tether-thread-resolved thread)
                                            (tether-thread-outdated thread))))
                                 (tether-context-threads ctx))))
             (format "%s open thread%s" (if (zerop open) "no" open)
                     (if (= open 1) "" "s"))))
    ('loading "threads still fetching")
    ('error "threads could not be fetched")))

(defun tether--describe (ctx)
  "Return a one-line description of CTX."
  (let* ((repo (tether-context-repo ctx))
         (pullreq (tether-context-pullreq ctx)))
    (string-join
     (cons (format (if (tether-context-commit ctx)
                       "%s %s: commit %4$s only"
                     "%s %s: %s..%s")
                   (tether--label repo (oref pullreq number))
                   (oref pullreq title)
                   (substring (tether-context-base-sha ctx) 0 7)
                   (substring (tether-context-head-sha ctx) 0 7))
           (append (or (tether--notes ctx)
                       (list (if (tether-context-checked-out ctx)
                                 "checked out"
                               "not checked out")))
                   (delq nil (list (tether--threads-note ctx)))))
     "; ")))

(defun tether--wait-for-threads (ctx)
  "Wait, at most `tether-request-timeout' seconds, for CTX's threads."
  (let ((deadline (+ (float-time) tether-request-timeout)))
    (while (and (eq (tether-context-threads-state ctx) 'loading)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))))

;;;###autoload
(defun tether-show-context (&optional pick)
  "Show the pull request of the current buffer and its commits.
With prefix argument PICK, choose the pull request."
  (interactive "P")
  (let ((ctx (tether-context t pick)))
    (tether--wait-for-threads ctx)
    (message "Tether: %s" (tether--describe ctx))))

;;;###autoload
(defun tether-refresh (&optional pick)
  "Resolve the pull request of the current buffer again.
With prefix argument PICK, choose the pull request."
  (interactive "P")
  (let* ((tether--refreshing t)
         (ctx (tether-context t pick)))
    (tether--wait-for-threads ctx)
    (message "Tether: %s" (tether--describe ctx))))

;;;###autoload
(defun tether-clear-cache ()
  "Forget all review contexts and delete the refs tether fetched."
  (interactive)
  (let ((toplevels (delete-dups
                    (delq nil (cons (magit-toplevel)
                                    (mapcar #'car (hash-table-keys
                                                   tether--contexts)))))))
    (dolist (toplevel toplevels)
      (let ((default-directory toplevel))
        (dolist (ref (magit-git-lines "for-each-ref" "--format=%(refname)"
                                      "refs/tether/"))
          (magit-call-git "update-ref" "-d" ref)))))
  (clrhash tether--contexts)
  (setq tether--offered-branches nil)
  (message "Tether: cache cleared"))

(provide 'tether-context)
;;; tether-context.el ends here
