;;; tether-deps.el --- What tether uses from magit, forge and ghub  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The single source of truth for every symbol tether uses from magit,
;; forge and ghub (SPEC.md §2.1).  It is used three ways:
;;
;; - `tether-check-dependencies' verifies at runtime that the installed
;;   packages still provide them, and warns once otherwise;
;; - the contract tests in test/tether-deps-test.el run the same checks
;;   in CI against the "min" and "latest" dependency sets;
;; - scripts/check-surface.el fails when tether's code uses a
;;   magit/forge/ghub symbol that is not listed here.
;;
;; Adding a symbol here needs a reason in SPEC.md §2.1.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst tether--external-functions
  '(;; forge
    forge-browse-pullreq
    forge-current-pullreq
    forge-dispatch
    forge-get-pullreq
    forge-get-repository
    forge-pull
    forge-read-pullreq
    forge-visit-pullreq
    forge-visit-topic-from-url
    ;; ghub
    ghub-query
    ghub-request
    ;; magit
    magit-blob-mode
    magit-call-git
    magit-current-section
    magit-diff-range
    magit-diff-visit-worktree-file
    magit-dispatch
    magit-file-relative-name
    magit-find-file-noselect
    magit-get
    magit-get-current-branch
    magit-get-upstream-branch
    magit-git-insert
    magit-git-lines
    magit-git-string
    magit-merge-base
    magit-rev-ancestor-p
    magit-rev-parse
    magit-rev-verify
    magit-split-branch-name
    magit-status-setup-buffer
    magit-toplevel)
  "Functions, macros and commands tether calls from magit, forge and ghub.")

(defconst tether--external-variables
  '(ghub-response-headers
    magit-blob-mode-hook
    magit-buffer-diff-range
    magit-post-refresh-hook
    magit-root-section)
  "Variables tether reads or hooks into from magit, forge and ghub.")

(defconst tether--external-classes
  '((forge-repository id owner name apihost githost remote pullreqs)
    (forge-github-repository)
    (forge-gitlab-repository)
    (forge-pullreq id repository number title state author base-ref head-ref
                   base-rev head-rev head-repo their-id cross-repo-p
                   locked-p))
  "Classes tether dispatches on, each followed by the slots it reads.")

(defconst tether--external-other
  '(forge-topic-mode magit-mode ghub-error)
  "Other symbols tether names: modes for `derived-mode-p', error symbols.")

(defconst tether--external-keyword-args
  '((ghub-request :auth :host :forge :username :payload
                  :callback :errorback :noerror)
    (ghub-query :auth :host :forge :username
                :callback :errorback :noerror :paginate))
  "Keyword arguments tether passes, by function.")

(defconst tether--external-methods
  '((forge-get-pullreq (eql :branch)))
  "Generic function methods tether relies on, with their specializer.")

(defun tether--external-symbols ()
  "Return every magit, forge and ghub symbol tether may use."
  (append tether--external-functions
          tether--external-variables
          (mapcar #'car tether--external-classes)
          tether--external-other))

(defun tether--keyword-args (function)
  "Return the keyword arguments FUNCTION accepts, as keywords.
Read from the usage line of its docstring, which `cl-defun' keeps."
  (when-let* ((doc (documentation function t))
              (usage (car (help-split-fundoc doc function)))
              (arglist (cdr (car (read-from-string usage))))
              (keys (cdr (memq '&key arglist))))
    (mapcar (lambda (arg)
              (intern (concat ":" (downcase (symbol-name
                                             (if (consp arg) (car arg) arg))))))
            (seq-take-while (lambda (arg)
                              (not (memq arg '(&allow-other-keys &aux))))
                            keys))))

(defun tether--method-exists-p (generic specializer)
  "Return non-nil if GENERIC has a method whose first argument is SPECIALIZER."
  (and (fboundp generic)
       (cl-find-method generic nil (list specializer))
       t))

(defun tether--dependency-problems ()
  "Return a list of strings describing missing dependencies.
Load magit, forge and ghub first.  An empty list means everything
tether uses is present."
  (let (problems)
    (dolist (fn tether--external-functions)
      (unless (fboundp fn)
        (push (format "function `%s' is not defined" fn) problems)))
    (dolist (var tether--external-variables)
      (unless (boundp var)
        (push (format "variable `%s' is not defined" var) problems)))
    (pcase-dolist (`(,class . ,slots) tether--external-classes)
      (if (not (find-class class nil))
          (push (format "class `%s' is not defined" class) problems)
        (dolist (slot slots)
          (unless (slot-exists-p class slot)
            (push (format "class `%s' has no slot `%s'" class slot)
                  problems)))))
    (pcase-dolist (`(,fn . ,keys) tether--external-keyword-args)
      (when (fboundp fn)
        (let ((accepted (tether--keyword-args fn)))
          (dolist (key keys)
            (unless (memq key accepted)
              (push (format "`%s' no longer accepts %s" fn key) problems))))))
    (pcase-dolist (`(,generic ,specializer) tether--external-methods)
      (unless (tether--method-exists-p generic specializer)
        (push (format "`%s' has no method for %S" generic specializer)
              problems)))
    (nreverse problems)))

(defvar tether--dependencies-checked nil
  "Non-nil once `tether-check-dependencies' has run in this session.")

(defun tether-check-dependencies (&optional interactive)
  "Check that installed magit, forge and ghub still provide what tether needs.
Warn once per session when something is missing.  With INTERACTIVE
non-nil, as when called interactively, always report the result.
Return non-nil when nothing is missing."
  (interactive (list t))
  (require 'magit)
  (require 'forge)
  (require 'ghub)
  (let ((problems (tether--dependency-problems)))
    (cond
     ((and problems (or interactive (not tether--dependencies-checked)))
      (display-warning
       'tether
       (format "Installed magit/forge/ghub changed in a way tether \
does not support; please report at https://github.com/gkfabs/tether:\n- %s"
               (string-join problems "\n- "))))
     (interactive
      (message "tether: magit, forge and ghub provide everything tether uses")))
    (setq tether--dependencies-checked t)
    (null problems)))

(provide 'tether-deps)
;;; tether-deps.el ends here
