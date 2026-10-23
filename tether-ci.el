;;; tether-ci.el --- CI status of pull requests  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; Assisted-by: Claude:claude-opus-5-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The status of the CI on the head of each open pull request: GitLab's
;; head pipeline, GitHub's checks.
;;
;; - at the end of each pull request line of forge's lists, in
;;   magit-status and in forge's own list buffers;
;; - in the mode line of `tether-mode';
;; - `tether-browse-ci' opens it in the browser.
;;
;; Fetched only when asked: `g' in a buffer listing pull requests, or
;; `tether-refresh'.  Never when a buffer is refreshed by itself, after
;; a commit for example.  One request per repository, for all its open
;; pull requests.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'forge)
(require 'tether-client)
(require 'tether-context)

(defcustom tether-ci-status t
  "Whether `g' in `magit-status' also fetches the CI status of pull requests.
It shows at the end of each pull request line and in the mode line.
`tether-refresh' fetches it too.  Nothing else does."
  :type 'boolean
  :group 'tether)

(defface tether-ci-success
  '((t :inherit success))
  "CI mark of a pull request whose pipeline or checks passed."
  :group 'tether)

(defface tether-ci-failure
  '((t :inherit error))
  "CI mark of a pull request whose pipeline or checks failed."
  :group 'tether)

(defface tether-ci-running
  '((t :inherit warning))
  "CI mark of a pull request whose pipeline or checks are not done."
  :group 'tether)

(defface tether-ci-canceled
  '((t :inherit shadow))
  "CI mark of a pull request whose pipeline was canceled or skipped."
  :group 'tether)

(defconst tether-ci--marks
  '((success "✓" tether-ci-success)
    (failure "✗" tether-ci-failure)
    (running "●" tether-ci-running)
    (canceled "⊘" tether-ci-canceled))
  "Mark and face of each CI state.")

(defvar tether-ci--statuses (make-hash-table :test #'equal)
  "CI status of pull requests, by (REPO-ID . NUMBER).
Each value is a plist, as described in `tether--fetch-ci'.")

(defvar tether-ci--fetching nil
  "Ids of the repositories whose CI status is being fetched.")

(defvar tether-ci-functions nil
  "Functions called with a repository whose CI status just arrived.")

;;;; Status

(defun tether-ci-mark (repo-id number)
  "Return the CI mark of pull request NUMBER of repository REPO-ID, or nil."
  (when-let* ((status (gethash (cons repo-id number) tether-ci--statuses))
              (state (plist-get status :state)))
    (pcase-let ((`(,mark ,face) (alist-get state tether-ci--marks))
                (sha (plist-get status :sha)))
      (propertize mark 'face face
                  'help-echo (format "CI: %s%s" state
                                     (if sha (format " on %s" (substring sha 0 7))
                                       ""))))))

(defun tether-ci--store (repo-id statuses)
  "Make STATUSES the CI status of the pull requests of REPO-ID.
Those of the pull requests not in STATUSES are forgotten."
  (maphash (lambda (key _)
             (when (equal (car key) repo-id)
               (remhash key tether-ci--statuses)))
           tether-ci--statuses)
  (dolist (status statuses)
    (puthash (cons repo-id (plist-get status :number)) status
             tether-ci--statuses)))

(defun tether-ci-fetch (repo)
  "Fetch the CI status of REPO's open pull requests, in the background.
When it arrives, show it, then run `tether-ci-functions'."
  (let ((id (oref repo id)))
    (unless (member id tether-ci--fetching)
      (push id tether-ci--fetching)
      (condition-case err
          (tether--fetch-ci
           repo
           (lambda (result)
             (setq tether-ci--fetching (delete id tether-ci--fetching))
             (if (not (plist-get result :ok))
                 (message "Tether: cannot fetch the CI status: %s"
                          (string-remove-prefix "Tether: "
                                                (plist-get result :message)))
               (tether-ci--store id (plist-get result :statuses))
               (tether-ci--redraw)
               (run-hook-with-args 'tether-ci-functions repo))))
        (error
         (setq tether-ci--fetching (delete id tether-ci--fetching))
         (message "Tether: cannot fetch the CI status: %s"
                  (error-message-string err)))))))

;;;; In lists of pull requests

(defun tether-ci--pullreq-sections (section)
  "Return the sections of forge pull requests in SECTION, itself included."
  (if (and (magit-section-match 'pullreq section)
           (cl-typep (oref section value) 'forge-pullreq))
      (list section)
    (mapcan #'tether-ci--pullreq-sections (oref section children))))

(defun tether-ci--draw (&optional sections)
  "Show the CI status at the end of the pull request lines of the buffer.
SECTIONS are the pull request sections, when already found."
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) 'tether-ci t)
    (dolist (section (or sections
                         (and magit-root-section
                              (tether-ci--pullreq-sections magit-root-section))))
      (let ((pullreq (oref section value)))
        (when-let* ((mark (tether-ci-mark (oref pullreq repository)
                                          (oref pullreq number))))
          (save-excursion
            (goto-char (oref section start))
            (let ((overlay (make-overlay (line-end-position) (line-end-position))))
              (overlay-put overlay 'tether-ci t)
              (overlay-put overlay 'after-string (concat " " mark)))))))))

(defun tether-ci--redraw ()
  "Show the CI status again in every buffer listing pull requests."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'magit-section-mode) magit-root-section)
        (tether-ci--draw)))))

(defun tether-ci--magit-refreshed ()
  "Show the CI status in the pull request lines of the current buffer.
When the user asked for the refresh with `g', fetch it first."
  (with-demoted-errors "Tether: %S"
    (when-let* ((sections (and magit-root-section
                               (tether-ci--pullreq-sections magit-root-section))))
      (when (and tether-ci-status
                 (memq this-command '(magit-refresh magit-refresh-all)))
        (dolist (id (delete-dups
                     (mapcar (lambda (section)
                               (oref (oref section value) repository))
                             sections)))
          (when-let* ((repo (forge-get-repository :id id)))
            (tether-ci-fetch repo))))
      (tether-ci--draw sections))))

(defun tether-ci--refreshed (ctx)
  "Fetch the CI status of the repository of CTX, which was just refreshed."
  (when tether-ci-status
    (tether-ci-fetch (tether-context-repo ctx))))

(add-hook 'tether-refresh-functions #'tether-ci--refreshed)

;;;; In the browser

(declare-function tether--buffer-pr "tether")

(defun tether-ci--pullreq-at-point ()
  "Return (REPO . NUMBER) of the pull request at point or of the buffer."
  (if-let* ((pullreq (magit-section-value-if 'pullreq))
            (repo (forge-get-repository :id (oref pullreq repository))))
      (cons repo (oref pullreq number))
    (if-let* ((found (tether--buffer-pr)))
        (cons (car found) (oref (cdr found) number))
      (user-error "Tether: no pull request here"))))

;;;###autoload
(defun tether-browse-ci ()
  "Open the CI of the pull request in the browser.
GitLab's pipeline, GitHub's checks.  The pull request at point in a
list of pull requests, else the current buffer's."
  (interactive)
  (pcase-let* ((`(,repo . ,number) (tether-ci--pullreq-at-point))
               (status (gethash (cons (oref repo id) number) tether-ci--statuses)))
    (browse-url (or (plist-get status :url) (tether--ci-url repo number)))))

(provide 'tether-ci)
;;; tether-ci.el ends here
