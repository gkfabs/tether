;;; tether-review.el --- Draft reviews and verdicts  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A draft review: comments made after `tether-review-start' are drafts
;; only you see, published together by `tether-review-submit' as a
;; comment, an approval or a request for changes.  `tether-review-discard'
;; deletes them, `tether-approve' approves without a review.
;;
;; The drafts live on the forge: a pending review on GitHub (also the
;; one started on github.com), draft notes on GitLab (also those made in
;; the web interface).  GitLab has no review object, so the review
;; started with tether but still without drafts is remembered here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tether-context)

(defvar tether-review-functions nil
  "Functions called with a context whose draft review state was fetched.")

(defvar tether--started-reviews nil
  "Contexts keys, (TOPLEVEL . NUMBER), with a review started by tether.
Only matters on GitLab, which has no review object until a draft
exists.")

(defun tether--review-active-p (ctx)
  "Return non-nil when a draft review of CTX's pull request is in progress."
  (plist-get (tether-context-review ctx) :active))

(defun tether--store-review (ctx review)
  "Store REVIEW, as fetched from the forge, in CTX, with whether it is active.
Active means drafts exist, or a review was started (GitHub's pending
review, or one started with tether on GitLab)."
  (setf (tether-context-review ctx)
        (plist-put review :active
                   (and (or (plist-get review :id)
                            (member (tether--context-key ctx) tether--started-reviews)
                            (> (or (plist-get review :drafts) 0) 0))
                        t))))

(defun tether-fetch-review (ctx)
  "Fetch the user's draft review of CTX's pull request in the background.
When it arrives, if CTX is still current, run `tether-review-functions'."
  (tether--review-state
   (tether-context-repo ctx) ctx
   (lambda (result)
     (when (and (plist-get result :ok) (tether-context-current-p ctx))
       (tether--store-review ctx (plist-get result :review))
       (run-hook-with-args 'tether-review-functions ctx)))))

(defun tether--review-done (ctx)
  "Forget the draft review of CTX, after it was submitted or discarded."
  (setq tether--started-reviews
        (delete (tether--context-key ctx) tether--started-reviews))
  (setf (tether-context-review ctx) (list :drafts 0)))

(defun tether--drafts-note (ctx)
  "Return how many drafts CTX's review has, in words."
  (let ((drafts (or (plist-get (tether-context-review ctx) :drafts) 0)))
    (format "%d draft comment%s" drafts (if (= drafts 1) "" "s"))))

;;;###autoload
(defun tether-review-start ()
  "Start a draft review: the next comments are drafts until submitted.
Submit them with `tether-review-submit', or delete them with
`tether-review-discard'."
  (interactive)
  (let ((ctx (tether-context t)))
    (tether--wait-for-review ctx)
    (if (tether--review-active-p ctx)
        (message "Tether: a review is already in progress (%s)"
                 (tether--drafts-note ctx))
      (let ((id (tether--review-start (tether-context-repo ctx) ctx)))
        (cl-pushnew (tether--context-key ctx) tether--started-reviews
                    :test #'equal)
        (setf (tether-context-review ctx) (list :active t :id id :drafts 0))
        (run-hook-with-args 'tether-review-functions ctx)
        (message "Tether: review started; comments are drafts until \
M-x tether-review-submit")))))

(defun tether--wait-for-review (ctx)
  "Make sure CTX's draft review state is known, fetching it if needed."
  (unless (tether-context-review ctx)
    (let ((result (tether--wait (oref (tether-context-repo ctx) apihost)
                                (lambda (done)
                                  (tether--review-state (tether-context-repo ctx)
                                                        ctx done)))))
      (when (plist-get result :ok)
        (tether--store-review ctx (plist-get result :review))))))

(declare-function tether--edit-comment "tether-comment")
(declare-function tether-fetch-threads "tether-threads")

;;;###autoload
(defun tether-review-submit (verdict)
  "Publish the draft review, as VERDICT: comment, approval or change request.
The summary is written in the comment editor and may stay empty.
Without drafts, gives just the verdict."
  (interactive
   (list (cdr (assoc (completing-read "Submit the review as: "
                                      '("comment" "approve" "request changes")
                                      nil t)
                     '(("comment" . comment) ("approve" . approve)
                       ("request changes" . request-changes))))))
  (let* ((ctx (tether-context t))
         (target (list :review verdict)))
    (tether--wait-for-review ctx)
    (tether--edit-comment ctx target (lambda (_) target))))

(defun tether--submit-review (ctx verdict body)
  "Submit CTX's draft review as VERDICT with summary BODY; return a plist.
Called by the comment editor when sending a review target."
  (let ((message (tether--review-submit (tether-context-repo ctx) ctx verdict body)))
    (tether--review-done ctx)
    (run-hook-with-args 'tether-review-functions ctx)
    (tether-fetch-threads ctx)
    (list :message message)))

;;;###autoload
(defun tether-review-discard ()
  "Delete the draft review and its draft comments, after confirmation."
  (interactive)
  (let ((ctx (tether-context t)))
    (tether--wait-for-review ctx)
    (unless (tether--review-active-p ctx)
      (user-error "Tether: no review in progress"))
    (when (yes-or-no-p (format "Delete your review and its %s? "
                               (tether--drafts-note ctx)))
      (tether--review-discard (tether-context-repo ctx) ctx)
      (tether--review-done ctx)
      (run-hook-with-args 'tether-review-functions ctx)
      (message "Tether: review discarded"))))

;;;###autoload
(defun tether-approve ()
  "Approve the pull request.
During a draft review, offer to submit it with the approval."
  (interactive)
  (let ((ctx (tether-context t)))
    (tether--wait-for-review ctx)
    (if (and (tether--review-active-p ctx)
             (or (y-or-n-p (format "Submit your review (%s) with the approval? "
                                   (tether--drafts-note ctx)))
                 (and (cl-typep (tether-context-repo ctx) 'forge-github-repository)
                      (user-error "Tether: on GitHub, approving goes through your \
pending review; submit or discard it first"))))
        (message "Tether: %s" (plist-get (tether--submit-review ctx 'approve "")
                                         :message))
      (tether--approve (tether-context-repo ctx) ctx)
      (message "Tether: approved %s"
               (tether--label (tether-context-repo ctx)
                              (oref (tether-context-pullreq ctx) number))))))

(provide 'tether-review)
;;; tether-review.el ends here
