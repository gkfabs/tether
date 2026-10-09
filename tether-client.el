;;; tether-client.el --- HTTP requests through ghub with forge's token  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Every request goes through `tether--call' (SPEC.md §5.1):
;;
;; - the host is always the repository's `apihost', never ghub's
;;   default (problem 2 of the design notes);
;; - the token is forge's (`:auth 'forge'), so there is nothing extra
;;   to configure (problem 1);
;; - requests are asynchronous; `tether--call' waits for the reply with
;;   `accept-process-output' until `tether-request-timeout', because
;;   ghub has no timeout of its own.  \\[keyboard-quit] interrupts the
;;   wait.
;;
;; Errors become `user-error's that say what to do (token, rate limit,
;; wrong host).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-util)
(require 'ghub)
(require 'forge)

(defcustom tether-request-timeout 10
  "Seconds to wait for an answer from GitHub or GitLab."
  :type 'number
  :group 'tether)

(defun tether--forge-type (repo)
  "Return the ghub forge symbol of REPO: `github' or `gitlab'."
  (cl-typecase repo
    (forge-github-repository 'github)
    (forge-gitlab-repository 'gitlab)
    (t (user-error "Tether: %s repositories are not supported yet"
                   (eieio-object-class-name repo)))))

(defun tether--project-id (repo)
  "Return the URL-encoded GitLab project id of REPO, as `owner/name'.
The owner holds the sub-groups, which are encoded too."
  (url-hexify-string (format "%s/%s" (oref repo owner) (oref repo name))))

(defun tether--header (name headers)
  "Return the value of header NAME in HEADERS, ignoring case."
  (cdr (assoc-string name headers t)))

(defun tether--error-text (payload)
  "Return the error message in the API error PAYLOAD, or nil."
  (when (consp payload)
    (let-alist payload
      (let ((parts (delq nil
                         (list (and (stringp .message) .message)
                               (and (stringp .error) .error)
                               (and (stringp .error_description)
                                    .error_description)
                               (and .errors (format "%s" .errors))))))
        (and parts (string-join parts "; "))))))

(defun tether--rate-limit-reset (headers)
  "Return the time the rate limit in HEADERS ends, as HH:MM."
  (let ((reset (tether--header "x-ratelimit-reset" headers))
        (retry (tether--header "retry-after" headers)))
    (cond (reset (format-time-string "%H:%M" (string-to-number reset)))
          (retry (format-time-string
                  "%H:%M" (time-add nil (string-to-number retry))))
          (t "a few minutes"))))

(defun tether--request-error-message (host err headers)
  "Return the message for the error ERR from HOST, given response HEADERS.
ERR is what ghub passes to its errorback: (error http STATUS PAYLOAD)
for HTTP errors, (error CONNECTION-ERROR ...) otherwise."
  (pcase err
    (`(error http ,status . ,rest)
     (let* ((payload (car rest))
            (text (or (tether--error-text payload) "no details"))
            (type (or (tether--header "content-type" headers) "")))
       (cond
        ((or (eq status 429)
             (and (eq status 403)
                  (or (equal (tether--header "x-ratelimit-remaining" headers)
                             "0")
                      (string-match-p "rate limit" text))))
         (format "Tether: %s rate limit reached; retry after %s"
                 host (tether--rate-limit-reset headers)))
        ((string-prefix-p "text/html" type)
         (format "Tether: %s returned HTML, not JSON (HTTP %s): \
wrong API host, or a Cloudflare challenge?" host status))
        ((memq status '(401 403))
         (let ((scopes (tether--header "x-accepted-oauth-scopes" headers)))
           (format "Tether: %s rejected the token (HTTP %s: %s)%s; \
check forge's token, see (info \"(forge)Setup\")"
                   host status text
                   (if (and scopes (not (string-empty-p scopes)))
                       (format ", needs scopes: %s" scopes)
                     ""))))
        (t (format "Tether: %s answered HTTP %s: %s" host status text)))))
    (`(,_ . ,data)
     (format "Tether: cannot reach %s: %s" host
             (mapconcat (lambda (d) (format "%s" d)) data " ")))
    (_ (format "Tether: request to %s failed: %S" host err))))

(cl-defun tether--call (repo method resource &optional params &key payload)
  "Send METHOD RESOURCE with PARAMS or PAYLOAD to the API of REPO.
Wait for the answer and return a plist:
  (:ok t :value VALUE :headers HEADERS) on success,
  (:ok nil :status STATUS :payload PAYLOAD :headers HEADERS :message MSG)
  on an HTTP or connection error, STATUS being nil for the latter.
Signal a `user-error' when no answer arrives within
`tether-request-timeout' seconds."
  (let ((host (oref repo apihost))
        (result nil))
    (message "Tether: contacting %s..." host)
    (ghub-request method resource params
                  :payload payload
                  :host host
                  :auth 'forge
                  :forge (tether--forge-type repo)
                  :callback (lambda (value headers &rest _)
                              (setq result (list :ok t :value value
                                                 :headers headers)))
                  :errorback (lambda (err headers &rest _)
                               (setq result
                                     (list :ok nil
                                           :status (pcase err
                                                     (`(error http ,s . ,_) s))
                                           :payload (pcase err
                                                      (`(error http ,_ ,p . ,_) p))
                                           :headers headers
                                           :message (tether--request-error-message
                                                     host err headers)))))
    (let ((deadline (+ (float-time) tether-request-timeout)))
      (while (and (not result) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (unless result
      (user-error "Tether: %s did not answer within %s seconds"
                  host tether-request-timeout))
    (message "Tether: contacting %s...done" host)
    result))

(cl-defun tether--request (repo method resource &optional params &key payload)
  "Send METHOD RESOURCE to the API of REPO and return the answer.
PARAMS and PAYLOAD are passed to `ghub-request'.  Signal a
`user-error' explaining the problem when the request fails; use
`tether--call' to handle failures yourself."
  (let ((result (tether--call repo method resource params :payload payload)))
    (if (plist-get result :ok)
        (plist-get result :value)
      (user-error "%s" (plist-get result :message)))))

;;;; Per-forge operations (SPEC.md §5.2)

(cl-defgeneric tether--fetch-diff-refs (repo pullreq)
  "Return the commits of PULLREQ of REPO according to the API.
The value is a plist:
  :head      the PR head commit;
  :base      the diff base, or nil when it must be computed locally
             as the merge-base of :base-tip and :head (GitHub);
  :start     GitLab's start_sha, or nil;
  :base-tip  the tip of the target branch, or nil;
  :base-ref  the name of the target branch.")

(cl-defgeneric tether--pullreq-ref (repo number)
  "Return the ref under which REPO's server publishes PR NUMBER's head.")

(cl-defgeneric tether--pr-prefix (repo)
  "Return the character that prefixes PR numbers on REPO's forge.")

(cl-defmethod tether--fetch-diff-refs ((repo forge-repository) _pullreq)
  "Signal that REPO's forge is not supported."
  (tether--forge-type repo))

(provide 'tether-client)
;;; tether-client.el ends here
