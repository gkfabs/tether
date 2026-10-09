;;; tether-client.el --- HTTP requests through ghub with forge's token  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Every request goes through `tether--call':
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
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'ghub)
(require 'forge)
(require 'tether-diff)

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

(defun tether--wait (host send)
  "Call SEND with a function to report the result, and wait for it.
SEND starts an asynchronous request to HOST and arranges for the
function it receives to be called with the result plist.  Return
that plist.  Signal a `user-error' when it does not arrive within
`tether-request-timeout' seconds."
  (let ((result nil))
    (message "Tether: contacting %s..." host)
    (funcall send (lambda (plist) (setq result plist)))
    (let ((deadline (+ (float-time) tether-request-timeout)))
      (while (and (not result) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (unless result
      (user-error "Tether: %s did not answer within %s seconds"
                  host tether-request-timeout))
    (message "Tether: contacting %s...done" host)
    result))

(defun tether--error-result (host err headers)
  "Return the result plist for the error ERR from HOST with HEADERS."
  (list :ok nil
        :status (pcase err (`(error http ,s . ,_) s))
        :payload (pcase err (`(error http ,_ ,p . ,_) p))
        :headers headers
        :message (tether--request-error-message host err headers)))

(cl-defun tether--call (repo method resource &optional params &key payload)
  "Send METHOD RESOURCE with PARAMS or PAYLOAD to the API of REPO.
Wait for the answer and return a plist:
  (:ok t :value VALUE :headers HEADERS) on success,
  (:ok nil :status STATUS :payload PAYLOAD :headers HEADERS :message MSG)
  on an HTTP or connection error, STATUS being nil for the latter.
Signal a `user-error' when no answer arrives within
`tether-request-timeout' seconds."
  (let ((host (oref repo apihost)))
    (tether--wait
     host
     (lambda (done)
       (ghub-request method resource params
                     :payload payload
                     :host host
                     :auth 'forge
                     :forge (tether--forge-type repo)
                     :callback (lambda (value headers &rest _)
                                 (funcall done (list :ok t :value value
                                                     :headers headers)))
                     :errorback (lambda (err headers &rest _)
                                  (funcall done (tether--error-result
                                                 host err headers))))))))

(defun tether--graphql-errors-text (errors)
  "Return the messages of the GraphQL ERRORS, joined."
  (mapconcat (lambda (e) (or (and (consp e) (alist-get 'message e))
                             (format "%s" e)))
             errors "; "))

(defun tether--query-call (repo query &optional variables)
  "Send the GraphQL QUERY with VARIABLES to REPO's API and wait.
QUERY is in ghub's s-expression syntax (see `ghub-query').  Return
\=(:ok t :value DATA) or (:ok nil :errors ERRORS :message MSG)."
  (let ((host (oref repo apihost)))
    (tether--wait
     host
     (lambda (done)
       (ghub-query query variables
         :host host
         :auth 'forge
         :forge (tether--forge-type repo)
         :callback (lambda (value &rest _)
                     (funcall done (list :ok t :value value)))
         :errorback (lambda (errors headers &rest _)
                      (funcall done
                               (if (eq (car-safe errors) 'error)
                                   (tether--error-result host errors headers)
                                 (list :ok nil :errors errors
                                       :message (format "Tether: %s: %s" host
                                                        (tether--graphql-errors-text
                                                         errors)))))))))))

(defun tether--query (repo query &optional variables)
  "Send the GraphQL QUERY with VARIABLES to REPO's API; return its data.
Signal a `user-error' explaining the problem when it fails."
  (let ((result (tether--query-call repo query variables)))
    (if (plist-get result :ok)
        (plist-get result :value)
      (user-error "%s" (plist-get result :message)))))

(cl-defun tether--request (repo method resource &optional params &key payload)
  "Send METHOD RESOURCE to the API of REPO and return the answer.
PARAMS and PAYLOAD are passed to `ghub-request'.  Signal a
`user-error' explaining the problem when the request fails; use
`tether--call' to handle failures yourself."
  (let ((result (tether--call repo method resource params :payload payload)))
    (if (plist-get result :ok)
        (plist-get result :value)
      (user-error "%s" (plist-get result :message)))))

;;;; Per-forge operations

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

(cl-defgeneric tether--post-comment (repo ctx target body)
  "Post BODY on TARGET of the pull request of CTX on REPO.
TARGET is a plist:
  :path      the file, as in the PR head;
  :old-path  its path on the base side;
  :start, :end  the first and last `tether-line', or nil for a
             comment on the whole file;
  :side      `right' (head) or `left' (base).
Return a plist with :url, the comment's web page, and :message, what
to tell the user.")

(defun tether--compact (alist)
  "Return ALIST without its entries whose value is nil.
`ghub-request' would send nil as JSON false."
  (seq-remove (lambda (entry) (null (cdr entry))) alist))

(defun tether--target-label (target)
  "Return how to name TARGET in messages, like a.el:42 or a.el:40-42."
  (let* ((side (plist-get target :side))
         (number (lambda (line)
                   (if (eq side 'left) (tether-line-old line)
                     (tether-line-new line))))
         (start (plist-get target :start))
         (end (plist-get target :end)))
    (concat
     (cond ((null end) (plist-get target :path))
           ((eq start end)
            (format "%s:%d" (plist-get target :path) (funcall number end)))
           (t (format "%s:%d-%d" (plist-get target :path)
                      (funcall number start) (funcall number end))))
     (and end (eq side 'left) " (removed)"))))

(defun tether--rejected (service target result)
  "Signal that SERVICE refused TARGET, as told by the failed RESULT.
When the forge says the line is not in its diff, explain that its
diff and git's can differ; the caller keeps the text."
  (let* ((text (or (tether--error-text (plist-get result :payload))
                   (plist-get result :message)))
         (diff-p (and (memq (plist-get result :status) '(400 422))
                      (string-match-p "diff\\|line\\|position" text))))
    (user-error "Tether: %s rejected %s: %s%s"
                service (tether--target-label target) text
                (if diff-p " (its diff and git's differ here)" ""))))

(cl-defmethod tether--fetch-diff-refs ((repo forge-repository) _pullreq)
  "Signal that REPO's forge is not supported."
  (tether--forge-type repo))

(provide 'tether-client)
;;; tether-client.el ends here
