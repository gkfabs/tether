;;; check-surface.el --- Fail on magit/forge/ghub symbols not allowlisted  -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Usage: emacs -Q --batch -L . -l scripts/check-surface.el
;;
;; Reads every tether*.el form (strings and comments are not symbols,
;; so docstrings may mention anything) and collects the symbols starting
;; with magit-, forge- or ghub-.  Fails when one is internal (contains
;; "--") or missing from the allowlist in tether-deps.el.

;;; Code:

(require 'tether-deps)

(defconst check-surface-prefix-regexp "\\`\\(?:magit\\|forge\\|ghub\\)-"
  "Symbols with this prefix belong to magit, forge or ghub.")

(defun check-surface--symbols (form)
  "Return the external symbols in FORM."
  (let (found)
    (named-let walk ((x form))
      (cond ((symbolp x)
             (when (string-match-p check-surface-prefix-regexp (symbol-name x))
               (push x found)))
            ((consp x) (walk (car x)) (walk (cdr x)))
            ((vectorp x) (mapc #'walk x))))
    found))

(defun check-surface--file (file allowed)
  "Return a list of (LINE SYMBOL REASON) for FILE given ALLOWED symbols."
  (let (problems)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (forward-comment (buffer-size))
            (let* ((line (line-number-at-pos))
                   (form (read (current-buffer))))
              (dolist (sym (delete-dups (check-surface--symbols form)))
                (cond ((string-search "--" (symbol-name sym))
                       (push (list line sym "internal symbol") problems))
                      ((not (memq sym allowed))
                       (push (list line sym "not in the allowlist") problems))))))
        (end-of-file nil)))
    (nreverse problems)))

(let ((allowed (tether--external-symbols))
      (failed nil))
  (dolist (file (directory-files default-directory nil "\\`tether.*\\.el\\'"))
    (pcase-dolist (`(,line ,sym ,reason) (check-surface--file file allowed))
      (setq failed t)
      (message "%s:%d: `%s': %s" file line sym reason)))
  (if failed
      (kill-emacs 1)
    (message "check-surface: only allowlisted magit/forge/ghub symbols used")))

;;; check-surface.el ends here
