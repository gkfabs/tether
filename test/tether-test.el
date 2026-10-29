;;; tether-test.el --- Tests for tether  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Unit tests that need no network.  Feature tests are added with each
;; step; this file only checks that the package loads.

;;; Code:

(require 'ert)
(require 'tether)

(ert-deftest tether-test-mode-toggles ()
  (with-temp-buffer
    (tether-mode 1)
    (should tether-mode)
    (tether-mode -1)
    (should-not tether-mode)))

(ert-deftest tether-test-version-matches-header ()
  "`tether-version' and the Version header must agree."
  (let ((file (locate-library "tether.el" t)))
    (should file)
    (with-temp-buffer
      (insert-file-contents file)
      (should (re-search-forward "^;; Version: \\(.+\\)$" nil t))
      (should (equal (match-string 1) tether-version)))))

(ert-deftest tether-test-former-names ()
  "The commands renamed in 0.2.0 still work under their former names."
  (pcase-dolist (`(,old . ,new)
                 '((tether-nitpick . tether-comment-line)
                   (tether-diff . tether-visit-diff)
                   (tether-threads . tether-thread-list)
                   (tether-reply . tether-thread-reply)
                   (tether-refresh . tether-pullreq-refresh)))
    (should (commandp new))
    (should (eq (indirect-function old) (indirect-function new)))
    (should (get old 'byte-obsolete-info))))

(provide 'tether-test)
;;; tether-test.el ends here
