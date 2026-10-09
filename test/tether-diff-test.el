;;; tether-diff-test.el --- Tests for tether-diff  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The parser against real git output (test/fixtures/*.diff, made with
;; the options of `tether--diff-options'), and the drift mapping.

;;; Code:

(require 'ert)
(require 'tether-diff)

(defconst tether-diff-test--fixtures
  (expand-file-name "fixtures/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory of the diff fixtures.")

(defun tether-diff-test--read (name)
  "Return the contents of fixture NAME, decoded as UTF-8."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents (expand-file-name name tether-diff-test--fixtures)))
    (buffer-string)))

(defun tether-diff-test--map (name)
  "Return the single file map of fixture NAME."
  (let ((maps (tether-diff-parse (tether-diff-test--read name))))
    (should (= (length maps) 1))
    (car maps)))

(defun tether-diff-test--line (map new)
  "Return (KIND OLD NEW POS) of head line NEW in MAP."
  (when-let* ((line (tether-file-map-line map new)))
    (list (tether-line-kind line) (tether-line-old line)
          (tether-line-new line) (tether-line-pos line))))

(ert-deftest tether-diff-test-modified ()
  "Two hunks with changes, a deletion and an insertion."
  (let ((map (tether-diff-test--map "modified.diff")))
    (should (eq (tether-file-map-status map) :modified))
    (should (equal (tether-file-map-old-path map) "mod.txt"))
    (should (equal (tether-file-map-new-path map) "mod.txt"))
    (should (= (length (tether-file-map-hunks map)) 2))
    (should (equal (tether-diff-test--line map 1) '(:context 1 1 1)))
    (should (equal (tether-diff-test--line map 3) '(:added nil 3 4)))
    ;; Old line 10 was deleted, so old 11 is new 10.
    (should (equal (tether-diff-test--line map 10) '(:context 11 10 12)))
    (should (equal (tether-diff-test--line map 16) '(:added nil 16 18)))
    (should (equal (tether-diff-test--line map 19) '(:context 19 19 21)))
    ;; The second @@ line is position 22.
    (should (equal (tether-diff-test--line map 22) '(:context 22 22 23)))
    (should (equal (tether-diff-test--line map 25) '(:added nil 25 27)))
    (should (eq (tether-line-kind (gethash 25 (tether-file-map-by-old map)))
                :removed))
    (should-not (tether-file-map-line map 20))
    (should (= (length (tether-file-map-commentable map)) 26))))

(ert-deftest tether-diff-test-regions ()
  (let ((map (tether-diff-test--map "modified.diff")))
    (should-not (tether-file-map-region-problem map 2 4))
    (should (equal (mapcar #'tether-line-new
                           (let ((r (tether-file-map-region map 2 4)))
                             (list (car r) (cdr r))))
                   '(2 4)))
    (should (equal (tether-file-map-region-problem map 18 23)
                   "lines 18-23 span two hunks of the pull request diff"))
    (should (equal (tether-file-map-region-problem map 20 20)
                   "line 20 is not in the pull request diff"))
    (should-error (tether-file-map-region map 20 20) :type 'user-error)
    (should (equal (tether-file-map-region-problem nil 1 1)
                   "the file is not changed in the pull request"))))

(ert-deftest tether-diff-test-added ()
  (let ((map (tether-diff-test--map "added.diff")))
    (should (eq (tether-file-map-status map) :added))
    (should-not (tether-file-map-old-path map))
    (should (equal (tether-file-map-new-path map) "added.txt"))
    (should (equal (tether-diff-test--line map 2) '(:added nil 2 2)))))

(ert-deftest tether-diff-test-deleted ()
  (let ((map (tether-diff-test--map "deleted.diff")))
    (should (eq (tether-file-map-status map) :deleted))
    (should (equal (tether-file-map-old-path map) "deleted.txt"))
    (should-not (tether-file-map-new-path map))
    (should-not (tether-file-map-commentable map))
    (should (eq (tether-line-kind (gethash 1 (tether-file-map-by-old map)))
                :removed))))

(ert-deftest tether-diff-test-rename-with-edits ()
  (let ((map (tether-diff-test--map "rename-edit.diff")))
    (should (eq (tether-file-map-status map) :renamed))
    (should (equal (tether-file-map-old-path map) "renamed-edit.txt"))
    (should (equal (tether-file-map-new-path map) "moved-edit.txt"))
    (should (equal (tether-diff-test--line map 10) '(:added nil 10 5)))))

(ert-deftest tether-diff-test-rename-only ()
  "A pure rename has no hunks, so no line can take a comment."
  (let ((map (tether-diff-test--map "rename-only.diff")))
    (should (eq (tether-file-map-status map) :renamed))
    (should (equal (tether-file-map-old-path map) "renamed-only.txt"))
    (should (equal (tether-file-map-new-path map) "moved-only.txt"))
    (should-not (tether-file-map-hunks map))
    (should (equal (tether-file-map-region-problem map 1 1)
                   "line 1 is not in the pull request diff"))))

(ert-deftest tether-diff-test-no-newline ()
  "The \"\\ No newline\" marker is not a line but has a position."
  (let ((map (tether-diff-test--map "no-newline.diff")))
    (should (equal (tether-diff-test--line map 2) '(:added nil 2 3)))
    (should (= (length (tether-hunk-lines (car (tether-file-map-hunks map))))
               3))))

(ert-deftest tether-diff-test-mode-change ()
  (let ((map (tether-diff-test--map "mode-change.diff")))
    (should (equal (tether-file-map-new-path map) "script.sh"))
    (should-not (tether-file-map-hunks map))))

(ert-deftest tether-diff-test-binary ()
  (let ((map (tether-diff-test--map "binary.diff")))
    (should (eq (tether-file-map-status map) :binary))
    (should-not (tether-file-map-commentable map))
    (should (equal (tether-file-map-region-problem map 1 1)
                   "the file is binary"))))

(ert-deftest tether-diff-test-quoted-path ()
  "Paths with spaces and non-ASCII characters are quoted by git."
  (let ((map (tether-diff-test--map "quoted-path.diff")))
    (should (equal (tether-file-map-old-path map) "spa ce é.txt"))
    (should (equal (tether-file-map-new-path map) "spa ce é.txt"))
    (should (equal (tether-diff-test--line map 1) '(:added nil 1 2)))))

(ert-deftest tether-diff-test-several-files ()
  (let ((maps (tether-diff-parse
               (concat (tether-diff-test--read "added.diff")
                       (tether-diff-test--read "modified.diff")))))
    (should (equal (mapcar #'tether-file-map-new-path maps)
                   '("added.txt" "mod.txt")))))

(ert-deftest tether-diff-test-name-status ()
  (should (equal (tether-diff-parse-name-status
                  (tether-diff-test--read "name-status.nul"))
                 '(("added.txt" "added.txt" . :added)
                   ("blob.bin" "blob.bin" . :modified)
                   ("deleted.txt" "deleted.txt" . :deleted)
                   ("mod.txt" "mod.txt" . :modified)
                   ("moved-edit.txt" "renamed-edit.txt" . :renamed)
                   ("moved-only.txt" "renamed-only.txt" . :renamed)
                   ("noeol.txt" "noeol.txt" . :modified)
                   ("script.sh" "script.sh" . :modified)
                   ("spa ce é.txt" "spa ce é.txt" . :modified)))))

(ert-deftest tether-diff-test-drift ()
  "Lines move through insertions and deletions; changed lines map to nil."
  ;; From a 10-line file to: line 3 modified, 1 line inserted after
  ;; line 5, line 8 deleted.
  (let ((drift (tether-diff-parse-drift
                "@@ -3 +3 @@\n-x\n+y\n@@ -5,0 +6 @@\n+new\n@@ -8 +8,0 @@\n-gone\n")))
    (should (equal drift '((3 1 3 1) (5 0 6 1) (8 1 8 0))))
    ;; TO -> FROM
    (should (equal (mapcar (lambda (l) (tether-diff-drift-from drift l))
                           '(1 2 3 4 5 6 7 8 9 10))
                   '(1 2 nil 4 5 nil 6 7 9 10)))
    ;; FROM -> TO
    (should (equal (mapcar (lambda (l) (tether-diff-drift-to drift l))
                           '(1 2 3 4 5 6 7 8 9 10))
                   '(1 2 nil 4 5 7 8 nil 9 10)))
    (should (= (tether-diff-drift-from nil 42) 42))))

(provide 'tether-diff-test)
;;; tether-diff-test.el ends here
