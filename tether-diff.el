;;; tether-diff.el --- Parse diffs into line maps  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; Assisted-by: Claude:claude-opus-5-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pure functions from git's diff output to the structures tether
;; comments with: which lines of a file are in the pull
;; request diff, on which side, in which hunk.  Also the mapping
;; through local edits between a buffer line and a PR head line.
;;
;; No git, magit or forge here: text in, structures out.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct (tether-line (:copier nil))
  "One line of a file's diff."
  kind   ; :context, :added or :removed
  old    ; line number on the base side, nil when added
  new    ; line number on the head side, nil when removed
  pos    ; 1-based position below the first @@ (GitHub's `position'),
         ; nil outside the hunks
  hunk   ; index of its hunk in the file's diff, from 0; nil outside
  code)  ; (OLD . NEW) line counters at this line, for GitLab's
         ; line_code: an added line has the old number the next old
         ; line would get, a removed line the next new number, as in
         ; GitLab's own diff parser

(cl-defstruct (tether-hunk (:copier nil))
  "One hunk of a file's diff."
  old-start old-count new-start new-count
  lines) ; vector of `tether-line', in diff order

(cl-defstruct (tether-file-map (:copier nil))
  "The diff of one file."
  old-path new-path
  status ; :modified, :added, :deleted, :renamed, :binary, :lfs
  hunks  ; list of `tether-hunk'
  by-new ; hash: head line number -> `tether-line'
  by-old); hash: base line number -> `tether-line'

;;;; Parsing

(defun tether-diff--unquote (path)
  "Return PATH as git wrote it, without C-style quotes and a/ or b/ prefix.
Return nil for /dev/null."
  (let* ((path (string-trim-right path "\t"))
         (path (if (string-prefix-p "\"" path)
                   (decode-coding-string
                    (string-to-unibyte (car (read-from-string path)))
                    'utf-8)
                 path)))
    (cond ((equal path "/dev/null") nil)
          ((string-match "\\`[ab]/" path) (substring path 2))
          (t path))))

(defun tether-diff--git-line-paths (line)
  "Return (OLD . NEW) from the diff --git LINE, or nil if ambiguous.
Used when no ---/+++ lines follow, as for a mode change."
  (let ((rest (substring line (length "diff --git "))))
    (if (string-prefix-p "\"" rest)
        (pcase-let* ((`(,old . ,end) (read-from-string rest))
                     (new (string-trim-left (substring rest end))))
          (cons (tether-diff--unquote (prin1-to-string old))
                (tether-diff--unquote new)))
      ;; Unquoted: "a/P b/P" splits unambiguously when both sides match.
      (let ((half (/ (1- (length rest)) 2)))
        (when (and (= (length rest) (1+ (* 2 half)))
                   (equal (substring rest 2 half) (substring rest (+ half 3))))
          (let ((path (substring rest 2 half)))
            (cons path path)))))))

(defun tether-diff--parse-hunk-header (line)
  "Return (OLD-START OLD-COUNT NEW-START NEW-COUNT) for hunk header LINE."
  (unless (string-match
           "\\`@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
           line)
    (error "Tether: malformed hunk header: %s" line))
  (list (string-to-number (match-string 1 line))
        (if (match-string 2 line) (string-to-number (match-string 2 line)) 1)
        (string-to-number (match-string 3 line))
        (if (match-string 4 line) (string-to-number (match-string 4 line)) 1)))

(defun tether-diff--finish (map hunks)
  "Store HUNKS, in diff order, in MAP and index its lines; return MAP."
  (let ((by-new (make-hash-table))
        (by-old (make-hash-table)))
    (dolist (hunk hunks)
      (cl-loop for line across (tether-hunk-lines hunk)
               do (when (tether-line-new line)
                    (puthash (tether-line-new line) line by-new))
               (when (tether-line-old line)
                 (puthash (tether-line-old line) line by-old))))
    (setf (tether-file-map-hunks map) hunks
          (tether-file-map-by-new map) by-new
          (tether-file-map-by-old map) by-old)
    map))

(defun tether-diff-parse (text)
  "Parse TEXT, the output of git diff, into a list of `tether-file-map'.
Expects the a/ and b/ prefixes and three lines of context; see
`tether--diff-options' for the git options that guarantee them."
  (let ((lines (split-string text "\n"))
        maps map hunks hunk-lines hunk old new pos)
    (cl-flet ((close-hunk ()
                (when hunk
                  (setf (tether-hunk-lines hunk) (vconcat (nreverse hunk-lines)))
                  (push hunk hunks)
                  (setq hunk nil hunk-lines nil)))
              (close-file ()
                (when map
                  (push (tether-diff--finish map (nreverse hunks)) maps)
                  (setq map nil hunks nil))))
      (dolist (line lines)
        (cond
         ((string-prefix-p "diff --git " line)
          (close-hunk)
          (close-file)
          (let ((paths (tether-diff--git-line-paths line)))
            (setq map (make-tether-file-map :status :modified
                                            :old-path (car paths)
                                            :new-path (cdr paths))
                  pos nil)))
         ((null map))
         ((and (null hunk) (null hunks))
          ;; Extended header lines.
          (cond
           ((string-prefix-p "new file mode " line)
            (setf (tether-file-map-status map) :added))
           ((string-prefix-p "deleted file mode " line)
            (setf (tether-file-map-status map) :deleted))
           ((string-prefix-p "rename from " line)
            (setf (tether-file-map-status map) :renamed
                  (tether-file-map-old-path map)
                  (tether-diff--unquote (substring line 12))))
           ((string-prefix-p "rename to " line)
            (setf (tether-file-map-new-path map)
                  (tether-diff--unquote (substring line 10))))
           ((string-prefix-p "Binary files " line)
            (setf (tether-file-map-status map) :binary))
           ((string-prefix-p "--- " line)
            (setf (tether-file-map-old-path map)
                  (tether-diff--unquote (substring line 4))))
           ((string-prefix-p "+++ " line)
            (setf (tether-file-map-new-path map)
                  (tether-diff--unquote (substring line 4))))
           ((string-prefix-p "@@ " line)
            (pcase-let ((`(,os ,oc ,ns ,nc) (tether-diff--parse-hunk-header line)))
              (setq hunk (make-tether-hunk :old-start os :old-count oc
                                           :new-start ns :new-count nc)
                    old os new ns pos 0)))))
         ((string-prefix-p "@@ " line)
          (close-hunk)
          (pcase-let ((`(,os ,oc ,ns ,nc) (tether-diff--parse-hunk-header line)))
            (setq hunk (make-tether-hunk :old-start os :old-count oc
                                         :new-start ns :new-count nc)
                  old os new ns)
            ;; GitHub counts later hunk headers as positions too.
            (cl-incf pos)))
         ((null hunk))
         ((string-prefix-p "\\" line)
          ;; "\ No newline at end of file": not a line of the file.
          (cl-incf pos))
         ((memq (string-to-char line) '(?\s ?+ ?-))
          (cl-incf pos)
          (let ((kind (pcase (string-to-char line)
                        (?\s :context) (?+ :added) (?- :removed))))
            (push (make-tether-line
                   :kind kind
                   :code (cons old new)
                   :old (and (not (eq kind :added)) (prog1 old (cl-incf old)))
                   :new (and (not (eq kind :removed)) (prog1 new (cl-incf new)))
                   :pos pos
                   :hunk (length hunks))
                  hunk-lines)))))
      (close-hunk)
      (close-file))
    ;; A pure rename has neither --- nor +++ lines; an added or deleted
    ;; file has /dev/null on one side.
    (dolist (map maps)
      (pcase (tether-file-map-status map)
        (:added (setf (tether-file-map-old-path map) nil))
        (:deleted (setf (tether-file-map-new-path map) nil))))
    (nreverse maps)))

(defun tether-diff-parse-name-status (text)
  "Parse TEXT from git diff --name-status -z -M.
Return an alist (NEW-PATH OLD-PATH . STATUS), STATUS being one of
:added, :deleted, :modified, :renamed or :copied.  NEW-PATH is the
old path for deleted files."
  (let ((fields (split-string text "\0" t))
        result)
    (while fields
      (let ((code (pop fields)))
        (pcase (aref code 0)
          ((or ?R ?C)
           (let ((old (pop fields)) (new (pop fields)))
             (push (cons new (cons old (if (eq (aref code 0) ?R)
                                           :renamed :copied)))
                   result)))
          (letter
           (let ((path (pop fields)))
             (push (cons path (cons path (pcase letter
                                           (?A :added) (?D :deleted)
                                           (_ :modified))))
                   result))))))
    (nreverse result)))

;;;; Lookup

(defun tether-file-map-unchanged-line (map line)
  "Return a `tether-line' for head LINE of MAP outside its hunks, or nil.
The line did not change: its base number follows from the hunks before
it.  Nil when LINE is in a hunk."
  (let ((old line))
    (catch 'in-hunk
      (dolist (hunk (tether-file-map-hunks map))
        (let ((new-start (tether-hunk-new-start hunk))
              (new-count (tether-hunk-new-count hunk)))
          (when (and (> new-count 0) (<= new-start line)
                     (< line (+ new-start new-count)))
            (throw 'in-hunk nil))
          ;; A hunk with no new line sits after its start line.
          (let ((new-end (if (= new-count 0) (1+ new-start) (+ new-start new-count)))
                (old-end (if (= (tether-hunk-old-count hunk) 0)
                             (1+ (tether-hunk-old-start hunk))
                           (+ (tether-hunk-old-start hunk)
                              (tether-hunk-old-count hunk)))))
            (when (>= line new-end)
              (setq old (+ old-end (- line new-end)))))))
      (make-tether-line :kind :context :old old :new line
                        :code (cons old line)))))

(defun tether-file-map-line (map line &optional anywhere)
  "Return the `tether-line' of head LINE in MAP, or nil.
With ANYWHERE, a line outside the hunks of MAP has one too."
  (and map
       (or (gethash line (tether-file-map-by-new map))
           (and anywhere
                (not (memq (tether-file-map-status map) '(:binary :lfs :deleted)))
                (tether-file-map-unchanged-line map line)))))

(defun tether-file-map-region-problem (map beg end &optional anywhere)
  "Return why head lines BEG to END of MAP cannot take one comment, or nil.
A comment covers lines of one hunk; with ANYWHERE, any lines of the
file, as GitLab allows."
  (let ((first (tether-file-map-line map beg anywhere))
        (last (tether-file-map-line map end anywhere)))
    (cond
     ((null map) "the file is not changed in the pull request")
     ((eq (tether-file-map-status map) :binary) "the file is binary")
     ((eq (tether-file-map-status map) :lfs) "the file is stored in Git LFS")
     ((null first) (format "line %d is not in the pull request diff" beg))
     ((null last) (format "line %d is not in the pull request diff" end))
     ((and (not anywhere)
           (/= (tether-line-hunk first) (tether-line-hunk last)))
      (format "lines %d-%d span two hunks of the pull request diff" beg end)))))

(defun tether-file-map-region (map beg end &optional anywhere)
  "Return (START . END) `tether-line's for head lines BEG to END of MAP.
ANYWHERE is as for `tether-file-map-region-problem'.  Signal a
`user-error' when the lines cannot take one comment."
  (when-let* ((problem (tether-file-map-region-problem map beg end anywhere)))
    (user-error "Tether: %s" problem))
  (cons (tether-file-map-line map beg anywhere)
        (tether-file-map-line map end anywhere)))

(defun tether-file-map-commentable (map)
  "Return the head lines of MAP that can take a comment, as (LINE . KIND)."
  (when (and map (not (memq (tether-file-map-status map) '(:binary :lfs))))
    (sort (let (result)
            (maphash (lambda (line l) (push (cons line (tether-line-kind l)) result))
                     (tether-file-map-by-new map))
            result)
          (lambda (a b) (< (car a) (car b))))))

;;;; Local edits

;; A drift is the list of hunks of a -U0 diff from a FROM file to a TO
;; file, as (FROM-START FROM-COUNT TO-START TO-COUNT).  It maps lines
;; that did not change between the two files.

(defun tether-diff-parse-drift (text)
  "Parse the hunk headers of TEXT, a git diff -U0, into a drift."
  (let (drift)
    (dolist (line (split-string text "\n"))
      (when (string-prefix-p "@@ " line)
        (push (tether-diff--parse-hunk-header line) drift)))
    (nreverse drift)))

(defun tether-diff--drift-map (drift line from-index)
  "Map LINE through DRIFT, from the side FROM-INDEX names.
FROM-INDEX is 2 to map a TO line to FROM, 0 to map FROM to TO.
Return nil when the line changed."
  (let ((to-index (- 2 from-index))
        (delta 0)
        (result 'unset))
    (while (and drift (eq result 'unset))
      (let* ((hunk (pop drift))
             (start (nth from-index hunk))
             (count (nth (1+ from-index) hunk))
             (first (if (zerop count) (1+ start) start)))
        (cond ((< line first)
               (setq result (- line delta)))
              ((< line (+ start count))
               (setq result nil))
              (t
               (cl-incf delta (- count (nth (1+ to-index) hunk)))))))
    (if (eq result 'unset) (- line delta) result)))

(defun tether-diff-drift-from (drift line)
  "Return the FROM line of DRIFT's TO LINE, or nil if it changed."
  (tether-diff--drift-map drift line 2))

(defun tether-diff-drift-to (drift line)
  "Return the TO line of DRIFT's FROM LINE, or nil if it changed."
  (tether-diff--drift-map drift line 0))

(provide 'tether-diff)
;;; tether-diff.el ends here
