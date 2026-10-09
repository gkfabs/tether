;; -*- mode: eask; lexical-binding: t -*-

;; Dependency sets (SPEC.md §2.2), chosen with the TETHER_DEPS
;; environment variable:
;;
;; - latest (default): everything from MELPA snapshots.
;; - min: forge and ghub built from the commits of their minimum
;;   releases (the packages that broke code-review); everything else
;;   from the GNU and NonGNU ELPA releases.  package.el cannot install
;;   an older release of magit, so magit is the latest release there.

(package "tether" "0.1.0-snapshot"
         "Review PRs and MRs from the file you are reading")
(website-url "https://github.com/gkfabs/tether")
(keywords "tools" "vc")

(package-file "tether.el")
(files "tether*.el")

(script "test" "eask test ert test/*.el")

(defconst tether-eask-deps (or (getenv "TETHER_DEPS") "latest"))

(source "gnu")
(source "nongnu")

(depends-on "emacs" "29.1")
(depends-on "compat")
(depends-on "magit")
(depends-on "transient")
(depends-on "markdown-mode")

(pcase tether-eask-deps
  ("latest"
   (source "melpa")
   (source-priority "melpa" 10)
   (depends-on "forge")
   (depends-on "ghub"))
  ("min"
   ;; Releases first; MELPA only for what GNU/NonGNU lack (closql,
   ;; emacsql).
   (source "melpa")
   (source-priority "gnu" 10)
   (source-priority "nongnu" 10)
   (source-priority "melpa" 0)
   ;; `package-vc-install' at a release tag.
   (depends-on "ghub" :vc '(ghub :url "https://github.com/magit/ghub"
                                 :lisp-dir "lisp")
               "v5.3.0")              ; 2026-08-01
   (depends-on "forge" :vc '(forge :url "https://github.com/magit/forge"
                                   :lisp-dir "lisp")
               "v0.6.0"))             ; 2025-09-09
  (_ (error "TETHER_DEPS must be \"latest\" or \"min\", not %S"
            tether-eask-deps)))

(development
 (depends-on "package-lint"))
