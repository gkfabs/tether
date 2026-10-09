;;; tether.el --- Review PRs and MRs from the file you are reading  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabien Carrion

;; Author: Fabien Carrion <fabien@carrion.mx>
;; Maintainer: Fabien Carrion <fabien@carrion.mx>
;; URL: https://github.com/gkfabs/tether
;; Keywords: tools, vc
;; Version: 0.1.0-snapshot
;; Package-Requires: ((emacs "29.1") (compat "31.0") (magit "4.6") (forge "0.6") (ghub "5.3") (transient "0.10") (markdown-mode "2.7"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tether ties the code you are reading to its pull request (GitHub) or
;; merge request (GitLab): comment on the current line or region from
;; the file buffer, and jump between the file, the PR diff, the forge
;; topic, the review threads and the browser while keeping file and
;; line.
;;
;; It is built on Forge: the repository, the pull request and the API
;; token all come from Forge.
;;
;; See SPEC.md in the source repository for the design.

;;; Code:

(require 'tether-deps)

(defgroup tether nil
  "Review pull requests and merge requests from file buffers."
  :group 'tools
  :link '(url-link "https://github.com/gkfabs/tether"))

(defconst tether-version "0.1.0-snapshot"
  "Version of the tether package.")

;;;###autoload
(define-minor-mode tether-mode
  "Minor mode tying a file buffer to its pull request.

This is a placeholder; commands are added in later steps."
  :lighter " tether"
  :group 'tether)

(provide 'tether)
;;; tether.el ends here
