;;; -*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018-2019 Robin Mattheussen

;; Author: Robin Mattheussen <robin.mattheussen@gmail.com>
;; URL: https://github.com/romatthe/dotfiles

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Robin Mattheussen's Emacs configuration
;;

;;; Code:

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;;; Requirements:
;; The following system utilities should be installed: git, gcc, cmake, ccls

;; Essential bootstrapping procedures
(require 'init-bootstrap)

;; Cheats!
(require 'init-cheatsheet)

;; Core
(require 'init-basic)
(require 'init-editing)
(require 'init-ivy)
(require 'init-dired)
(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-org)
(require 'init-treemacs)
(require 'init-window)

;; Programming Tools
(require 'init-company)
(require 'init-git)
(require 'init-flycheck)
(require 'init-projectile)
(require 'init-lsp)

;; UI
(require 'init-dashboard)
(require 'init-ui)

;; Languages
(require 'init-lang-c-cpp)
(require 'init-lang-common-lisp)
(require 'init-lang-clojure)
(require 'init-lang-go)
(require 'init-lang-haskell)
(require 'init-lang-latex)
(require 'init-lang-markdown)
(require 'init-lang-nix)
(require 'init-lang-rust)
