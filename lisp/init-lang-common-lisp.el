;;; -*- lexical-binding: t -*-

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
;; Configuration for working with the Common Lisp language
;;

;;; Code:

;; Defer the existing lisp-mode in Emacs
(use-package lisp-mode
  :straight nil
  :defer t)

;; This requires a functional Roswell installation as well as having SLIME installed
;; through Roswell with `ros install slime'
(use-package sly
  ;;:mode (".lisp$" ".cl$")
  :custom
  (inferior-lisp-program "ros -Q run" "Preferred Common Lisp implementation")
  :config
  (load (expand-file-name "~/.roswell/helper.el"))
  (use-package rainbow-delimiters
    :hook (sly-mode . rainbow-delimiters-mode))
  (use-package sly-company
    :hook (sly-mode . sly-company-mode)))

(use-package sly-macrostep)

(use-package sly-repl-ansi-color
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color nil #'eq))


(provide 'init-lang-common-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lang-common-lisp.el ends here
