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
;; Configuration for using Git within Emacs
;;

;;; Code:

;; Magit, a git interface
(use-package magit
  :bind (("C-c g" . magit-status)))

;; Show tasks
(use-package magit-todos
  :hook (after-init . magit-todos-mode))

;; Git related modes
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)


(provide 'init-git)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
