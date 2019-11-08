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
;; Base configuration
;;

;;; Code:


;; Personal information
(setq user-full-name "Robin Mattheussen")
(setq user-mail-address "robin.mattheussen@gmail.com")

;; Environment
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
  (exec-path-from-shell-arguments '("-l"))
  :init
  (exec-path-from-shell-initialize))

;; History
(use-package saveplace
  :demand t
  :hook (after-init . save-place-mode))

;; Recent files
(use-package recentf
  :demand t
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "ido.*"
                          "persp-confs"
                          "recentf"
                          "undo-tree-hist"
                          "url"
                          "COMMIT_EDITMSG\\'")))

;; Save the history of buffers
(use-package savehist
  :demand t
  :hook (after-init . savehist-mode)
  :custom
  (enable-recursive-minibuffers t "Allow commands in minibuffers")
  (history-length 1000)
  (savehist-additional-variables '(mark-ring
				   global-mark-ring
				   search-ring
				   regexp-search-ring
				   extended-command-history))
  (savehist-autosave-interval 300))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)))

(use-package restart-emacs
  :bind ("C-x C-r" . restart-emacs))


(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
