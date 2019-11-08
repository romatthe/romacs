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
;; Configuration for Ivy / Swiper / Counsel
;;

;;; Code:

(use-package ivy
  :blackout t
  :bind (("C-s" . swiper-isearch)
         ("C-S-s" . swiper-all))
  :custom
  (ivy-count-format "%d/%d " "Show anzu-like counter")
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  :config
  (ivy-mode t))

(use-package counsel
  :blackout t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map help-map
         ("f" . counsel-describe-function)
         ("v" . counsel-describe-variable)
         ("l" . counsel-find-library)
         :map mode-specific-map
         :prefix-map counsel-prefix-map
         :prefix "c"
         ("a" . counsel-apropos)
         ("b" . counsel-bookmark)
         ("B" . counsel-bookmarked-directory)
         ("c w" . counsel-colors-web)
         ("c e" . counsel-colors-emacs)
         ("d" . counsel-dired-jump)
         ("f" . counsel-file-jump)
         ("F" . counsel-faces)
         ("g" . counsel-org-goto)
         ("h" . counsel-command-history)
         ("H" . counsel-minibuffer-history)
         ("i" . counsel-imenu)
         ("j" . counsel-find-symbol)
         ("l" . counsel-locate)
         ("L" . counsel-find-library)
         ("m" . counsel-mark-ring)
         ("o" . counsel-outline)
         ("O" . counsel-find-file-extern)
         ("p" . counsel-package)
         ("r" . counsel-recentf)
         ("s g" . counsel-grep)
         ("s r" . counsel-rg)
         ("s s" . counsel-ag)
         ("t" . counsel-org-tag)
         ("v" . counsel-set-variable)
         ("w" . counsel-wmctrl)
         ("g" . counsel-git)
         ("j" . counsel-git-grep))
  :custom
  (ivy-count-format "%d/%d " "Show anzu-like counter")
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  :init
  (counsel-mode))

;; Additional key bindings for Ivy
(use-package ivy-hydra
  :after (ivy hydra)
  :bind (:map ivy-minibuffer-map
              ("M-o" . ivy-dispatching-done-hydra)))

;; Ivy integration for Projectile
(use-package counsel-projectile
  :after (counsel projectile)
  :custom
  (counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  :init
  (counsel-projectile-mode 1))

;; Integrate yasnippet
(use-package ivy-yasnippet
  :after ivy
  :commands ivy-yasnippet--preview
  :bind ("C-c C-y" . ivy-yasnippet)
  :config
  (advice-add #'ivy-yasnippet--preview :override #'ignore))

;; More friendly display transformer for Ivy
(use-package ivy-rich
  :demand t
  :config
  (ivy-rich-mode 1))


(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
