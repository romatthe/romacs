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
;; Editing configuration
;;

;;; Code:

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; How to handle two buffers with the same name
(use-package uniquify
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffers-p t "Rename after killing uniquified")
  (uniquify-ignore-buffers-re "^\\*"))

;; Emacs pseudo packages allows us to config variables defined in C code
;; through `use-package`.
(use-package emacs
  :straight nil
  :custom
  (delete-by-moving-to-trash t "Deleting files go to OS's trash folder")
  (tab-width 4 "Default tab width")
  (indent-tabs-mode nil "Do not indent with TABs"))

;; Try to keep Emacs from littering stuff all over the place
(use-package files
  :straight nil
  :custom
  (make-backup-files nil "Stop making backup files")
  (auto-save-default nil "Disable auto save"))

;; Delete selection if you insert
(use-package delsel
  :straight t
  :demand t
  :config (delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :straight nil
  :demand t
  :blackout t
  :config
  (global-auto-revert-mode))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-x z" . avy-goto-char-2)
         ("C-x C-z" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-background t)
  (avy-style 'pre))

;; Kill text between the point and the character CHAR
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; Quickly follow links
(use-package ace-link
  :defines (org-mode-map
            gnus-summary-mode-map
            gnus-article-mode-map
            ert-results-mode-map)
  :bind ("M-o" . ace-link-addr)
  :hook (after-init . ace-link-setup-default)
  :config
  (with-eval-after-load 'org
    (bind-key "M-o" #'ace-link-org org-mode-map))
  (with-eval-after-load 'gnus
    (bind-keys
     :map gnus-summary-mode-map
     ("M-o" . ace-link-gnus)
     :map gnus-article-mode-map
     ("M-o" . ace-link-gnus)))
  (with-eval-after-load 'ert
    (bind-key "o" #'ace-link-help ert-results-mode-map)))

;; An all-in-one comment command
(use-package comment-dwim-2
  :bind ("C-/" . comment-dwim-2))

;; Move pieces of text around
(use-package drag-stuff
  :blackout t
  :bind (("M-S-<up>" .    drag-stuff-up)
         ("M-S-<down>" .  drag-stuff-down)
         ("M-S-<left>" .  drag-stuff-left)
         ("M-S-<right>" . drag-stuff-right))
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode))

;; Automatic parenthesis pairing
(use-package elec-pair
  :straight nil
  :blackout t
  :hook (after-init . electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Multiple editing cursors
(use-package multiple-cursors
  :bind (("M-j" . mc/mark-next-like-this)
         ("M-J" . mc/unmark-next-like-this)))

;; Increase selected region by semantic units
(use-package expand-region
  :bind (("C-w" . er/expand-region)
         ("C-S-w" . er/contract-region)))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (("<home>" . mwim-beginning-of-code-or-line)
         ("<end>" . mwim-end-of-code-or-line)))

;; Treat undo history as a tree
(use-package undo-tree
  :blackout
  :hook (after-init . global-undo-tree-mode)
  :bind (("C-z" . undo-tree-undo)
         ("C-x u" . undo-tree-undo-visualize))
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist `(("." . ,(locate-user-emacs-file "undo-tree-hist/")))))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :blackout
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Produces menus for accessing locations in documents
(use-package imenu-list
  :bind ("C-." . imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))


(provide 'init-editing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
