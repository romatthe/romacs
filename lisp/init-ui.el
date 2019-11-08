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
;; Configuration of Emacs' UI elements
;;

;;; Code:

;; Default font
(set-frame-font "Fira Code-11" nil t)

;; Fix for Fira Code ligatures
(defun fira-code-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
    (mapcar
     (lambda (s)
       (setq idx (1+ idx))
       (let* ((code (+ #Xe100 idx))
          (width (string-width s))
          (prefix ())
          (suffix '(?\s (Br . Br)))
          (n 1))
     (while (< n width)
       (setq prefix (append prefix '(?\s (Br . Bl))))
       (setq n (1+ n)))
     (cons s (append prefix suffix (list (decode-char 'ucs code))))))
     list)))

(defconst fira-code-mode--ligatures
  '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
    "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
    "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
    "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
    ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
    "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
    "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
    "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
    ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
    "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
    "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
    "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
    "x" ":" "+" "+" "*"))

(defvar fira-code-mode--old-prettify-alist)

(defun fira-code-mode--enable ()
  "Enable Fira Code ligatures in current buffer."
  (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
  (prettify-symbols-mode t))

(defun fira-code-mode--disable ()
  "Disable Fira Code ligatures in current buffer."
  (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :lighter " Fira-Code"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if fira-code-mode
      (fira-code-mode--enable)
    (fira-code-mode--disable)))

(defun fira-code-mode--setup ()
  "Setup Fira Code Symbols"
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

;; Enable Fira Code ligatures in all programming modes
(add-hook 'prog-mode-hook 'fira-code-mode)

;; Menu / Tool / Scroll bars
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; The Doom mode-line
(use-package doom-modeline
  :demand t
  :hook (after-init . doom-modeline-mode)
  :init
  ;; prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq-default mode-line-format nil))
  :custom
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project))

(use-package hide-mode-line
  :hook (((completion-list-mode completion-in-region-mode) . hide-mode-line-mode)))

;; Install the collection of themes from Doom-Emacs
(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-gruvbox t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Line numbers and columns
(use-package display-line-numbers
  :straight nil
  :blackout t
  :hook (after-init . global-display-line-numbers-mode))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Popup for showing key completions
(use-package which-key
  :demand t
  :config
  (which-key-mode))

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'window-setup-hook #'size-indication-mode)
(setq ring-bell-function 'ignore) ; Don't play a sound or flash the screen on error
(setq inhibit-compacting-font-caches t) ; Don’t compact font caches during GC.


(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
