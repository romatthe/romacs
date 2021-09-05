;;; -*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018-2021 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Configuration for visual aspects of Emacs

;;; Code:

;; Disable various annoying GUI aspects
(when (window-system)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;; Set default font
(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"
                    :height 105
                    :weight 'normal
                    :width 'normal)

;; TODO: Temperorary fix, see https://github.com/rougier/nano-modeline/issues/3
;; (use-package s
;;   :demand t)

;; A minimalist modeline that works best with nano-theme
;; (use-package nano-modeline
;;   :straight (nano-modeline :type git
;;                            :host github
;;                            :repo "rougier/nano-modeline")
;;   :demand t
;;   :config
;;   (setq nano-modeline-position 'bottom)
;;   (nano-modeline))

;; Large collection of themes used in Doom-Emacs
(use-package doom-themes
  :demand t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Customize for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Feature-packed mode-line used in Doom-Emacs
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 30
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-buffer-encoding nil
        doom-modeline-env-version t)

  (custom-set-faces
   '(mode-line ((t (:family "JetBrainsMono Nerd Font" :height 120))))
   '(mode-line-inactive ((t (:family "JetBrainsMono Nerd Font" :height 120))))))

;; Distingiush real text buffers from UI buffers
(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

;; Large collection of icons
(use-package all-the-icons
  :demand t)

;; Enable the use of the all-the-icons in dired-mode
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; Always start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Show matching parantheses
(show-paren-mode 1)
(setq show-paren-delay 0)


(provide 'init-visual)
