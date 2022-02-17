;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Configuration for the overall appearance of Emacs 

;;; Code:

;; Disable cursor display in inactive windows.
(setq-default cursor-in-non-selected-windows nil)

;; Variables for various fonts used 
(defvar romacs/fixed-font-name "Office Code Pro")
(defvar romacs/fixed-font-weight 'normal)
(defvar romacs/var-font-name "iA Writer Duospace")

;; Quick workaround to help switching between laptop screens and workstation monitor.
(defvar romacs/font-height
  (if (< (display-pixel-height) 1600)
      110 80))

;; Native line numbers and fringe setup.
(setq-default display-line-numbers-width 4)

(add-hook 'after-init-hook
          (lambda nil
            (set-fringe-style 0)
            (setq-default cursor-type 'bar)
	    ;; Maximize the window
            (toggle-frame-maximized)
            (set-face-attribute
             'default nil
             :family romacs/fixed-font-name
             :height romacs/font-height
             :weight romacs/fixed-font-weight)
            (set-face-attribute
             'line-number nil
             :family romacs/fixed-font-name
             :height romacs/font-height
             :weight romacs/fixed-font-weight)
            (set-face-attribute
             'variable-pitch nil
             :family romacs/var-font-name)))

(use-package darkokai-theme
  :defer nil
  :config
  (setq-default darkokai-blue-tint nil)
  (load-theme 'darkokai t))

;; (use-package doom-themes
;;   :defer nil
;;   :config
;;   (load-theme 'doom-gruvbox t)
;;   (custom-set-faces
;;    '(mode-line ((t (:box (:line-width 8)))))))


(provide 'romacs-appearance)
