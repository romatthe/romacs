;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Configuration for basic text editing within Emacs  

;;; Code:

;; Delete marked text on typing
(delete-selection-mode t)

;; Require newline at end of file.
(setq require-final-newline t)

;; Revert buffers automatically when underlying files are changed externally.
(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :blackout auto-revert-mode)

;; Native line numbers.
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers t)))

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Use conf-mode where appropriate.
(use-package conf-mode
  :mode (("\\.editorconfig$" . conf-mode)
         ("\\.conf" . conf-mode)
         ("\\.cfg" . conf-mode)
         ("\\.ini" . conf-mode)))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("M-j" . mc/mark-next-like-this-symbol)
         ("M-J" . mc/unmark-next-like-this))
  :custom-face
   (mc/cursor-bar-face
    ((t (:height 0.2 :background "#657b83"
                 :foreground "#657b83")))))

;; expand-region
(use-package expand-region
  :bind (("C-w" . er/expand-region)
	 ("C-S-w" . er/contract-region)))

;; smartparens
;; TODO: I've had issues with `smartparens-strict-mode` so I'm going to look for a simpler package to balance parenthesis
;; (use-package smartparens
;;   :blackout
;;   :hook
;;   ((after-init . (lambda ()
;;                   (smartparens-global-mode t)
;;                   (show-smartparens-global-mode t)))
;;    (prog-mode . smartparens-strict-mode)))

;; browse-kill-ring
(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

;; whitespace cleanup
;; Automatically cleans whitespace on save.
(use-package whitespace-cleanup-mode
  :blackout
  :hook ((text-mode prog-mode) . whitespace-cleanup-mode))

;; subword
(use-package subword
  :blackout
  :hook (prog-mode . subword-mode))

;; smart-comment
;; Better `comment-dwim' supporting uncommenting.
;; TODO: Just look for a simple solution to comment/uncomment multiple lines
(use-package smart-comment
  :bind ("M-;" . smart-comment))


(provide 'romacs-editing)

