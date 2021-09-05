;;; -*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018-2021 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Sensible defaults to use across Emacs

;;; Code:

;; Sensible defaults
(setq inhibit-startup-screen    t        ;; Don't show the GNU Emacs startup screen.
      initial-scratch-message   nil      ;; Don't show the default message in the scratch buffers.
      sentence-end-double-space nil      ;; No double-spaces after periods.
      ring-bell-function        'ignore  ;; Stop making dinging noises.
      use-dialog-box            nil      ;; Prompts should go in the minibuffer, not in a GUI.
      mark-even-if-inactive     nil      ;; Fix undo in commands affecting the mark.
      kill-whole-line           t)       ;; C-k will delete the whole line.

;; Never use stabs for indentation
(setq-default indent-tabs-mode nil)

;; Always use UTF-8
(set-charset-priority               'unicode)
(set-terminal-coding-system         'utf-8)
(set-keyboard-coding-system         'utf-8)
(set-selection-coding-system        'utf-8)
(prefer-coding-system               'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq locale-coding-system          'utf-8)

;; Modes that are useful across the entire system
(delete-selection-mode t)            ;; Overwrite the current selection when typing
(global-display-line-numbers-mode t) ;; Show line numbers in every buffer
(column-number-mode t)               ;; Display the column in the mode-line

;; Enable line highlighting in text and programming mode
;; (This is to avoid its noisy presence in eg terminal modes, ivy, etc.)
(use-package hl-line
  :hook ((prog-mode . hl-line-mode)
	 (text-mode . hl-line-mode)))

;; Stop littering the file system with junk
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles  nil)

;; Dump custom variables set through the configuration GUI into a random file
(setq custom-file (make-temp-file ""))
(setq custom-safe-themes t)

;; Configure the recent files list
(use-package recentf
  :config
  ;; Don't clutter the recent file list with downloaded packages
  (add-to-list 'recentf-exclude "\\elpa"))

;; Remove some annoying/useless keybindings
(unbind-key "C-x C-d")        ;; list-directory
(unbind-key "C-z")            ;; suspend-frame
(unbind-key "C-x C-z")        ;; suspend-frame
(unbind-key "<mouse-2>")      ;; pasting with mouse-wheel click
(unbind-key "<C-wheel-down>") ;; text scale adjust

;; Better whitespace management
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)


(provide 'init-defaults)
