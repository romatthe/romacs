;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Sane defaults for all modes

;;; Code:

(require 'romacs-defuns)

;; Create and set ancillary directories 
(romacs/mkdir-p (romacs/emacs.d "var/cache"))
(romacs/mkdir-p (romacs/emacs.d "etc"))
(romacs/mkdir-p (romacs/cache-for "backups"))

;; Ignore customisation by putting it in the cache dir, so they never accidentally clutter init.el.
(setq custom-file (romacs/cache-for "custom.el"))

;; Set default directory.
(setq default-directory "~")

;; Set home dir.
(cd (expand-file-name "~/"))

;; Frame title formatting.)
(setq-default frame-title-format '("😈 Emacs  // " "%b" " 😈"))

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Show column numbers in mode line.
(setq column-number-mode t)

;; Please never ring the bell.
(setq ring-bell-function (lambda()))

;; Don't use dialog boxes.
(setq use-dialog-box nil)

;; Hide mouse while typing.
(setq make-pointer-invisible t)

;; Reduce keystroke echo delay.
(setq echo-keystrokes 0.001)

;; Enable y/n answersm, because this is much faster.
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatically scroll compilation window.
(setq compilation-scroll-output 1)

;; Emoji font.
(set-fontset-font t 'symbol
                  (font-spec :family "Twitter Color Emoji")
                  nil 'prepend)

;; Keep autosave files in /tmp.
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Change auto-save-list directory.
(setq auto-save-list-file-prefix (romacs/cache-for "auto-save-list/.saves-"))

;; Change eshell directory.
(setq eshell-directory-name (romacs/cache-for "eshell"))

;; Disable annoying lock files.
(setq create-lockfiles nil)

;; Change bookmarks file location.
(setq bookmark-default-file (romacs/emacs.d "etc/bookmarks"))

;; Change save-places file location.
(setq save-place-file (romacs/cache-for "places"))

;; Allow pasting selection outside of Emacs.
(setq x-select-enable-clipboard t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Ignore case for completion.
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; When copying something from outside emacs, save to kill-ring.
(setq save-interprogram-paste-before-kill t)

;; Dired defaults.
(setq-default dired-listing-switches "-lhva")
(setq-default dired-clean-up-buffers-too t)
(setq-default dired-recursive-copies 'always)
(setq-default dired-recursive-deletes 'top)

(setq inhibit-startup-message t)

;; Enable highlighting in all modes by default
(global-font-lock-mode t)


(provide 'romacs-defaults)
