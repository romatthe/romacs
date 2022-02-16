;;; -*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018-2022 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Robin Mattheussen's Emacs configuration

;;; Code:

;; Reset GC threshold after init to something reasonable.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 16777216)))

<<<<<<< HEAD
;; Prepare paths.
=======

;; Prepare paths containing the configuration
>>>>>>> 012e832 (New configuration structure)
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
