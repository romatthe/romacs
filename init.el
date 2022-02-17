;;; -*- lexical-binding: t -*-

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

;; Prepare paths containing the configuration
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; Bootstrap straight.el
(require 'romacs-bootstrap)

;; Sane defaults across all modes
(require 'romacs-defaults)

;; Custom functions.
(require 'romacs-defuns)

;; Additional modules
(require 'romacs-appearance)
(require 'romacs-editing)
