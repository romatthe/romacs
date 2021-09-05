;;; -*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018-2021 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Code for bootstrapping my Emacs configuration

;;; Code:

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Bootstrap straight.el for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Effectively replace use-package with straight-use-package
;; https://github.com/raxod502/straight.el/blob/master/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; Always defer loading a package unless :demand is specified
(setq use-package-always-defer t)

;; An easy to use package to clean up mode lighters
(straight-use-package
 '(blackout :host github :repo "raxod502/blackout"))

;; Now that we have `use-package`, we can use the Garbage Collection Magic Trick
(use-package gcmh
  :demand t
  :blackout
  :init
  (gcmh-mode 1))
