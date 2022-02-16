;;; early-init.el -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

<<<<<<< HEAD
=======
;;; Code:

>>>>>>> 012e832 (New configuration structure)
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; GCC Emacs deferred compilation.
(setq comp-speed 2
      comp-deferred-compilation nil)

;; Using straight.el means we don't want to initialize package.el at all.
(setq package-enable-at-startup nil)

<<<<<<< HEAD
;; By disabling these UI elements in init-early.el we prevent the glimpse of un-styled Emacs.
=======
;; By disabling UI elements during early-init we prevent the glimpse of un-styled Emacs.
>>>>>>> 012e832 (New configuration structure)
(menu-bar-mode 0)
(tool-bar-mode 0)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
