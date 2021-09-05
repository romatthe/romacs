;;; -*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018-2021 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Configuration for normal text editing operations

;;; Code:

;; Undo-fu is a package for linear undo operations that is more stable than undo-tree
(use-package undo-fu
  :bind (("C-z"   . 'undo-fu-only-undo)   ;; Linear undo
         ("C-S-z" . 'undo-fu-only-redo))) ;; Linear redo


(provide 'init-editing)
