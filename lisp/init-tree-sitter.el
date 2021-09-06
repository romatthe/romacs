;;; -*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018-2021 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Configure tree-sitter to take care of syntax highlighting

;;; Code:

;; Tree-sitter provides advanced syntax-highlighting features
(use-package tree-sitter
  :hook ((go-mode   . tree-sitter-hl-mode)
         (rust-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :demand t)


(provide 'init-tree-sitter)
