;;; -*- lexical-binding: t no-byte-compile: t; -*-
;; Copyright (C) 2018-2021 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Robin Mattheussen's Emacs configuration

;;; Code:

;; Bootstrap the configuration
(load-file (expand-file-name "init-bootstrap.el" user-emacs-directory))

;; Core modules
(require 'init-defaults)
;(require 'init-editing)
;(require 'init-tree-sitter)
;(require 'init-visual)

;; Languages
;(require 'init-lang-nix)
;(require 'init-lang-rust)
