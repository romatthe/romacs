;;; -*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018-2021 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Configuration for working with the Rust language

;;; Code:

(use-package toml-mode
  :mode ".toml$")

(use-package rust-mode
  :mode ".rs$"
  :config
  (setq rust-format-on-save t))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))


(provide 'init-lang-rust)
