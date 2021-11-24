;;; -*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018-2021 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Configuration for working with the Nix language

;;; Code:
(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nix-mode
  :mode (("\\.nix\\'" . nix-mode)
         ("\\.drv\\'" . nix-drv-mode))
  :hook (nix-mode . flycheck-mode))


(provide 'init-lang-nix)
