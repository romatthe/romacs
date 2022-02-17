;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Configuring basic settings from programming across several languages

;;; Code:

;; Project-local coding style definitions.
(use-package editorconfig
  :blackout
  :delight editorconfig-mode
  :hook (prog-mode . editorconfig-mode))

;; Basic mode for editing yaml files
(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :bind (:map yaml-mode-map ("C-m" . newline-and-indent)))

;; Basic mode for editing protobuf IDL files
(use-package protobuf-mode
  :mode (("\\.proto$" . protobuf-mode)
         ("\\.proto3$" . protobuf-mode)))

;; Highlights magic numbers in programming modes.
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; flycheck
(use-package flycheck
  :blackout
  :hook (prog-mode . flycheck-mode)
  :custom (flycheck-emacs-lisp-load-path 'inherit))

;; Runs REST queries from a query sheet and pretty-prints responses.
(use-package restclient
  :mode ("\\.http$" . restclient-mode))

;; lsp
(use-package lsp-mode
  :custom (lsp-enable-snippet t))

;; Additional UI elements for `lsp-mode`
(use-package lsp-ui
  :after lsp-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-sideline-ignore-duplicate t))


(provide 'romacs-dev)
