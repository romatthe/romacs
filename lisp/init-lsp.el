;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Robin Mattheussen

;; Author: Robin Mattheussen <robin.mattheussen@gmail.com>
;; URL: https://github.com/romatthe/dotfiles

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Configuration of lsp-mode and various accompanying tools
;;

;;; Code:

(use-package lsp-mode
  :blackout t
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((c-mode c++-mode java-mode rust-mode) . lsp)
  :custom
  (lsp-auto-guess-root t)       ; Detect project root
  (lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (flymake-fringe-indicator-position 'right-fringe)
  (lsp-file-watch-threshold 2000))

(use-package lsp-treemacs
  :bind (:map lsp-mode-map
              ("M-9" . lsp-treemacs-errors-list)))

(use-package lsp-ui
  :after lsp-mode
  :blackout t
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-doc-use-webkit t))

(use-package company-lsp
  :custom
  (company-lsp-cache-candidates 'auto)
  (company-lsp-async t))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-mode
  :after lsp-mode
  :blackout t
  :bind (:map dap-mode-map
              (("<f12>" . dap-debug)
               ("<f8>" . dap-continue)
               ("<f9>" . dap-next)
               ("<M-f11>" . dap-step-in)
               ("C-M-<f11>" . dap-step-out)
               ("<f7>" . dap-breakpoint-toggle)))
  :config
  (dap-mode t)
  (dap-ui-mode t)
  ;; Call the dap-mode hydra every time a break point is hit
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

;;; Clients:

(use-package ccls
  :after projectile
  :demand t
  :ensure-system-package ccls
  :custom
  (ccls-sem-highlight-method 'font-lock)
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(use-package lsp-java
  :demand t
  :after lsp)

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
