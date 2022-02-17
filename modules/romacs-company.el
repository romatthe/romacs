;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Configuration for the company auto-completion framework.

;;; Code:

(use-package company
  :blackout
  :commands (company-mode global-company-mode)
  :hook ((prog-mode comint-mode org-mode) . company-mode)
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :custom
  (company-tooltip-limit 20)
  (company-idle-delay 0.25)
  (company-echo-delay 0)
  (company-minimum-prefix-length 2))


;; Quick-help (popup documentation for suggestions).
(use-package company-quickhelp
  :if window-system
  :after company
  :config (company-quickhelp-mode t))


(provide 'romacs-company)
