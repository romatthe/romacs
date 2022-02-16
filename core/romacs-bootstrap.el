;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Bootstrap package management via straight.el

;;; Code:

;; straight.el tuning for faster startup.
(setq straight-cache-autoloads t)
(setq straight-check-for-modifications '(check-on-save))

;; Bootstrap straight.
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

;; Use `use-package' via straight.el.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Defer by default. Most packages should be configured with `:mode' or similar
;; but this is a bit safer and sped up init-time.
;; For cases where `:mode' etc. don't make sense, `:defer nil' explicitly.
(setq use-package-always-defer t)

;; Install blackout as required by `:blackout` with use-package.
(use-package blackout)

(provide 'romacs-bootstrap)
