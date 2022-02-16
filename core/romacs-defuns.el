;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Custom helper functions used throughout this configuration

;;; Code:

(defun romacs/emacs.d (path)
  "Return path inside user's `.emacs.d'."
  (expand-file-name path user-emacs-directory))

(defun romacs/cache-for (identifier)
  "Return cache directory for given identifier."
  (expand-file-name identifier (romacs/emacs.d "var/cache")))

(defun romacs/mkdir-p (dir-path)
  "Make directory if it doesn't exist."
  (unless (file-exists-p dir-path)
    (make-directory dir-path t)))


(provide 'romacs-defuns)
