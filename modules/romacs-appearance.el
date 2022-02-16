;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Robin Mattheussen

;; Author: Robin Mattheussen <me@romatthe.dev>
;; URL: https://github.com/romatthe/romacs

;;; Commentary:
;; Configuration for the overall appearance of Emacs 

;;; Code:

;; Disable cursor display in inactive windows.
(setq-default cursor-in-non-selected-windows nil)

;; Variables for various fonts used 
(defvar romacs/fixed-font-name "Office Code Pro")
(defvar romacs/fixed-font-weight 'normal)
(defvar romacs/var-font-name "iA Writer Duospace")

;; Quick workaround to help switching between laptop screens and workstation monitor.
(defvar romacs/font-height
  (if (< (display-pixel-height) 1600)
      150 130))

;; Native line numbers and fringe setup.
(setq-default display-line-numbers-width 4)
