;;; early-init.el --- Early Emacs Configuration -*- lexical-binding: t; -*-
;;; Commentary:

;; This file sets up early configuration options (before the UI renders and
;; before package management starts.) As such, this file is meant to be small
;; and only make configuration tweaks that either improve load time by reducing
;; UI elements or affect the package management.

;;; Code:

;; Disable unneeded GUI elements
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-to-list 'default-frame-alist '(internal-border-width . 16))

;; Disable package setup (use straight.el later)
(setq package-enable-at-startup 'nil)

(provide 'early-init)
;;; early-init.el ends here
