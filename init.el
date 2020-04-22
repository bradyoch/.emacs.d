;;; init.el --- Brady's emacs config -*- lexical-binding: t -*-
;;; Commentary:

;; This file contains most of the configuration options for Emacs,
;; loading other modules as needed for organization.

;; This file is largely inspired by Steve Purcell's config located
;; here: https://github.com/purcell/emacs.d/blob/master/init.el

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Utility Functions
;;----------------------------------------------------------------------------

(defun brady/update-load-config ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

;;----------------------------------------------------------------------------
;; UI Tweaks
;;----------------------------------------------------------------------------

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(when (fboundp 'fringe-mode) (fringe-mode '(0 . 0)))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-hl-line-mode 1)

(setq ring-bell-function 'ignore)

(load-theme 'misterioso 't)

;;----------------------------------------------------------------------------
;; Default Behavior Tweaks
;;----------------------------------------------------------------------------

(setq backup-inhibited 't
      auto-save-default 'nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;;----------------------------------------------------------------------------
;; Python Configs
;;----------------------------------------------------------------------------

(use-package pyvenv
  :ensure t)

;;----------------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------------

(global-unset-key (kbd "C-z"))

(global-set-key (kbd "C-o") 'other-window)

(provide 'init)
;;; init.el ends here
