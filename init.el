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

(provide 'init)
;;; init.el ends here
