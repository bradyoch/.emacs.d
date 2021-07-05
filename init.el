;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:

;; This file sets up all my configurations for Emacs. The bulk of the
;; code to configure individual packages or features are located in
;; the lisp/ directory of my .emacs.d.

;; Thanks to Steve Purcell for the inspiration for this configuration
;; format (https://github.com/purcell/emacs.d)

;;; Code:

(setq user-full-name "Brady Ochse"
      user-mail-address "bradyoch@gmail.com")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq make-backup-files nil
      create-lockfiles nil
      auto-save-default nil)

(setq initial-scratch-message ""
      inhibit-startup-screen t)

(setq-default indent-tabs-mode nil
              tab-width 2)

(setq require-final-newline t)

(fringe-mode '(8 . 0))
(set-face-attribute 'fringe nil :background nil)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(setq tab-always-indent 'complete)
(setq completion-styles '(basic partial-completion initials))
;; Change "yes or no" prompts to "y or n"
(fset #'yes-or-no-p 'y-or-n-p)

(require 'init-straight)
(require 'init-use-package)

(provide 'init)
;;; init.el ends here
