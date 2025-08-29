;;; ui.el --- Summary
;;; Commentary:
;; UI configuration

;;; Code:

;; theme
;; (add-to-list 'load-path (expand-file-name "lisp/themes" user-emacs-directory))
;; (require 'rose)
;; (require 'nano-dark)

(add-to-list 'custom-theme-load-path (expand-file-name "lisp/themes" user-emacs-directory))
(load-theme 'poimandres t)

;; font
(set-frame-font "JetBrainsMono Nerd Font-14" t t)
(setq-default line-spacing 8)

;; disable elements
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; set scroll off
(setq scroll-margin 8)

;; set custom modeline
;; (require 'custom-modeline)

;; WARNING: EXPERIMENT
(require 'nano-module)

;; which key
(which-key-mode 1)
(setq which-key-idle-delay 0.1)

(provide 'ui)
;;; ui.el ends here
