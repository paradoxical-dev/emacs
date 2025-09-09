;;; core.el --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;; INFO: Module will house all core packages typically used as dependencies.
;; Will be loaaded before all other packages but AFTER ui module

;;; Code:

;; Fix auto saving and backup files
(defvar my-auto-dir   (expand-file-name "auto-saves/" user-emacs-directory))
(defvar my-backup-dir (expand-file-name "backups/" user-emacs-directory))

(setq backup-directory-alist `(("." . ,my-backup-dir)))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 2
      version-control t)

(setq auto-save-file-name-transforms `((".*" ,my-auto-dir t)))

;; simple-httpd ;;

;; INFO: emacs simple web server
(use-package simple-httpd
  :ensure t
  :defer 0)

;; s ;;

;; INFO: string manipulation
(use-package s
  :ensure t
  :defer 0)

;; dash ;;

;; INFO: list library
(use-package dash
  :ensure t
  :defer 0)

;; spinner ;;

;; INFO: loading spinners
(use-package spinner
  :ensure t
  :defer 0)

;; direx ;;

;; INFO: directory/tree explorer
(use-package direx
  :ensure t
  :defer 0)

;; nerd fonts ;;

;; INFO: provides nerd icons for tui/gui compatibility
(use-package nerd-icons
  :ensure t
  :config
  (setq nerd-icons-color-icons t))

(provide 'core)
;;; core.el ends here
