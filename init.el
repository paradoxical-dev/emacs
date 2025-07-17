;; STRAIGHT ;;

;; INFO: this bootstraps straight.el for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; the straight.el docs reccomend this line for versions >= 27
(setq package-enable-at-startup nil)

;; installs use-package
(straight-use-package 'use-package)

;; set var to make straight use use-package by default
(setq straight-use-package-by-default t)

;; MODULES ;;

;; add /lisp to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; load modules
(require 'packages)
(require 'ui)
