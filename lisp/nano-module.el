;;; package --- Summary
;;; Commentary:
;; Loads nano modules with personal tweaks.  Most of these will be
;; copy and pased from the nano repo: https://github.com/rougier/nano-emacs

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/nano" user-emacs-directory))
;; (require 'nano-base-colors)
;; (require 'nano-faces)
;; (require 'nano-theme)

;; requires mini-frame
;; (use-package mini-frame
;;   :ensure t
;;   :defer t)
;; (require 'nano-minibuffer)

(require 'nano-modeline-module)
(require 'nano-layout)
(require 'nano-defaults)

(provide 'nano-module)
;;; nano.el ends here
