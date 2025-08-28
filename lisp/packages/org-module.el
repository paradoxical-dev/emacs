;;; org-module.el --- Summary
;;; Commentary:
;; This module contains all packages and configuraiton related to org-mode

;;; Code:

;;                  ;;
;; Builtin packages ;;
;;                  ;;

;; will not pull in org-mode
(use-package org
  :straight (:type built-in)
  :defer 0)

;; header faces
(custom-set-faces
 '(org-level-1 ((t (:height 1.5 :weight bold))))
 '(org-level-2 ((t (:height 1.3 :weight bold))))
 '(org-level-3 ((t (:height 1.1 :weight bold))))
 '(org-level-4 ((t (:height 1.0 :weight semi-bold))))
 '(org-level-5 ((t (:height 0.95 :weight normal)))))

;; global modern mode
(with-eval-after-load 'org (global-org-modern-mode))
(setq
 org-modern-star 'replace
 org-modern-replace-stars "◉○◈◇✳"
 org-modern-fold-stars nil
 org-modern-list '((?- . "•") (?+ . "◦") (?* . "◆"))
 org-startup-with-inline-images t
 org-startup-with-latex-preview t
 org-hide-emphasis-markers t
 org-pretty-entities t)

;; remove line numbers and wrap lines
(add-hook 'org-mode-hook (lambda ()
                           (display-line-numbers-mode 0)
                           (visual-line-mode 1)))

;; babel
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(( emacs-lisp . t)
     ( latex      . t)
     ( python     . t)
     ( shell      . t))))

;; custom agenda
(use-package nano-agenda
  :straight (:type git
                   :host github
                   :repo "rougier/nano-agenda"
                   :branch "rewrite")
  :after org
  :config
  (setq nano-agenda-header-show nil))


;;          ;;
;; Org Roam ;;
;;          ;;

(use-package org-roam
  :after org
  :straight t
  :config
  (setq org-roam-directory "~/Org")
  (org-roam-db-autosync-mode)
  (setq org-agenda-files (directory-files-recursively org-roam-directory "\\.org$"))
  :hook
  (org-roam-db-autosync-mode . (lambda ()
                                 (setq org-agenda-files
                                       (directory-files-recursively org-roam-directory "\\.org$"))))
  :bind
  (:map evil-normal-state-map
        ("<leader>oi" . org-roam-node-insert)
        ("<leader>of" . org-roam-node-find)))

(use-package org-roam-ui
  :straight t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t))


;;           ;;
;; Book Mode ;;
;;           ;;

;; centered reading/editing
(use-package olivetti
  :ensure t
  :hook
  (org-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 75))

(provide 'org-module)
;;; org-module.el ends here
