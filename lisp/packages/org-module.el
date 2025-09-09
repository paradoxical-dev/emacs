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
(with-eval-after-load 'org
  (setq
   org-modern-star 'replace
   org-modern-replace-stars "◉○◈◇✳"
   org-modern-fold-stars nil
   org-modern-list '((?- . "•") (?+ . "◦") (?* . "◆"))
   org-startup-with-inline-images t
   org-startup-with-latex-preview t
   org-hide-emphasis-markers t
   org-pretty-entities t)
  (global-org-modern-mode))

;; remove line numbers and wrap lines
(add-hook 'org-mode-hook (lambda ()
                           (display-line-numbers-mode 0)
                           (visual-line-mode 1)))
(add-hook 'org-agenda-mode-hook (lambda ()
                           (display-line-numbers-mode 0)
                           (visual-line-mode 1)))

;; babel
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(( emacs-lisp . t)
     ( latex      . t)
     (sql         . t)
     ( python     . t)
     ( shell      . t))))

;; syntax highlighting in src blocks
(with-eval-after-load 'org
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-edit-src-content-indentation 0)

  (add-to-list 'org-src-lang-modes
               '("python" . python-ts)
               '("qml"    . qml-ts)))

;;                   ;;
;; AGENDA EXTENSIONS ;;
;;                   ;;

;; nano agenda
(use-package nano-agenda
  :straight (:type git
                   :host github
                   :repo "rougier/nano-agenda"
                   :branch "rewrite")
  :after org
  :config
  (setq nano-agenda-header-show nil))

(use-package org-super-agenda
  :after org
  :config
  (org-super-agenda-mode))

(use-package org-timeblock
  :after org
  :config
  (org-timeblock-mode))


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
        ("<leader>ox" . org-toggle-checkbox)
        ("<leader>od" . org-deadline)
        ("<leader>os" . org-schedule)
        ("<leader>ot" . org-todo)
        ("<leader>oa" . nano-agenda)
        ("<leader>oA" . org-agenda)
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
  (nano-agenda-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 75))

(provide 'org-module)
;;; org-module.el ends here
