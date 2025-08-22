;;; org-module.el --- Summary
;;; Commentary:
;; This module contains all packages and configuraiton related to org-mode

;;; Code:

;; will not pull in org-mode
(use-package org
  :straight (:type built-in)
  :defer 0)

;; global modern mode
(with-eval-after-load 'org (global-org-modern-mode))

;; remove line numbers and wrap lines
(add-hook 'org-mode-hook (lambda ()
                           (display-line-numbers-mode 0)
                           (visual-line-mode 1)))

;; org-roam
(use-package org-roam
  :after org
  :straight t
  :config
  (setq org-roam-directory "~/Org")
  (org-roam-db-autosync-mode)
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

;; book mode
;; provides centered reading/editing with custom heeader and mode lines

(defun my/book-mode-setup ()
  "Setup book mode while preserving current window configuration."
  (book-mode)
  ;; preserve frame padding
  (let ((f (selected-frame)))
    (set-frame-parameter f 'min-height 1)
    (set-frame-parameter f 'height 45)
    (set-frame-parameter f 'min-width 1)
    (set-frame-parameter f 'width 81)
    (set-frame-parameter f 'vertical-scroll-bars nil)
    (set-frame-parameter f 'internal-border-width 20)
    (set-frame-parameter f 'left-fringe 8)
    (set-frame-parameter f 'right-fringe 8)
    (set-frame-parameter f 'tool-bar-lines 0)
    (set-frame-parameter f 'menu-bar-lines 0))

  ;; window divider
  (setq window-divider-default-right-width 24)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1))

(use-package book-mode
  :straight (:type git :host github :repo "rougier/book-mode")
  :defer t
  :config
  (setq book-mode-frame-border-color (face-background 'default))
  (setq book-mode-top-margin 1.75)
  (setq book-mode-bottom-margin 0.75)
  :hook
  (org-mode . my/book-mode-setup))
  ;; (org-mode . book-mode))

;; olivetti to pair with book-mode
(use-package olivetti
  :ensure t
  :hook
  (org-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 75))

(provide 'org-module)
;;; org-module.el ends here
