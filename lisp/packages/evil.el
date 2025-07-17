;; INFO: Defines vim keymaps and behaviors through evil packages

;; base
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (evil-set-leader '(normal visual motion) (kbd "SPC")))

;; undo-tree for C-r map
(use-package undo-tree
  :ensure t
  :init
  (setq evil-undo-system 'undo-tree)
  :config
  (global-undo-tree-mode))

;; maps in other tools
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; surround for text objs
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; auto comment
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

;; better search
(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode))

(provide 'evil)
