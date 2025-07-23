;; INFO: Defines vim keymaps and behaviors through evil packages

;; base
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (evil-set-leader '(normal visual motion) (kbd "SPC")))

;; undo-tree for C-r map
(use-package undo-tree
  :ensure t
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
  :after evil
  :hook ((prog-mode . evil-surround-mode)
	 (text-mode . evil-surround-mode)
	 (org-mode . evil-surround-mode)))

;; auto comment
(use-package evil-commentary
  :ensure t
  :after evil
  :hook ((prog-mode . evil-commentary-mode)
	 (text-mode . evil-commentary-mode)
	 (org-mode . evil-commentary-mode)))

;; better search
(use-package evil-visualstar
  :ensure t
  :after evil
  :hook ((prog-mode . evil-visualstar-mode)
	 (text-mode . evil-visualstar-mode)
	 (org-mode . evil-visualstar-mode)))

(provide 'evil-module)
