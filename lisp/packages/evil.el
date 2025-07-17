;; INFO: Defines vim keymaps and behaviors through evil packages

;; base
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-leader 'normal (kbd "SPC"))) ; set leader to space

;; maps in other tools
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(provide 'evil)
