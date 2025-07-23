;; projectile for project management

(use-package projectile
  :config
  (projectile-mode +1)
  :bind
  (:map evil-normal-state-map ("<leader>." . projectile-find-file)))

(provide 'project-module)
