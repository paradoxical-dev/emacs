;; INFO: This defines the mappings for consult search functions

(use-package consult
  :config
  (setq consult-async-min-input 1)
  :bind
  (:map evil-normal-state-map ("<leader>ff" . consult-fd))
  (:map evil-normal-state-map ("<leader>fo" . consult-recent-file))
  (:map evil-normal-state-map ("<leader>fb" . consult-buffer)))

(provide 'consult)
