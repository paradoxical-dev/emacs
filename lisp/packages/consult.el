;; INFO: This defines the mappings for consult search functions

(use-package consult
  :config
  (setq consult-async-min-input 0)
  :bind
  ;; files
  (:map evil-normal-state-map ("<leader>ff" . consult-fd))
  (:map evil-normal-state-map ("<leader>fo" . consult-recent-file))
  (:map evil-normal-state-map ("<leader>fb" . consult-buffer))
  (:map evil-normal-state-map ("<leader>fw" . consult-ripgrep))
  ;; etc
  (:map evil-normal-state-map ("<leader>fi" . consult-info))
  (:map evil-normal-state-map ("<leader>fm" . consult-man)))


(provide 'consult)
