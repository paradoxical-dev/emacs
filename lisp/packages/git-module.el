;; INFO: This will cover all of the bindings and configuration for git integrations

;; MAGIT ;;

(use-package magit
  ;; :init (require 'magit)
  :commands (magit-status magit-log-all magit-ediff magit-branch magit-pull magit-push)
  :bind
  (:map evil-normal-state-map ("<leader>gg" . magit-status))
  (:map evil-normal-state-map ("<leader>gl" . magit-log-all))
  (:map evil-normal-state-map ("<leader>gd" . magit-ediff))
  (:map evil-normal-state-map ("<leader>gb" . magit-branch))
  (:map evil-normal-state-map ("<leader>gp" . magit-pull))
  (:map evil-normal-state-map ("<leader>gP" . magit-push))
  :ensure t)

;; set default behavior for ediff
(with-eval-after-load 'ediff
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; DIFF HL ;;

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  :config
  ;; (setq vc-handled-backends '(Git))
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind
  (:map evil-normal-state-map ("]h" . diff-hl-show-hunk-next))
  (:map evil-normal-state-map ("[h" . diff-hl-show-hunk-previous))
  (:map evil-normal-state-map ("<leader>gh" . diff-hl-stage-current-hunk)))

(provide 'git-module)
