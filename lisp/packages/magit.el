;; INFO: This will cover all of the bindings and configuration for git integrations

;; MAGIT ;;

(use-package magit
  :ensure t)

;; set default behavior for ediff
(with-eval-after-load 'ediff
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; WARNING: Have to set bindings after magit loads.
;; Otherwise we get the "Symbol's value as variable void... error"

;; function to simplify keymap setting
(defun my/evil-normal-leader-bind (key func)
  "Bind KEY to FUNC in `evil-normal-state-map` using <leader> prefix."
  (let ((full-key (concat "" key)))
    (with-eval-after-load 'evil
      (define-key evil-normal-state-map (kbd full-key) func))))

(my/evil-normal-leader-bind "<leader>gg" #'magit-status)
(my/evil-normal-leader-bind "<leader>gl" #'magit-log-all)
(my/evil-normal-leader-bind "<leader>gd" #'magit-ediff)
(my/evil-normal-leader-bind "<leader>gb" #'magit-branch)
(my/evil-normal-leader-bind "<leader>gp" #'magit-pull)
(my/evil-normal-leader-bind "<leader>gP" #'magit-push)

;; DIFF HL ;;

(use-package diff-hl
  :ensure t
  :config
  (setq vc-handled-backends '(Git))
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(my/evil-normal-leader-bind "]h" #'diff-hl-show-hunk-next)
(my/evil-normal-leader-bind "[h" #'diff-hl-show-hunk-previous)
(my/evil-normal-leader-bind "<leader>gh" #'diff-hl-stage-current-hunk)


(provide 'magit)
