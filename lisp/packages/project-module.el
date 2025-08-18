;; INFO: Project and file management

(use-package transient
  :ensure t)

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :after transient
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("s" "/ssh:my-remote-server"      "SSH server")
     ("e" "/sudo:root@localhost:/etc"  "Modify program settings")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  (dirvish-peek-mode)             ; Preview files in minibuffer
  (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'

  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))

  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))

  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)

  (custom-set-faces                 ; re set commit messafe face
   '(dirvish-git-commit-message-face ((t (:background "#00000000"))))
   '(dirvish-subtree-guide ((t (:background "#00000000"))))
   '(dirvish-subtree-state ((t (:background "#00000000")))))

  ;; any keymaps which need to be manually remapped due to evil
  (with-eval-after-load 'dirvish
    (evil-define-key 'normal dirvish-mode-map
      "q" 'dirvish-quit
      "h" 'dired-up-directory
      "l" 'dired-find-file
      "?" 'dirvish-dispatch
      "TAB" 'dirvish-subtree-toggle
      "L" 'dirvish-ls-switches-menu
      "f" 'dirvish-file-info-menu
      "a" 'dirvish-setup-menu
      "o" 'dirvish-quick-access
      "s" 'dirvish-quicksort
      "r" 'dirvish-history-jump
      "v" 'dirvish-vc-menu
      "*" 'dirvish-mark-menu
      "y" 'dirvish-yank-menu
      "N" 'dirvish-narrow
      "^" 'dirvish-history-last))

  :bind
   (:map evil-normal-state-map
   ("<leader>m" . dirvish))
   (:map dirvish-mode-map
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

(use-package projectile
  :config
  (projectile-mode +1)
  :bind
  (:map evil-normal-state-map
	("<leader>pp" . projectile-switch-project)
	("<leader>pc" . projectile-compile-project)
	("<leader>pi" . projectile-install-project)
	("<leader>px" . projectile-test-project)
	("<leader>." . projectile-find-file)))

(provide 'project-module)
