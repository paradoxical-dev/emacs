;; INFO: This module contains all packages and configuraiton related to completion
;; It includes the backend systems for completion functions and frontend displays

;; completion frontend
(use-package corfu
  :ensure t
  :defer t
  :hook ((prog-mode . corfu-mode)
	 (prog-mode . corfu-popupinfo-mode)
	 (text-mode . corfu-mode)
	 (text-mode . corfu-popupinfo-mode)
	 (org-mode . corfu-mode)
	 (org-mode . corfu-popupinfo-mode))
  :config
  ;; (setq corfu-auto t
  ;; 	corfu-quit-no-match 'separator)
  (setq corfu-quit-no-match 'separator)
  (setq corfu-popupinfo-delay 0.1))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; terminal support
(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  ;; only enables in terminal session
  ;; removes popupinfo with echo for documentation
  (unless (display-graphic-p)
    (corfu-popupinfo-mode -1)
    (setq corfu-echo-delay 0.1)
    (corfu-echo-mode +1)
    (corfu-terminal-mode +1)))

;; use tab for completion cycling
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil))

;; snippets
(use-package yasnippet
  :ensure t
  :defer 0
  :config
  (setq yas-snippet-dirs
	'("~/.config/emacs/straight/repos/yasnippet-snippets/snippets"))
  (yas-global-mode 1))

;; snippets bundle
(use-package yasnippet-snippets
  :defer 0
  :ensure t)

;; extensions for completion
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'codeium-completion-at-point))

;; WARN: this will have to be user made.
;; Create the secrets.el file in the root of your config directory and add:
;; `(setq codeium-api-key <your-api-key>)`
(let ((secrets-file (expand-file-name "secrets.el" user-emacs-directory)))
  (when (file-exists-p secrets-file)
    (load secrets-file)))

;; ai-completions
(use-package codeium
  :straight (:host github :repo "Exafunction/codeium.el")
  :config
  (setq codeium/metadata/api_key codeium-api-key))

(use-package corfu-candidate-overlay
  :after (corfu)
  :init
  (setq corfu-auto nil)
  :config
  (keymap-global-set "M-TAB" #'corfu-candidate-overlay-complete-at-point)
  (unless (daemonp)
    (let ((inhibit-message t)
          (message-log-max nil))
      (corfu-candidate-overlay-mode +1))))

;; (use-package supermaven
;;   :straight (:host github :repo "crazywolf132/supermaven.el")
;;   :ensure t
;;   :config
;;   (setq supermaven-log-level 'debug)
;;   (supermaven-mode)
;;   (setq supermaven-keymaps
;; 	'((accept-suggestion . "C-<return>")
;; 	  (clear-suggestion . "C-c C-c")
;; 	  (accept-word . "M-<right>"))))
;; :hook ((prog-mode . supermaven-mode)
;;        (supermaven-mode . supermaven-setup)) ; crucial!
;; :init
;; (setq supermaven-keymaps
;;       '((accept-suggestion . "C-g")
;;         (clear-suggestion . "C-]")
;;         (accept-word . "C-j")))
;; (setq supermaven-ignore-filetypes '("org" "md" "txt"))
;; (setq supermaven-log-level 'debug))

(provide 'completion-module)
