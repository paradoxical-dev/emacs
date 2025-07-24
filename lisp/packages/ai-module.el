;; INFO: This module contains any packages and configurations related to AI workflows.
;; This includes completion, chats, etc.

;; WARN: this will have to be user made.
;; Create the secrets.el file in the root of your config directory and add:
;; `(setq codeium-api-key <your-api-key>)`
(let ((secrets-file (expand-file-name "secrets.el" user-emacs-directory)))
  (when (file-exists-p secrets-file)
    (load secrets-file)))

(use-package codeium
  :straight (:host github :repo "Exafunction/codeium.el")
  :config
  (setq codeium/metadata/api_key codeium-api-key)
  :hook ((prog-mode . (lambda ()
			(setq-local completion-at-point-functions '(codeium-completion-at-point))))))

(provide 'ai-module)
