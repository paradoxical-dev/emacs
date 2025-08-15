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
  (setq corfu-auto t
	corfu-quit-no-match 'separator)
  ;; (setq corfu-quit-no-match 'separator)
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
  (add-hook 'completion-at-point-functions #'cape-keyword))
  ;; (add-hook 'completion-at-point-functions #'codeium-completion-at-point))

;; ai-completions ;;

;; WARN: this will have to be user made.
;; Create the secrets.el file in the root of your config directory and add:
;; `(setq codeium-api-key <your-api-key>)`
(let ((secrets-file (expand-file-name "secrets.el" user-emacs-directory)))
  (when (file-exists-p secrets-file)
    (load secrets-file)))

(defvar my/comingle-is-enabled t)
(use-package comingle
  :straight '(:type git :host github :repo "jeff-phil/comingle.el")
  :diminish "🧠"
  :commands (my/comingle-toggle comingle-mode)
  :bind
  (:map evil-normal-state-map
	("<leader>cc" . my/comingle-toggle)
	("<leader>co" . comingle-chat-open))
  (:map evil-insert-state-map
	("M-TAB" . comingle-accept-completion)
	("C-<return>" . comingle-accept-completion-line)
	("C-j" . comingle-next-completion)
	("C-k" . commingle-next-completion))
  :hook (prog-mode . my/try-run-comingle-mode)
  :init
  ;; optionally set a timer, which might speed up things as the
  ;; comingle local language server takes ~0.2s to start up
  (when (bound-and-true-p my/comingle-is-enabled)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (run-with-timer
                 0.1
                 nil
                 (lambda ()
                   (when (comingle-state-proc comingle-state)
                     ;; temporarily disable comingle, so that we can init
                     (let ((my/comingle-is-enabled nil))
                       (my/comingle-toggle))))))))
  :config
  (defun my/try-run-comingle-mode ()
    "Use in hook, like prog-mode-hook, so that can test first if comingle is enabled."
    (when (bound-and-true-p my/comingle-is-enabled)
      (comingle-mode)))

  (defun my/comingle-toggle (&optional state)
    "Toggle comingle's enabled state."
    (interactive)
    (let ((current-state (or state comingle-state)))
      (cond
       ;; First condition: Is comingle currently enabled?
       (my/comingle-is-enabled
         (when (and current-state (comingle-state-proc current-state))
           ;; -> Then, disable it. No `progn` needed.
           (comingle-reset))
        (setq my/comingle-is-enabled nil)
        (message "Comingle disabled"))
       ;; if you don't want to use customize to save the api-key
       (t (setopt comingle/metadata/api_key codeium-api-key)
          ;; Reset only if it was already in a valid (but not running) state
          (when current-state
            (comingle-reset))
          (comingle-init)
          (setq my/comingle-is-enabled t)
          (message "Comingle enabled")))))

  (setq use-dialog-box nil) ;; do not use popup boxes

  ;; get comingle status in the modeline
  (setq comingle-mode-line-enable
        (lambda (api)
          (when (bound-and-true-p my/comingle-is-enabled)
            (not (memq api '(CancelRequest Heartbeat AcceptCompletion))))))
  (add-to-list 'mode-line-format '(:eval (car-safe comingle-mode-line)) t)
  ;; alternatively for a more extensive mode-line
  (add-to-list 'mode-line-format '(-50 "" comingle-mode-line) t)

  ;; You can overwrite all the comingle configs!
  ;; for example, we recommend limiting the string sent to comingle for better perf
  (defun my/comingle/document/text ()
    (buffer-substring-no-properties
     (max (- (point) 3000) (point-min))
     (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my/comingle/document/cursor_offset ()
    (comingle-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq comingle/document/text 'my/comingle/document/text)
  (setq comingle/document/cursor_offset 'my/comingle/document/cursor_offset))

(provide 'completion-module)
