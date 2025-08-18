;; INFO: This module covers all lsp functionality over various packages
;; with lsp-mode being the base for the facilitation.
;; It also includes the mode installation for various languages

;; LANGUAGES ;;

;; nix
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;; python
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :custom (lsp-pyright-langserver-command "pyright")
  :hook
  (python-mode . (lambda ()
		   (require 'lsp-pyright)
		   (lsp-deferred))))

(use-package pyvenv
  :commands (pyvenv-workon pyvenv-activate)
  :ensure t)

;; rust
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

;; LSP ;;

;; base lsp backend
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  ;; do not load for elisp
  ((prog-mode . (lambda ()
		  (unless (derived-mode-p 'emacs-lisp-mode)
		    (lsp-deferred)))))
  (text-mode . lsp-deferred)
  (org-mode . lsp-deferred)
  ((lsp-mode . lsp-enable-which-key-integration))
  :bind
  (:map evil-normal-state-map ("<leader>lR" . lsp-rename))
  (:map evil-normal-state-map ("<leader>lC" . lsp-execute-code-action))
  :config
  (setq lsp-enable-symbol-highlighting t
	lsp-enable-snippet t
	lsp-idle-delay 0.25
	lsp-headerline-breadcrumb-segments '(project file symbols)
	lsp-warn-no-matched-clients nil
	gc-cons-threshold 100000000
	lsp-use-plists 1
	lsp-log-io nil)
  (setq flymake-show-diagnostics-at-end-of-line t)
  ;; server specific ;;
  ;; nix
  (setq lsp-nix-auto-eval-inputs nil))

;; (use-package flycheck
;;   :ensure t
;;   :defer 0)

;; consult replacement for helm-lsp
(use-package consult-lsp
  :ensure t
  :after (lsp-mode consult)
  :bind
  (:map evil-normal-state-map ("<leader>lS" . consult-lsp-symbols))
  (:map evil-normal-state-map ("<leader>ls" . consult-lsp-file-symbols))
  (:map evil-normal-state-map ("<leader>ld" . consult-lsp-diagnostics)))

;; treemacs integrations
(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :hook
   (lsp-treemacs-generic-mode . (lambda () (display-line-numbers-mode 0)))
  :bind
  (:map evil-normal-state-map ("<leader>le" . lsp-treemacs-errors-list))
  (:map evil-normal-state-map ("<leader>lc" . lsp-treemacs-call-hierarchy))
  (:map evil-normal-state-map ("<leader>lt" . lsp-treemacs-type-hierarchy))
  (:map evil-normal-state-map ("<leader>lo" . lsp-treemacs-symbols)))

;; extra ui effects
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  ;; doc
  (setq lsp-ui-doc-position 'at-point
	lsp-ui-doc-border "none")

  ;; peek ;;

  ;; maps
  (define-key lsp-ui-peek-mode-map (kbd "C-j") #'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "C-l") #'lsp-ui-peek--select-next-file)
  (define-key lsp-ui-peek-mode-map (kbd "C-k") #'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map (kbd "C-h") #'lsp-ui-peek--select-prev-file)
  ;; colors
  (let ((bg (face-background 'menu))
	(fg (face-foreground 'font-lock-keyword-face)))
    (set-face-attribute 'lsp-ui-peek-header nil :background bg :foreground fg)
    (set-face-attribute 'lsp-ui-peek-footer nil :background bg :foreground fg))

  (let ((bg (face-background 'widget-field))
	(fg (face-foreground 'font-lock-builtin-face)))
    (set-face-attribute 'lsp-ui-peek-filename nil :background bg :foreground fg))

  (let ((bg (face-background 'highlight))
	(fg (face-foreground 'highlight)))
    (set-face-attribute 'lsp-ui-peek-highlight nil :background bg :foreground fg :box nil))

  (let ((bg (face-background 'region))
	(fg (face-foreground 'region)))
    (set-face-attribute 'lsp-ui-peek-selection nil :background bg :foreground fg))

  (let ((bg (face-background 'menu))
	(fg (face-foreground 'default)))
    (set-face-attribute 'lsp-ui-peek-peek nil :background bg :foreground fg)
    (set-face-attribute 'lsp-ui-peek-list nil :background bg :foreground fg))

  ;; (setq lsp-ui-peek-footer 'default)
  :bind
  (:map evil-normal-state-map ("<leader>lr" . lsp-ui-peek-find-references))
  (:map evil-normal-state-map ("<leader>lg" . lsp-ui-peek-find-definitions))
  (:map evil-normal-state-map ("K" . lsp-ui-doc-glance))
  (:map evil-normal-state-map ("<leader>li" . lsp-ui-peek-find-implementations)))


(provide 'lsp-module) 
