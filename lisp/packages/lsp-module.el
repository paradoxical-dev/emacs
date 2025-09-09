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

;; qml
(use-package qml-ts-mode
  :straight (:type git :host github :repo "xhcoding/qml-ts-mode")
  :after lsp-mode
  :mode ("\\.qml\\'" "\\.qmljs\\'")
  :config
  (add-to-list 'lsp-language-id-configuration '(qml-ts-mode . "qml-ts"))
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   ;; WARNING: Need to remove this after moving from NixOS
                   ;; I fucking hate NixOS
                   '("qmlls"
                     "-I" "/home/gitmoney/.config/quickshell"   ;; custom module
                     "-I" "/nix/store/bijr4q2zjgxj06v074d3icp3nix1qnkk-quickshell-0.1.0/lib/qt-6/qml" ;; Quickshell
                     "-I" "/nix/store/g0ahmk2wixwwibdb49p29l69vhm6ymwa-qtdeclarative-6.9.0/lib/qt-6/qml")) ;; QtQuick
  :activation-fn (lsp-activate-on "qml-ts")
  :server-id 'qmlls)))

  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection '("qmlls -I /home/gitmoney//.config/quickshell:/nix/store/g0ahmk2wixwwibdb49p29l69vhm6ymwa-qtdeclarative-6.9.0/lib/qt6/qml:/nix/store/bijr4q2zjgxj06v074d3icp3nix1qnkk-quickshell-0.1.0/"))
  ;;                   :activation-fn (lsp-activate-on "qml-ts")
  ;;                   :server-id 'qmlls)))

  ;; (add-hook 'qml-ts-mode-hook (lambda ()
  ;;                               (setq-local electric-indent-chars '(?\n ?\( ?\) ?{ ?} ?\[ ?\] ?\; ?,))
  ;;                               (lsp-deferred))))

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
	;; lsp-headerline-breadcrumb-segments '(project file symbols)
	lsp-headerline-breadcrumb-enable nil
	lsp-warn-no-matched-clients nil
	gc-cons-threshold 100000000
	lsp-use-plists 1
	lsp-log-io nil)
  (setq flymake-show-diagnostics-at-end-of-line t)
  ;; server specific ;;
  ;; nix
  (setq lsp-nix-auto-eval-inputs nil))

(use-package flycheck
  :ensure t
  ;; :defer 0
  :hook
  (emacs-startup . global-flycheck-mode))

(use-package flyover
  :ensure t
  :after flycheck
  :hook
  (flycheck-mode . flyover-mode)
  (flymake-mode . flyover-mode)
  :config
  (setq flyover-levels '(error warning info))
  (setq flyover-background-lightness 45)
  ;; (setq flyover-use-theme-colors t)
  (setq flyover-debounce-interval 0.2) 
  ;; (setq flyover-line-position-offset 1)
  (setq flyover-show-at-eol t)
  (setq flyover-show-virtual-line nil)
  (setq flyover-wrap-messages nil)
  ;; (setq flyover-max-line-length 80)
  (setq flyover-checkers '(flycheck flymake)))


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

  (setq lsp-ui-sideline-show-diagnostics nil)

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
