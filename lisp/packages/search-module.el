;; INFO: Advanced search minibuffer

;; base
(use-package vertico
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
	("C-j" . vertico-next)
	("C-k" . vertico-previous)))

;; (use-package vertico-posframe
;;   :ensure t
;;   :after vertico
;;   :config
;;   (setq vertico-posframe-parameters
;; 	'((left-fringe . 20)
;; 	  (right-fringe . 20)))

;;   ;; (setq vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
;;   (setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)

;;   (set-face-background 'vertico-posframe "#191724")
;;   (set-face-foreground 'vertico-posframe-border "#191724")
;;   (set-face-background 'vertico-posframe-border "#191724")

;;   ;; (setq vertico-posframe-min-width (frame-width))

;;   (vertico-posframe-mode 1))

;; persistent history
(use-package savehist
  :init
  (savehist-mode))

;; orderless completion
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; entry annotations
(use-package marginalia
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; needed for vertico buffer as stated in docs
(use-package emacs
  :custom
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; defines the mappings for consult search functions
(use-package consult
  :config
  (recentf-mode)
  (setq consult-async-min-input 0)
  ;; include hidden files
  (setq consult-fd-args
      `((if (executable-find "fdfind" 'remote) "fdfind" "fd")
     "--full-path" "--color=never" "--hidden" "--exclude=.git"))
  :bind
  ;; files
  (:map evil-normal-state-map ("<leader>ff" . consult-fd))
  (:map evil-normal-state-map ("<leader>fo" . consult-recent-file))
  (:map evil-normal-state-map ("<leader>fb" . consult-buffer))
  (:map evil-normal-state-map ("<leader>fw" . consult-ripgrep))
  ;; etc
  (:map evil-normal-state-map ("<leader>fi" . consult-info))
  (:map evil-normal-state-map ("<leader>fm" . consult-man)))

(provide 'search-module)
