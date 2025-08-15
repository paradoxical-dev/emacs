;; INFO: This module houses all editor specific packages and configs.
;; For instance, line highlights, mode lines, etc.

;; INDENT ;;

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0)

  (let ((bg (face-background 'default))
	(fg (face-foreground 'link)))
    (set-face-attribute 'highlight-indent-guides-top-character-face nil :background bg :foreground fg)
    (set-face-attribute 'highlight-indent-guides-character-face nil :background bg :foreground "#303030"))

  :hook (prog-mode . highlight-indent-guides-mode))

;; ZEN MODE ;;

(use-package darkroom
  :straight '(:type git :host github :repo "joaotavora/darkroom")
  :config
  (setq darkroom-text-scale-increase 0.8)
  :bind
  (:map evil-normal-state-map ("<leader>z" . darkroom-mode)))

;; TODO HIGHLIGHTS ;;

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-keyword-faces
	'(("TODO"   . "#ff0000")
	  ("FIXME"  . "#FF0000")
	  ("DEBUG"  . "#A020F0")
	  ("GOTCHA" . "#FF4500")
	  ("INFO"   . "#1E90FF")
	  ("STUB"   . "#1E90FF")))
  :hook (prog-mode . hl-todo-mode))

(provide 'editor-module)
