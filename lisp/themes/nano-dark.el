(use-package nano-theme
  :straight (:type git :host github :repo "rougier/nano-theme"))

(load-theme 'nano-dark t)

(set-face-attribute 'default nil :background "#000000")

(set-face-attribute 'window-divider nil :distant-foreground (face-foreground 'default))

(set-face-background 'header-line "#191724")
(with-eval-after-load 'nano-modeline
  (set-face-background 'nano-modeline-face-buffer-read-write "#65617b")
  (set-face-foreground 'nano-modeline-face-secondary "#666680"))
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(set-face-foreground 'nano-faded "#65617b")
(set-face-foreground 'nano-salient "#b49bd7")
(set-face-background 'nano-subtle "#191724")

(set-face-attribute 'font-lock-comment-face nil
		    :font "Victor Mono-14"
		    :weight 'normal
		    :italic t
		    :foreground "#65617b")

(provide 'nano-dark)
