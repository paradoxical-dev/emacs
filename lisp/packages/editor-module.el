;; INFO: This module houses all editor specific packages and configs.
;; For instance, line highlights, mode lines, etc.

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0)

  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")

  (let ((bg (face-background 'default))
	(fg (face-foreground 'link)))
    (set-face-attribute 'highlight-indent-guides-top-character-face nil :background bg :foreground fg)
    (set-face-attribute 'highlight-indent-guides-character-face nil :background bg :foreground "#303030"))

  :hook (prog-mode . highlight-indent-guides-mode))

(provide 'editor-module)
