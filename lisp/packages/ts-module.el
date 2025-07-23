;; INFO: This controls treesitter for syntax highlighting and manipulation

(use-package tree-sitter
  :ensure t
  :hook (prog-mode . tree-sitter-mode)
  :config
  ;; (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(provide 'ts-module)
