;; INFO: This controls treesitter for syntax highlighting and manipulation

;; (use-package tree-sitter
;;   :ensure t
;;   :hook ((prog-mode . tree-sitter-mode)
;; 	 (text-mode . tree-sitter-mode)
;; 	 (org-mode . tree-sitter-mode))
;;   :config
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter)

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (setq treesit-language-source-alist
	'((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
	  (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
	  (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
	  (json       . ("https://github.com/tree-sitter/tree-sitter-json" "master" "src"))
	  (css        . ("https://github.com/tree-sitter/tree-sitter-css" "master" "src"))
	  (python     . ("https://github.com/tree-sitter/tree-sitter-python" "master" "src"))))

  (setq major-mode-remap-alist
        '((typescript-mode . typescript-ts-mode)
          (tsx-mode . tsx-ts-mode)
          (js-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))

  (dolist (lang '(typescript tsx javascript json css python))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

;; manual remap for ts modes
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))


(provide 'ts-module)
