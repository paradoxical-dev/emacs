;; INFO: This is the "kickoff" file for loading package modules.
;; These will be stored in lisp/packages and can be enabled/disabled from here.

;; add lisp/packages to load path
(add-to-list 'load-path (expand-file-name "packages" (file-name-directory load-file-name)))

;; load packages
(require 'evil)
(require 'search-module)
(require 'shell-module)
(require 'projectile)
(require 'git-module)
(require 'treesitter)
(require 'lsp-module)

(provide 'packages)
