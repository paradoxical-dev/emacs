;;; packages.el --- Summary
;;; Commentary:
;; INFO: This is the "kickoff" file for loading package modules.
;; These will be stored in lisp/packages and can be enabled/disabled from here.

;;; Code:

;; add lisp/packages to load path
(add-to-list 'load-path (expand-file-name "packages" (file-name-directory load-file-name)))

;; load packages
(require 'evil-module)
(require 'editor-module)
(require 'ts-module)
(require 'search-module)
(require 'completion-module)
(require 'project-module)
(require 'git-module)
(require 'shell-module)
(require 'lsp-module)
(require 'dap-module)
(require 'sql-module)
(require 'org-module)
(require 'rest-module)

(provide 'packages)
;;; packages.el ends here
