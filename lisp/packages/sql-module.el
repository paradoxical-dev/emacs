; INFO: This module will contain all packages and configuraitons for
;; working with sql in emacs.

;; DEPENDENCIES ;;

;; INFO: https://github.com/clojure-emacs/clomacs
;; clojure and emacs lisp integration
(use-package clomacs
  :ensure t
  :defer 0)

;; clomacs dependency
(use-package cider
  :ensure t
  :defer 0)

;; EJC SQL ;;

;; Will require the system package `Leiningen` (https://leiningen.org/).
;; Also *possibly* requires JDBC drivers for different database types.
(use-package ejc-sql
  :ensure t
  :defer 0
  :bind
  (:map evil-normal-state-map
	("<leader>ss" . ejc-eval-user-sql-at-point)
	("<leader>sc" . ejc-connect)
	("<leader>sC" . ejc-connect-interactive)
	("<leader>sq" . ejc-cancel-query)
	("<leader>sx" . ejc-quit-connection)
	("<leader>sj" . ejc-next-sql)
	("<leader>sk" . ejc-previous-sql)
	("<leader>sp" . ejc-show-prev-result)
	("<leader>sn" . ejc-show-next-result))
  (:map evil-visual-state-map ("<leader>s" . ejc-eval-user-sql-region)))

(provide 'sql-module)
