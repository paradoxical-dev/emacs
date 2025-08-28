;;; pdf-module.el --- Summary
;;; Commentary:
;; This module contains all packages and configuraiton related to pdf-tools

;;; Code:

(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (setq pdf-view-midnight-colors '("#e0def4" . "#000000"))
  (pdf-tools-install))

(provide 'pdf-module)
;;; pdf-module.el ends here
