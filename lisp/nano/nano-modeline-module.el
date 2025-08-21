;;; package --- Summary
;;; Commentary:
;; Nano modeline and configuration

;;; Code:

(use-package nano-modeline
  :straight (:type git :host github :repo "rougier/nano-modeline" :branch "rewrite")
  :hook
  (text-mode             . nano-modeline)
  (org-mode              . nano-modeline)
  (vterm-mode             . nano-modeline)
  (messages-buffer-mode  . nano-modeline)
  (prog-mode             . nano-modeline))

(setq-default mode-line-format nil)

(provide 'nano-modeline-module)
;;; nano-modeline.el ends here
