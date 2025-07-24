;; INFO: This module defines the http client integration

(use-package verb
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind
  (:map evil-normal-state-map ("<leader>rr" . verb-send-request-on-point-other-window))
  (:map evil-normal-state-map ("<leader>rx" . verb-kill-all-response-buffers))
  (:map evil-normal-state-map ("<leader>rR" . verb-re-send-request)))

(add-to-list 'display-buffer-alist
             '("\\*verb-response.*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(provide 'rest-module)
