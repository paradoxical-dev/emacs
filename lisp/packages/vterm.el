;; TERM TOGGLES ;;

;; horizontal toggle
(defun my/horizontal-vterm ()
  "Toggle a persistent vterm window at the bottom."
  (interactive)
  (let* ((buf-name "*vterm*")
         (buf (get-buffer buf-name)))
    (if (and buf (get-buffer-window buf))
        (delete-window (get-buffer-window buf))
      (let ((win (split-window-vertically -15)))
        (select-window win)
        (if buf
            (switch-to-buffer buf)
          (vterm))))))

;; vertical toggle
(defun my/vertical-vterm ()
  "Toggle a persistent vterm window on the right."
  (interactive)
  (let* ((buf-name "*vterm*")
         (buf (get-buffer buf-name)))
    (if (and buf (get-buffer-window buf))
        ;; If visible, hide it
        (delete-window (get-buffer-window buf))
      ;; Else create a vertical split on the right
      (let ((win (split-window-horizontally -75))) ;; adjust -60 to control width
        (select-window win)
        (if buf
            (switch-to-buffer buf)
          (vterm))))))

;; CONFIG ;;

(use-package vterm
  :bind (:map evil-normal-state-map
	      ("<leader>tt" . #'my/vertical-vterm)
	      ("<leader>th" . #'my/horizontal-vterm))
  :ensure t)

(provide 'vterm)
