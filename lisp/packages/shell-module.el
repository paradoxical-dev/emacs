;; TERM TOGGLES ;;

;; kill function
(defun my/setup-vterm-sentinel ()
  "Kill buffer when vterm process exits."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (when (string= event "finished\n")
	   (let ((buf (process-buffer process)))
	     (when (buffer-live-p buf)
	       (let ((win (get-buffer-window buf)))
		 (when win (delete-window win)))
               (kill-buffer buf)))))))))

;; horizontal toggle
(defun my/horizontal-vterm ()
  "Toggle a persistent vterm window at the bottom."
  (interactive)
  (let* ((buf-name "*vterm*")
         (buf (get-buffer buf-name)))
    (if (and buf (get-buffer-window buf))
	;; hide if visible
        (delete-window (get-buffer-window buf))
      (let ((win (split-window-vertically -15)))
        (select-window win)
        (if buf
            (switch-to-buffer buf)
          (vterm)
	  (my/setup-vterm-sentinel))))))

;; vertical toggle
(defun my/vertical-vterm ()
  "Toggle a persistent vterm window on the right."
  (interactive)
  (let* ((buf-name "*vterm*")
         (buf (get-buffer buf-name)))
    (if (and buf (get-buffer-window buf))
        ;; hide if visible
        (delete-window (get-buffer-window buf))
      (let ((win (split-window-horizontally -75)))
        (select-window win)
        (if buf
            (switch-to-buffer buf)
          (vterm)
	  (my/setup-vterm-sentinel))))))

;; CONFIG ;;

(use-package vterm
  :defer t
  :bind (:map evil-normal-state-map
	      ("<leader>tt" . #'my/vertical-vterm)
	      ("<leader>th" . #'my/horizontal-vterm))
  :hook
  (vterm-mode . (lambda () (display-line-numbers-mode 0)))
  :ensure t)

(provide 'shell-module)
