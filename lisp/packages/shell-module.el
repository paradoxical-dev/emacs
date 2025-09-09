;;; shell-module.el --- shell integration -*- lexical-binding: t; -*-
;;; Commentary:
;; TERM TOGGLES ;;

;;; Code:

;; kill function
(defun my/setup-vterm-sentinel ()
  "Close vterm window when the vterm shell process exits normally."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (when (and (eq (process-status process) 'exit)
                    (buffer-live-p (process-buffer process)))
           (let ((buf (process-buffer process)))
             (when-let ((win (get-buffer-window buf)))
               (delete-window win))
             (kill-buffer buf))))))))

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

;; fullscreen toggle
(defun my/fullscreen-vterm ()
  "Toggle a persistent vterm in the current window, deleting all others."
  (interactive)
  (let* ((buf-name "*vterm*")
         (buf (get-buffer buf-name)))
    (if (and buf (get-buffer-window buf))
        ;; hide if visible
        (progn
          (kill-buffer buf)
          (when (one-window-p) (delete-other-windows)))
      (progn
        (delete-other-windows)
        (if buf
            (switch-to-buffer buf)
          (vterm))))))
          ;; (my/setup-vterm-sentinel))))))

;; CONFIG ;;

(use-package vterm
  :defer t
  :bind (:map evil-normal-state-map
	      ("<leader>tt" . #'my/vertical-vterm)
	      ("<leader>tf" . #'my/fullscreen-vterm)
	      ("<leader>th" . #'my/horizontal-vterm))
  :hook
  (vterm-mode . (lambda () (display-line-numbers-mode 0)))
  :ensure t)

(provide 'shell-module)
;;; shell-module.el ends here
