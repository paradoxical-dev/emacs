;;; nano-layout.el --- Summary
;;; Commentary:
;; Nano layout and configurations for padding and frame defaults

;;; Code:

(setq default-frame-alist
      (append (list
	           '(min-height . 1)
               '(height     . 45)
	           '(min-width  . 1)
               '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 20)
               '(left-fringe    . 8)
               '(right-fringe   . 8)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

(setq widget-image-enable nil) ; no checkbox icons, just text
(setq org-hide-emphasis-markers t) ; hides *bold* markers in Org mode
(setq x-underline-at-descent-line t) ; prettier underlines

(add-hook 'term-mode-hook
	  (lambda () (setq buffer-display-table (make-display-table))))

(set-face-background 'window-divider (face-background 'default))
(set-face-background 'window-divider-first-pixel (face-background 'default))
(set-face-background 'window-divider-last-pixel (face-background 'default))

(set-face-foreground 'window-divider (face-background 'default))
(set-face-foreground 'window-divider-first-pixel (face-background 'default))
(set-face-foreground 'window-divider-last-pixel (face-background 'default))
(set-face-attribute 'window-divider nil :distant-foreground (face-background 'default))

(set-face-background 'mode-line-inactive (face-background 'default))
(set-face-background 'mode-line-active (face-background 'default))

(provide 'nano-layout)
;;; nano-layout.el ends here
