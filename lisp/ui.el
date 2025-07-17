;; theme
(load-theme 'modus-vivendi-tinted t)

;; font
(set-frame-font "JetBrainsMono Nerd Font-12" t t)
;;(setq-default line-spacing 8)

;; disable elements
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; which key
(which-key-mode 1)
(setq which-key-idle-delay 0.1)

(provide 'ui)
