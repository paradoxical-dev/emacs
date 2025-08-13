;; theme
(use-package autothemer
  :ensure t)

(straight-use-package
 '(pinerose-emacs
   :host github
   :repo "konrad1977/pinerose-emacs"))

(load-theme 'rose-pine t)

(if (display-graphic-p)
    (set-face-attribute 'default nil
                        :background "#000000"
                        :foreground "#e0def4")
    (set-face-foreground 'vertico-current "e0def4")
  (set-face-attribute 'default nil
                      :background "undefined"
                      :foreground "#e0def4"))

;; font
(set-frame-font "JetBrainsMono Nerd Font-13" t t)
;; (setq-default line-spacing 8)

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
