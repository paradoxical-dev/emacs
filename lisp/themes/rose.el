;; INFO: Customized Rose-Pine theme

(use-package autothemer
  :ensure t)

(straight-use-package
 '(pinerose-emacs
   :host github
   :repo "konrad1977/pinerose-emacs"))

(load-theme 'rose-pine t)

;; CUSTOM COLORS ;;

;; background
(if (display-graphic-p)
    (set-face-attribute 'default nil
                        :background "#000000"
                        :foreground "#e0def4")
  (progn
    (set-face-attribute 'default nil
                        :background "undefined"
                        :foreground "#e0def4")))

;; vertico
(unless (display-graphic-p)
  (with-eval-after-load 'vertico
	(set-face-foreground 'vertico-current "#e0def4")))

(provide 'rose)
