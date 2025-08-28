;;; nano-dark.el --- Summary
;;; Commentary:
;; nano dark theme with custom face overrides

;;; Code:

(use-package nano-theme
  :straight (:type git :host github :repo "rougier/nano-theme"))

(load-theme 'nano-dark t)

;; CUSTOM FACES ;;

;;
;; UI
;;

(if (display-graphic-p)
    (set-face-attribute 'default nil
                        :background "#000000"
                        :foreground "#e0def4")
  (progn
    (set-face-attribute 'default nil
                        :background "undefined"
                        :foreground "#e0def4")))

(set-face-background 'cursor "#dddbe7")

(set-face-attribute 'window-divider nil
		    :distant-foreground (face-foreground 'default))

(set-face-background 'header-line "#191724")
(with-eval-after-load 'nano-modeline
  (set-face-background 'nano-modeline-face-buffer-read-write "#65617b")
  (set-face-foreground 'nano-modeline-face-secondary "#666680"))
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;;
;; NANO
;;

(set-face-foreground 'nano-faded "#65617b")
(set-face-foreground 'nano-salient "#b49bd7")
(set-face-background 'nano-subtle "#191724")

;;
;; EDITOR FACES
;;

(set-face-attribute 'font-lock-comment-face nil
		    :font "Victor Mono-14"
		    :weight 'normal
		    :italic t
		    :foreground "#65617b")
(set-face-attribute 'font-lock-string-face nil :inherit 'nano-popout)
(set-face-attribute 'isearch nil :background "#a0bbcb" :foreground "#000000")

;;
;; ORG FACES
;;

(with-eval-after-load 'org
  (set-face-background 'org-block "#17171e"))

(provide 'nano-dark)
;;; nano-dark.el ends here
