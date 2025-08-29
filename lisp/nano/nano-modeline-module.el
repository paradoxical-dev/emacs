;;; package --- Summary
;;; Commentary:
;; Nano modeline and configuration

;;; Code:

(use-package nano-modeline
  :straight (:type git
                   :host github
                   :repo "rougier/nano-modeline"
                   :branch "rewrite")
  :config
  (defun nano-modeline--make-padded-icon (icon left right &optional face raise)
    "Return ICON as a string padded by LEFT and RIGHT pixels.
     ICON is a string (the glyph). LEFT / RIGHT are pixel widths.
     FACE is applied to icon and padding. RAISE is vertical raise for the icon."
    (let ((face (or face 'default))
          (left (or left 0))
          (right (or right 0)))
      (concat
       (when (> left 0)
         (propertize " " 'display `(space :width ,left) 'face face))
       (propertize icon 'face face 'display `(raise ,(or raise 0)))
       (when (> right 0)
         (propertize " " 'display `(space :width ,right) 'face face)))))

  (defun my/nano-modeline-element-current-time ()
    "Return the current time as a modeline element."
    (propertize (format-time-string "%H:%M")
                'face 'nano-modeline-face-secondary))

  ;; agenda format override ;;

  (if (display-graphic-p)
      (setf (alist-get 'buffer-agenda nano-modeline-symbol-list)
            (nano-modeline--make-padded-icon "" 0.25 0.75 'nano-modeline-face-buffer-read-only 0))
    ;; set to offcenter for termninal since no pixel paddin
    (setf (alist-get 'buffer-agenda nano-modeline-symbol-list)
          "  "))

  (defun my/nano-modeline-element-agenda-status ()
  "Agenda status"
  (nano-modeline-element-buffer-status (nano-modeline-symbol 'buffer-agenda)
                                       'nano-modeline-face-buffer-read-only))

  ;; override nano agenda format
  (setq nano-modeline-format-nano-agenda
    (cons '(my/nano-modeline-element-agenda-status
            nano-modeline-element-space
            nano-modeline-element-nano-agenda-date)
          '(nano-modeline-button-nano-agenda-prev-month
            nano-modeline-element-half-space
            nano-modeline-button-nano-agenda-prev-day
            nano-modeline-element-half-space
            nano-modeline-button-nano-agenda-today
            nano-modeline-element-half-space
            nano-modeline-button-nano-agenda-next-day
            nano-modeline-element-half-space
            nano-modeline-button-nano-agenda-next-month
            nano-modeline-element-half-space)))

  ;; define regular agenda mode format
  (defvar nano-modeline-format-org-agenda
    (cons '(my/nano-modeline-element-agenda-status
            nano-modeline-element-space
            "Agenda")
          '(my/nano-modeline-element-current-time
            nano-modeline-element-space)))

  :hook
  (text-mode             . nano-modeline)
  (messages-buffer-mode  . nano-modeline)
  (vterm-mode . (lambda ()
                  (nano-modeline
                   nano-modeline-format-terminal 'header)))
  (nano-agenda-mode      . (lambda ()
                             (nano-modeline
                              nano-modeline-format-nano-agenda 'header)))
  (org-agenda-mode       . (lambda ()
                             (nano-modeline
                              nano-modeline-format-org-agenda 'header)))
  (prog-mode             . nano-modeline))

(setq-default mode-line-format nil)

(provide 'nano-modeline-module)
;;; nano-modeline-module.el ends here
