;; INFO: This module houses all editor specific packages and configs.
;; For instance, line highlights, mode lines, etc.

;; INDENT ;;

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0)

  (let ((bg (face-background 'default))
	(fg (face-foreground 'link)))
    (set-face-attribute 'highlight-indent-guides-top-character-face nil :background bg :foreground fg)
    (set-face-attribute 'highlight-indent-guides-character-face nil :background bg :foreground "#303030"))

  :hook (prog-mode . highlight-indent-guides-mode))

;; ZEN MODE ;;

(use-package darkroom
  :straight '(:type git :host github :repo "joaotavora/darkroom")
  :config
  (setq darkroom-text-scale-increase 0.8)
  :bind
  (:map evil-normal-state-map ("<leader>z" . darkroom-mode)))

;; TODO HIGHLIGHTS ;;

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-keyword-faces
	'(("TODO"    . "#a5eaf3")
	  ("FIXME"   . "#a5eaf3")
	  ("DEBUG"   . "#f16b66")
	  ("GOTCHA"  . "#fdac3b")
	  ("WARNING" . "#f4e66c")
	  ("INFO"    . "#75f3b3")
	  ("STUB"    . "#ab7deb")))
  :hook (prog-mode . hl-todo-mode))

;; MODE LINE ;;

;; INFO: custom var and functions to update gloabal directory

(defvar my/session-directory nil
  "Session-wide current working directory.")

;; Initialize on startup
(setq my/session-directory default-directory)

(defun my/update-session-directory (&rest _args)
  "Update `my/session-directory` to current `default-directory`."
  (setq my/session-directory default-directory))

;; Automatically update when calling cd
(advice-add 'cd :after #'my/update-session-directory)

(use-package doom-modeline
  :ensure t
  :config
  ;; CONFIG ;;

  (setq-default mode-line-format nil)

  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-lsp-icon t)
  (setq doom-modeline-vcs-icon t)
  (setq doom-modeline-modal-icon t)

  ;; FACE DEFINITIONS ;;

  ;; base ;;

  (set-face-background 'mode-line "undefined")
  (set-face-background 'mode-line-active "undefined")
  (set-face-background 'mode-line-inactive "undefined")

  ;; segments ;;

  (set-face-attribute 'doom-modeline-buffer-file nil :background "#2D3139" :foreground "#B5B5B8")
  (set-face-attribute 'doom-modeline-buffer-modified nil :background "#2D3139" :foreground "#B5B5B8")

  (set-face-attribute 'doom-modeline-vcs-default nil :background "undefined" :foreground "#8E8AA8" :bold t)

  ;; modes ;;

  ;; normal
  (let ((bg (face-foreground 'font-lock-type-face)))
    (set-face-attribute 'doom-modeline-evil-normal-state nil :background bg :foreground "#000000"))

  ;; insert
  (let ((bg (face-foreground 'button)))
    (set-face-attribute 'doom-modeline-evil-insert-state nil :background bg :foreground "#000000"))

  ;; visual
  (let ((bg (face-foreground 'font-lock-keyword-face)))
    (set-face-attribute 'doom-modeline-evil-visual-state nil :background bg :foreground "#000000"))

  ;; replace
  (let ((bg (face-foreground 'font-lock-function-name-face)))
    (set-face-attribute 'doom-modeline-evil-replace-state nil :background bg :foreground "#000000"))

  ;; operator
  (let ((bg (face-foreground 'font-lock-function-name-face)))
    (set-face-attribute 'doom-modeline-evil-operator-state nil :background bg :foreground "#000000"))
  
  ;; CUSTOM SEGMENTS ;;

  (defun my/left-modeline-left-sep (face)
    "Return a left powerline separator with FACE.
If FACE is nil, return empty string."
    (if face
	(propertize "" 'face face)
      ""))

  (defun my/right-modeline-right-sep (face)
    "Return a left powerline separator with FACE.
If FACE is nil, return empty string."
    (if face
	(propertize "" 'face face)
      ""))

  (defun my/right-modeline-left-sep (face)
    "Return a left powerline separator with FACE.
If FACE is nil, return empty string."
    (if face
	(propertize "" 'face face)
      ""))

  (defun my/left-modeline-right-sep (face)
    "Return a right powerline separator with FACE.
If FACE is nil, return empty string."
    (if face
	(propertize " " 'face face)
      ""))

  (doom-modeline-def-segment modals
    "Custom segment for evil modes"
    (when doom-modeline-modal
      (when (bound-and-true-p evil-local-mode)
	(let* ((state (cond
		       ((evil-normal-state-p) 'normal)
		       ((evil-insert-state-p) 'insert)
		       ((evil-visual-state-p) 'visual)
		       ((evil-motion-state-p) 'motion)
		       ((evil-operator-state-p) 'operator)
		       ((evil-replace-state-p) 'replace)
		       (t 'user)))
	       (face (intern (format "doom-modeline-evil-%s-state"
				     (symbol-name state))))
	       ;; custom faces for following icons
	       (inv-face `(:foreground ,(face-attribute face :background nil 'default)
				       :background "#42464e"))
	       (fill-face `(:foreground "#2d3139" :background "#42464e"))
	       (icon (propertize " 󰕷 " 'face face))
	       ;; label text for mode
	       (state-name (propertize (concat (capitalize (symbol-name state)) " ") 'face face))
	       ;; icon fillers
	       (right-edge-sep (my/left-modeline-left-sep inv-face))
	       (right-edge-fill (my/left-modeline-right-sep fill-face)))
	  (concat icon state-name right-edge-sep right-edge-fill)))))

(doom-modeline-def-segment my-buffer-info
  "Show file icon, dot if modified, then base file name."
  (let* ((file (or buffer-file-name (buffer-name)))
         ;; get a nerd icon for this file (with coloring)
         (file-icon (if buffer-file-name
                        (nerd-icons-icon-for-file file)
                      (concat (nerd-icons-icon-for-mode major-mode) " ")))

         ;; add background without changing foreground
         (_ (add-face-text-property 0 (length file-icon)
                                    `(:background "#2d3139")
                                    nil file-icon))

         ;; dot icon if modified
         (dot-icon (when (and buffer-file-name (buffer-modified-p))
                     (propertize "  " 'face 'doom-modeline-buffer-modified)))

	 ;; fillers and icons
	 (filler (propertize "█" 'face `(:foreground "#2d3139" :background "#000000")))
	 (left-side-end-icon (my/left-modeline-left-sep `(:foreground "#2d3139" :background "#000000")))

         ;; just base name, not path
         (file-name (propertize
                     (concat (file-name-nondirectory file) " ")
                     'face `(:inherit ,(doom-modeline-face
                                        (if (and doom-modeline-highlight-modified-buffer-name
                                                 (buffer-modified-p))
                                            'doom-modeline-buffer-modified
                                          'doom-modeline-buffer-file))
                                     :foreground "#B5B5B8"
                                     :background "#2D3139")
                     'mouse-face 'doom-modeline-highlight
                     'help-echo "Buffer name
mouse-1: Previous buffer
mouse-3: Next buffer"
                     'local-map mode-line-buffer-identification-keymap)))
    (concat
     filler
     file-icon
     filler
     file-name
     (when dot-icon (concat dot-icon))
     left-side-end-icon)))

(doom-modeline-def-segment my-buffer-percentage
  "Show buffer percentage styled like modals."
  (let* ((state (cond
                 ((and (bound-and-true-p evil-local-mode) (evil-insert-state-p))   'insert)
                 ((and (bound-and-true-p evil-local-mode) (evil-visual-state-p))   'visual)
                 ((and (bound-and-true-p evil-local-mode) (evil-motion-state-p))   'motion)
                 ((and (bound-and-true-p evil-local-mode) (evil-operator-state-p)) 'operator)
                 ((and (bound-and-true-p evil-local-mode) (evil-replace-state-p))  'replace)
                 (t 'normal)))
         (face (intern (format "doom-modeline-evil-%s-state" (symbol-name state))))
         (inv-face `(:background ,(face-attribute face :background nil 'default)
                                 :foreground "#42464e"))
         (fill-face `(:foreground "#2d3139" :background "#42464e"))
         (icon (propertize "   " 'face face))
         (raw-perc (format-mode-line "%p"))
         ;; append extra space only if numeric
         (perc (if (string-match-p "^[0-9]+%" raw-perc)
                   (concat (format " %s" raw-perc) " ")
                 raw-perc))
	 (perc (concat perc "  "))
         (perc-str (propertize perc 'face face))
         (right-edge-sep (my/right-modeline-right-sep inv-face))
         (right-edge-fill (my/left-modeline-right-sep fill-face)))
    (concat right-edge-sep icon perc-str)))


(doom-modeline-def-segment my-session-directory
  "Show base name of session directory."
  (let* ((dir (file-name-nondirectory
               (directory-file-name my/session-directory))) ; strip trailing slash
         (icon (propertize "    " 'face 'doom-modeline-buffer-file))
	 (dir (concat dir " "))
         (dir-str (propertize dir 'face 'doom-modeline-buffer-file))

	 (right-fill-face `(:foreground "#2d3139" :background "#42464e"))
	 (right-fill (my/right-modeline-right-sep right-fill-face))
	 (filler (propertize "█" 'face `(:foreground "#42464e" :background "#000000")))
	 (left-edge (my/right-modeline-left-sep `(:foreground "#2d3139" :background "unspecified"))))
    (concat left-edge icon dir-str right-fill filler)))

  ;; IMPLEMENTATION ;;

  (doom-modeline-def-modeline 'main
    '(modals my-buffer-info vcs remote-host)
    '(lsp check my-session-directory my-buffer-percentage))
  :hook
  (after-init . doom-modeline-mode)
  (doom-modeline-mode . (lambda () (doom-modeline-set-modeline 'main 'default))))

(provide 'editor-module)
