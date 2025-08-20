;; INFO: This module contains the functions and styling for the modeline
;; it also uses the `common-header-mode-line` package

;; WARNING: This package needs to be manually installed. For more info see
;; https://github.com/Bad-ptr/common-header-mode-line.el?tab=readme-ov-file#installation

;; FACES ;

;; load doom for builtin faces
(use-package doom-modeline
  :ensure t)

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

;; CUSTOM SEGMENTS ;

(defun my/left-modeline-left-sep (face)
  (if face (propertize "" 'face face) ""))

(defun my/right-modeline-right-sep (face)
  (if face (propertize "" 'face face) ""))

(defun my/right-modeline-left-sep (face)
  (if face (propertize "" 'face face) ""))

(defun my/left-modeline-right-sep (face)
  (if face (propertize " " 'face face) ""))

;;
;; modal segment
;;

(defun my/mode-segment ()
  "Return Evil state indicator like in Doom."
  (when (bound-and-true-p evil-local-mode)
    (let* ((state (cond
                   ((evil-normal-state-p) 'normal)
                   ((evil-insert-state-p) 'insert)
                   ((evil-visual-state-p) 'visual)
                   ((evil-motion-state-p) 'motion)
                   ((evil-operator-state-p) 'operator)
                   ((evil-replace-state-p) 'replace)
                   (t 'user)))
           (face (intern (format "doom-modeline-evil-%s-state" (symbol-name state))))
           (icon (propertize " 󰕷 " 'face face))
           (state-name (propertize (concat (capitalize (symbol-name state)) " ") 'face face))
           (inv-face `(:foreground ,(face-attribute face :background nil 'default)
                                   :background "#42464e"))
           (fill-face `(:foreground "#2d3139" :background "#42464e"))
           (right-edge-sep (my/left-modeline-left-sep inv-face))
           (right-edge-fill (my/left-modeline-right-sep fill-face)))
      (concat icon state-name right-edge-sep right-edge-fill))))

;;
;; buffer-info
;;

(defun my/buffer-info-segment ()
  "Show file icon, dot if modified, and base file name."
  (let* ((file (or buffer-file-name (buffer-name)))
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

         (filler (propertize "█" 'face `(:foreground "#2d3139" :background "#000000")))
         (left-side-end-icon (my/left-modeline-left-sep `(:foreground "#2d3139" :background "#000000")))

         (file-name (propertize
                     (concat (file-name-nondirectory file) " ")
                     'face `(:inherit ,(if (and (boundp 'doom-modeline-highlight-modified-buffer-name)
                                                doom-modeline-highlight-modified-buffer-name)
                                           'doom-modeline-buffer-modified
                                         'doom-modeline-buffer-file)
                                     :foreground "#B5B5B8"
                                     :background "#2D3139")
                     'mouse-face 'mode-line-highlight
                     'help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                     'local-map mode-line-buffer-identification-keymap)))
    (concat filler file-icon filler file-name (when dot-icon (concat dot-icon)) left-side-end-icon)))


;;
;; cursor position
;;

(defun my/buffer-percentage-segment ()
  "Show buffer percentage styled like Doom modals."
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
	 (clean-perc (string-trim-left raw-perc))
         (perc (if (string-match-p "^[0-9]+%" clean-perc)
                   (concat " " raw-perc " ")
                 raw-perc))
         (perc (concat perc "  "))
         (perc-str (propertize perc 'face face))
         (right-edge-sep (my/right-modeline-right-sep inv-face))
         (right-edge-fill (my/left-modeline-right-sep fill-face)))
    (concat right-edge-sep icon perc-str)))

;;
;; directory
;;

;; INFO: custom var and functions to update gloabal directory

(defvar my/session-directory nil
  "Session-wide current working directory.")

;; initialize on startup
(setq my/session-directory default-directory)

(defun my/update-session-directory (&rest _args)
  "Update `my/session-directory` to current `default-directory`."
  (setq my/session-directory default-directory))

;; automatically update when calling cd
(advice-add 'cd :after #'my/update-session-directory)

(defun my/session-directory-segment ()
  "Show base name of session directory."
  (let* ((dir (if (boundp 'my/session-directory)
                  (file-name-nondirectory
                   (directory-file-name my/session-directory))
                ""))
         (icon (propertize "    " 'face 'doom-modeline-buffer-file))
         (dir (concat dir " "))
         (dir-str (propertize dir 'face 'doom-modeline-buffer-file))
         (right-fill-face `(:foreground "#2d3139" :background "#42464e"))
         (right-fill (my/right-modeline-right-sep right-fill-face))
         (filler (propertize "█" 'face `(:foreground "#42464e" :background "#000000")))
         (left-edge (my/right-modeline-left-sep `(:foreground "#2d3139" :background "unspecified"))))
    (concat left-edge icon dir-str right-fill filler)))

;;
;; vcs
;;

(defun my/git-branch ()
  "Return current Git branch with icon, or empty string if not in Git."
  (let ((branch (and vc-mode
                     (cadr (split-string (string-trim vc-mode) "^[A-Z]+[-:]+")))))
    (if branch
        (concat " " branch)
      "")))

;;
;; check
;;

(defun my/diagnostics-segment ()
  "Return a string representing Flymake diagnostics for the current buffer."
  (when (and (bound-and-true-p flymake-mode)
             (bound-and-true-p flymake--state))
    ;; Count errors/warnings/notes
    (let ((note 0) (warning 0) (error 0)
          (warning-level (warning-numeric-level :warning))
          (note-level (warning-numeric-level :debug)))
      (maphash
       (lambda (_backend state)
         (dolist (diag (flymake--state-diags state))
           (let ((sev (flymake--lookup-type-property
                       (flymake--diag-type diag)
                       'severity
                       (warning-numeric-level :error))))
             (cond ((> sev warning-level) (cl-incf error))
                   ((> sev note-level) (cl-incf warning))
                   (t (cl-incf note))))))
       flymake--state)
      ;; Build modeline string
      (let ((vsep " ")
	    (warn-fg (face-foreground 'modeline-flymake-warning))
	    (note-fg (face-foreground 'modeline-flymake-note))
	    (error-fg (face-foreground 'modeline-flymake-error)))
        (concat
         (when (> error 0)
           (propertize (format " %d" error) 'face `(:foreground ,error-fg)))
         (when (> warning 0)
           (propertize (format "  %d" warning) 'face `(:foreground ,warn-fg)))
         (when (> note 0)
           (propertize (format "  %d" note) 'face `(:foreground ,note-fg))))))))

;;
;; lsp
;;

(defun my/lsp-segment ()
  "Return a string representing the active LSP server for the current buffer."
  (when (and (bound-and-true-p lsp-mode)
             (lsp-workspaces))
    (let ((server-names (mapcar (lambda (ws)
                                  (symbol-name (lsp--workspace-server-id ws)))
                                (lsp-workspaces))))
      (concat
       "  [ "
       (mapconcat 'identity server-names ", ") " ]  " ))))

;; IMPLEMENTATION ;;

;; INFO: This is where common-header-mode-line is brought in.
;; It will unify the mode line to a single global one

;; disable normal mode-line-format so common can manage it
(setq-default mode-line-format nil)

(defun my/common-header-mode-line-string ()
  "Build the per-frame global mode line string with custom segments."
  (format-mode-line
   `("%e"
     (:eval (my/mode-segment))
     (:eval (my/buffer-info-segment))
     "  "
     (:eval (my/git-branch))
     "  "
     (:eval (my/diagnostics-segment))

     ;; spacer to push right-hand segments to the edge
     (:eval
      (propertize " " 'display
                  `((space :align-to (- (+ right right-fringe right-margin)
                                        ,(string-width
                                          (concat
                                           ;; (my/vcs-segment)
                                           (my/session-directory-segment)
					   (my/lsp-segment)
                                           (my/buffer-percentage-segment))))))))

     ;; (:eval (my/vcs-segment))
     (:eval (my/lsp-segment))
     (:eval (my/session-directory-segment))
     (:eval (my/buffer-percentage-segment)))))

;; activate common-header-mode-line using custom string generator
(with-eval-after-load "common-header-mode-line-autoloads"
  (setopt common-header-mode-line-update-delay 0.01)
  (add-hook 'window-setup-hook
            (lambda ()
              (common-mode-line-mode 1)
	      (per-window-mode-line-mode -1)
	      (set-face-background 'mode-line "undefined")
	      (set-face-background 'mode-line-active "undefined")
	      (set-face-background 'mode-line-inactive "undefined")
              ;; inject custom per-frame update function
              (setq per-frame-mode-line-update-display-function
                    (lambda (display)
                      (let ((buf (cdr (assq 'buf display))))
                        (with-current-buffer buf
                          (setq-local buffer-read-only nil
                                      tab-width 8)
                          ;; generate custom mode-line string
                          (let ((mode-l-str (my/common-header-mode-line-string)))
                            (erase-buffer)
                            (insert mode-l-str)
                            (goto-char (point-min))
                            (setq-local mode-line-format nil
                                        header-line-format nil
                                        buffer-read-only t)))))))))

(provide 'custom-modeline)
