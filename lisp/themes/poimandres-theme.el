;;; poimandres-theme.el --- Poimandres theme for Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;; Poimandres theme ported from Neovim (https://github.com/olivercederborg/poimandres.nvim).

;;; Code:

(deftheme poimandres
  "Poimandres theme ported from Neovim (https://github.com/olivercederborg/poimandres.nvim).")

;; Palette
(let ((colors '((yellow       . "#FFFAC2")
                (teal1        . "#5DE4C7")
                (teal2        . "#5FB3A1")
                (teal3        . "#42675A")
                (blue1        . "#89DDFF")
                (blue2        . "#ADD7FF")
                (blue3        . "#91B4D5")
                (blue4        . "#7390AA")
                (pink1        . "#FAE4FC")
                (pink2        . "#FCC5E9")
                (pink3        . "#D0679D")
                (blueGray1    . "#A6ACCD")
                (blueGray2    . "#767C9D")
                (blueGray3    . "#506477")
                (background1  . "#303340")
                (background2  . "#1B1E28")
                (background3  . "#171922")
                (text         . "#E4F0FB")
                (black        . "#000000")
                (white        . "#FFFFFF")
                (red          . "#cd5757")
                (none         . "unspecified"))))

  ;; Faces
  (custom-theme-set-faces
   'poimandres

   ;; BASE ;;

   `(default ((t (:foreground ,(cdr (assoc 'text colors))
                 :background ,(cdr (assoc 'background3 colors))))))
   `(cursor ((t (:background ,(cdr (assoc 'blueGray1 colors))))))
   `(region ((t (:background ,(cdr (assoc 'background1 colors))))))
   `(highlight ((t (:background ,(cdr (assoc 'background1 colors))))))
   `(minibuffer-prompt ((t (:foreground ,(cdr (assoc 'pink3 colors)) :weight bold))))
   `(link ((t (:foreground ,(cdr (assoc 'blue1 colors)) :underline t))))
   `(shadow ((t (:foreground ,(cdr (assoc 'blueGray2 colors))))))
   `(match ((t (:background ,(cdr (assoc 'pink2 colors)) :foreground ,(cdr (assoc 'black colors))))))
   `(show-paren-match ((t (:background ,(cdr (assoc 'pink2 colors)) :foreground ,(cdr (assoc 'black colors))))))

   ;; SYNTAX ;;

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,(cdr (assoc 'blue2 colors))))))
   `(font-lock-comment-face ((t (:foreground ,(cdr (assoc 'blueGray2 colors)) :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,(cdr (assoc 'text colors)) :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,(cdr (assoc 'teal1 colors)) :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,(cdr (assoc 'blue3 colors))))))
   `(font-lock-string-face ((t (:foreground ,(cdr (assoc 'teal2 colors))))))
   `(font-lock-type-face ((t (:foreground ,(cdr (assoc 'blueGray1 colors))))))
   `(font-lock-number-face ((t (:foreground ,(cdr (assoc 'teal1 colors))))))
   `(font-lock-variable-name-face ((t (:foreground ,(cdr (assoc 'text colors))))))
   `(font-lock-warning-face ((t (:foreground ,(cdr (assoc 'yellow colors)) :weight bold))))

   ;; lsp
   `(lsp-face-semhl-parameter ((t (:foreground ,(cdr (assoc 'pink3 colors))))))
   `(lsp-face-semhl-method ((t (:foreground ,(cdr (assoc 'teal1 colors)) :slant italic))))

   ;; MODE SPECIFIC ;;

   ;; magit
   `(magit-section-highlight ((t (:background ,(cdr (assoc 'background2 colors))))))
   `(magit-diff-file-heading-highlight ((t (:background ,(cdr (assoc 'background2 colors))))))
   `(magit-diff-hunk-heading ((t (:background ,(cdr (assoc 'background2 colors))))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,(cdr (assoc 'background1 colors))))))
   `(magit-diff-context-highlight ((t (:background ,(cdr (assoc 'background2 colors))))))
   `(magit-section-heading ((t (:foreground ,(cdr (assoc 'pink3 colors))))))
   `(magit-branch-remote ((t (:foreground ,(cdr (assoc 'pink1 colors))))))
   `(magit-log-author ((t (:foreground ,(cdr (assoc 'blue4 colors))))))

   `(magit-reflog-commit ((t (:foreground ,(cdr (assoc 'pink2 colors))))))
   `(magit-reflog-checkout ((t (:foreground ,(cdr (assoc 'pink2 colors))))))
   `(magit-reflog-merge ((t (:foreground ,(cdr (assoc 'teal1 colors))))))
   `(magit-reflog-amend ((t (:foreground ,(cdr (assoc 'blue3 colors))))))
   `(magit-reflog-rebase ((t (:foreground ,(cdr (assoc 'blue3 colors))))))
   `(magit-reflog-cherry-pick ((t (:foreground ,(cdr (assoc 'blue1 colors))))))
   `(magit-reflog-reset ((t (:foreground ,(cdr (assoc 'red colors))))))
   `(magit-reflog-other ((t (:foreground ,(cdr (assoc 'blueGray3 colors))))))
   `(magit-reflog-remote ((t (:foreground ,(cdr (assoc 'blueGray3 colors))))))

   ;; `(magit-diff-added ((t (:foreground "#94baad" :background "#32453e"))))
   `(magit-diff-added ((t (:foreground "#369681" :background "#1c2825"))))
   `(magit-diff-added-highlight ((t (:foreground "#369681" :background "#1c2825"))))
   `(magit-diff-removed ((t (:foreground "#963636" :background "#1e1717"))))
   `(magit-diff-removed-highlight ((t (:foreground "#963636" :background "#1e1717"))))
   `(magit-diff-base ((t (:foreground "#f1e98b" :background "#1e1d16"))))
   `(magit-diff-base-highlight ((t (:foreground "#f1e98b" :background "#1e1717"))))

   ;; diff & ediff
   `(ediff-odd-diff-A ((t (:background ,(cdr (assoc 'background2 colors))))))
   `(ediff-odd-diff-B ((t (:background ,(cdr (assoc 'background1 colors))))))
   `(ediff-odd-diff-C ((t (:background ,(cdr (assoc 'background2 colors))))))
   `(ediff-even-diff-A ((t (:background ,(cdr (assoc 'background1 colors))))))
   `(ediff-even-diff-B ((t (:background ,(cdr (assoc 'background2 colors))))))
   `(ediff-even-diff-C ((t (:background ,(cdr (assoc 'background1 colors))))))
   `(ediff-even-diff-C ((t (:background ,(cdr (assoc 'background1 colors))))))
   `(ediff-current-diff-A ((t (:background "#1e1717" :foreground "#963636"))))
   `(ediff-current-diff-B ((t (:background "#1c2825" :foreground "#369681"))))
   `(ediff-current-diff-C ((t (:background "#1e1d16" :foreground "#f1e98b"))))
   `(ediff-fine-diff-A ((t (:background "#673131" :foreground ,(cdr (assoc 'black colors))))))
   `(ediff-fine-diff-B ((t (:background ,(cdr (assoc 'teal2 colors)) :foreground ,(cdr (assoc 'black colors))))))

   `(diff-added ((t (:foreground "#369681" :background "#1c2825"))))
   `(diff-removed ((t (:foreground "#963636" :background "#1e1717"))))
   `(diff-changed ((t (:foreground "#f1e98b" :background "#1e1d16"))))

   ;; diff-hl
   `(diff-hl-insert ((t (:foreground "#2e5048" :background "#2e5048"))))
   `(diff-hl-delete ((t (:foreground "#472b2b" :background "#472b2b"))))
   `(diff-hl-change ((t (:foreground "#45412a" :background "#45412a"))))

   ;; corfu

   ;; flymake/flycheck
   `(flymake-error
     ((t (:underline (:style wave :color ,(cdr (assoc 'red colors)))))))
   `(flymake-warning
     ((t (:underline (:style wave :color ,(cdr (assoc 'yellow colors)))))))
   `(flymake-note
     ((t (:underline (:style wave :color ,(cdr (assoc 'blue1 colors)))))))

   `(flycheck-error
     ((t (:underline (:style wave :color ,(cdr (assoc 'red colors)))))))
   `(flycheck-warning
     ((t (:underline (:style wave :color ,(cdr (assoc 'yellow colors)))))))
   `(flycheck-info
     ((t (:underline (:style wave :color ,(cdr (assoc 'blue1 colors)))))))
   `(flycheck-fringe-error ((t (:foreground ,(cdr (assoc 'red colors))))))
   `(flycheck-fringe-warning ((t (:foreground ,(cdr (assoc 'yellow colors))))))
   `(flycheck-fringe-info ((t (:foreground ,(cdr (assoc 'blue1 colors))))))

   ;; flyover
   ;; `(flyover-error ((t (:foreground ,(cdr (assoc 'red colors))))))
   ;; `(flyover-warning ((t (:foreground ,(cdr (assoc 'yellow colors))))))
   ;; `(flyover-info ((t (:foreground ,(cdr (assoc 'blue1 colors))))))

   ;; corfu
   `(corfu-current ((t (:background ,(cdr (assoc 'background1 colors))))))
   `(corfu-default ((t (:background ,(cdr (assoc 'background2 colors))))))

   ;; orderless
   `(orderless-match-face-0 ((t (:foreground ,(cdr (assoc 'pink3 colors))))))
   `(orderless-match-face-0 ((t (:foreground ,(cdr (assoc 'teal1 colors))))))
   `(orderless-match-face-0 ((t (:foreground ,(cdr (assoc 'red colors))))))
   `(orderless-match-face-0 ((t (:foreground ,(cdr (assoc 'yellow colors))))))

   ;; TTY ;;

   `(ansi-color-black ((t
                        (:background ,(cdr (assoc 'background2 colors))
                                     :foreground ,(cdr (assoc 'background2 colors))))))
   `(ansi-color-red ((t
                        (:background ,(cdr (assoc 'pink3 colors))
                                     :foreground ,(cdr (assoc 'pink3 colors))))))
   `(ansi-color-green ((t
                        (:background ,(cdr (assoc 'teal2 colors))
                                     :foreground ,(cdr (assoc 'teal2 colors))))))
   `(ansi-color-yellow ((t
                        (:background ,(cdr (assoc 'yellow colors))
                                     :foreground ,(cdr (assoc 'yellow colors))))))
   `(ansi-color-blue ((t
                        (:background ,(cdr (assoc 'blue2 colors))
                                     :foreground ,(cdr (assoc 'blue2 colors))))))
   `(ansi-color-magenta ((t
                        (:background ,(cdr (assoc 'pink1 colors))
                                     :foreground ,(cdr (assoc 'pink1 colors))))))
   `(ansi-color-cyan ((t
                        (:background ,(cdr (assoc 'teal1 colors))
                                     :foreground ,(cdr (assoc 'teal1 colors))))))
   `(ansi-color-white ((t
                        (:background ,(cdr (assoc 'text colors))
                                     :foreground ,(cdr (assoc 'text colors))))))
   `(ansi-color-bright-black ((t
                        (:background ,(cdr (assoc 'background1 colors))
                                     :foreground ,(cdr (assoc 'background1 colors))))))
   `(ansi-color-bright-red ((t
                        (:background ,(cdr (assoc 'pink2 colors))
                                     :foreground ,(cdr (assoc 'pink2 colors))))))
   `(ansi-color-bright-green ((t
                        (:background ,(cdr (assoc 'teal3 colors))
                                     :foreground ,(cdr (assoc 'teal3 colors))))))
   `(ansi-color-bright-yellow ((t
                        (:background ,(cdr (assoc 'blue4 colors))
                                     :foreground ,(cdr (assoc 'blue4 colors))))))
   `(ansi-color-bright-blue ((t
                        (:background ,(cdr (assoc 'blue1 colors))
                                     :foreground ,(cdr (assoc 'blue1 colors))))))
   `(ansi-color-bright-magenta ((t
                        (:background ,(cdr (assoc 'blueGray1 colors))
                                     :foreground ,(cdr (assoc 'blueGray1 colors))))))
   `(ansi-color-bright-cyan ((t
                        (:background ,(cdr (assoc 'blueGray2 colors))
                                     :foreground ,(cdr (assoc 'blueGray2 colors))))))
   `(ansi-color-bright-white ((t
                        (:background ,(cdr (assoc 'white colors))
                                     :foreground ,(cdr (assoc 'white colors))))))
   ;; UI ELEMENTS ;;

   `(mode-line ((t (:foreground ,(cdr (assoc 'text colors))
                   :background ,(cdr (assoc 'background1 colors))))))
   `(mode-line-inactive ((t (:foreground ,(cdr (assoc 'blueGray2 colors))
                             :background ,(cdr (assoc 'background3 colors))))))
   `(vertical-border ((t (:foreground ,(cdr (assoc 'background1 colors))))))
   `(fringe ((t (:background ,(cdr (assoc 'background3 colors))))))
   `(line-number ((t (:foreground ,(cdr (assoc 'blueGray3 colors))))))
   `(line-number-current-line ((t (:foreground ,(cdr (assoc 'text colors)) :weight bold))))

   ;; Selection & Search
   `(isearch ((t (:background ,(cdr (assoc 'pink3 colors)) :foreground ,(cdr (assoc 'background2 colors)) :weight bold))))
   `(lazy-highlight ((t (:background ,(cdr (assoc 'blue4 colors)) :foreground ,(cdr (assoc 'background2 colors))))))

   ;; Whitespace
   `(whitespace-space ((t (:foreground ,(cdr (assoc 'blueGray3 colors))))))
   `(whitespace-tab ((t (:foreground ,(cdr (assoc 'blueGray3 colors))))))))

(provide-theme 'poimandres)
;;; poimandres-theme.el ends here
