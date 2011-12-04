(require 'solarized-definitions)
; emacs-24 enable-theme does not work;
;(add-to-list 'custom-theme-load-path solarized-path)
(setq solarized-path (concat (expand-file-name "emacs-color-theme-solarized" site-lisp-dir)))
(require 'color-theme-solarized)
(color-theme-solarized-dark)
;(color-theme-solarized-light)

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

(global-hl-line-mode 1)
(set-face-background 'region "#222222")
;; Highlight in yasnippet
(set-face-background 'yas/field-highlight-face "#333399")

;; org-mode colors
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMPEDED" . (:foreground "red" :weight bold))
        ))

(when nil
  (set-face-background 'region "#464740")
  ;; Highlight current line
  ;; Customize background color of lighlighted line
  (set-face-background 'hl-line "#222222")
  ;; Subtler highlight in magit
  (set-face-background 'magit-item-highlight "#121212")
  (set-face-foreground 'magit-diff-none "#666666")
  (set-face-foreground 'magit-diff-add "#00cc33")
  )

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; No menu bars
(menu-bar-mode -1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (turn-off-tool-bar)
  (tooltip-mode -1)
  (turn-off-tool-bar)
  (blink-cursor-mode -1))

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;; Ditch them scrollbars
(scroll-bar-mode -1)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)

(remove-dos-eol)

(provide 'appearance)
