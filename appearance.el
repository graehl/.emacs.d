;; see http://codefork.com/blog/index.php/2011/11/27/getting-the-solarized-theme-to-work-in-emacs/ for instructions
;; (require 'solarized-dark-theme)
(setq default-tab-width 2)

;; emacs-24 enable-theme does not work;
;;(add-to-list 'custom-theme-load-path solarized-path)
(setq solarized-path (concat (expand-file-name "emacs-color-theme-solarized" site-lisp-dir)))
(require 'color-theme-solarized)
(color-theme-solarized-dark)
;;(color-theme-solarized-light)

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

(defun cursor-color (color) "set color even for new frames"
(interactive)
(add-hook 'window-setup-hook '(lambda () (set-cursor-color color)))
(add-hook 'after-make-frame-functions '(lambda (f) (with-selected-frame f (set-cursor-color color)))))
;(cursor-color "red") ; doesn't work with hl-line or what?

(require 'hl-line+)
;;(global-hl-line-mode t)
(toggle-hl-line-when-idle)
(set-face-background 'hl-line "#111144")
(set-face-background 'cursor "#111144") ; magic value same as hl-line makes grey? cool
(set-face-background 'cursor "#777777")

;;(set-face-background 'region "#222222")

;; Highlight in yasnippet
(ignore-errors (set-face-background 'yas-field-highlight-face "#333399"))

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
  (safe-wrap (turn-off-tool-bar))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;; Ditch them scrollbars
(when (fboundp 'scroll-bar-mode)
(scroll-bar-mode -1)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)

(require 'fit-frame)
(setq split-height-threshold nil)
(setq split-width-threshold nil)
)
(provide 'appearance)
()
(remove-dos-eol)
(require 'avoid)
(require 'misc-fns)

(setq-default
 sgml-quick-keys t
 sgml-validate-command "tidy"
 teach-extended-commands-p t
 truncate-partial-width-windows nil

                                        ; mode-line-buffer-identification '("%12b [%f]")
 indent-tabs-mode nil
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally
 )

(global-auto-revert-mode 1)
(global-auto-revert-mode -1)
(transient-mark-mode 1)

;; Enable these two supposedly "advanced" commands which come disabled by default.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(delete-other-windows)
(split-window-horizontally)
(when (require 'rainbow-delimiters nil 'noerror)
  (install-hook 'coding-hooks 'rainbow-delimiters-mode))



(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 10 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    ; (message "Buffer is set to read-only because it is large.  Undo also disabled.")
    ))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)
