
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(add-log-current-defun-function (quote ignore))
 '(c-require-final-newline
   (quote
    ((c-mode . t)
     (c++-mode . t)
     (objc-mode . t)
     (java-mode))))
 '(compilation-ask-about-save nil)
 '(compilation-error-regexp-alist (quote (gcc-include gnu)))
 '(compilation-skip-threshold 2)
 '(fill-column 80)
 '(frame-background-mode (quote dark))
 '(global-auto-revert-ignore-modes (quote ((quote c++-mode))))
 '(gud-gdb-command-name "~/bin/egdbHgCompose --fsm-compose 0")
 '(ido-use-filename-at-point nil)
 '(inferior-octave-prompt
   "\\(^octave\\(\\|.bin\\|.exe\\)?\\(-[.0-9]+\\)?\\(:[0-9]+\\)?\\|^debug\\|^\\)>+ ")
 '(isearchp-initiate-edit-commands (quote (backward-char)))
 '(js2-allow-rhino-new-expr-initializer nil)
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-enter-indents-newline t)
 '(js2-global-externs
   (quote
    ("module" "require" "jQuery" "$" "_" "buster" "sinon" "ZOMBIE")))
 '(js2-idle-timer-delay 0.5)
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode nil)
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(kill-whole-line t)
 '(line-move-visual t)
 '(magit-diff-expansion-threshold 3.0)
 '(magit-diff-refine-hunk (quote all))
 '(make-modes
   (quote
    (make-mode makefile-mode makefile-gmake-mode jam-mode fundamental-mode conf-mode conf-unix-mode change-log-mode)) t)
 '(next-error-highlight t)
 '(next-error-highlight-no-select (quote fringe-arrow))
 '(next-error-recenter (quote (4)))
 '(ns-use-native-fullscreen nil)
 '(recentf-max-menu-items 30)
 '(recentf-max-saved-items 300)
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (indent-tabs-mode . 1)
     (encoding . utf-8))))
 '(sourcepair-header-extensions (quote (".h" ".hpp" ".hh" ".in" ".derivs")))
 '(sourcepair-source-extensions
   (quote
    (".ipp" ".cpp" ".cxx" ".cc" ".C" ".c" ".out" ".log")))
 '(text-mode-hook
   (quote
    (er/add-text-mode-expansions
     (lambda nil
       (set-fill-column 70))
     text-mode-hook-identify)))
 '(xterm-extra-capabilities nil)
 '(yas-also-auto-indent-first-line t)
 '(yas/also-auto-indent-first-line t)
 '(yas/next-field-key (quote ("TAB" "<tab>" "M-<return>")))
 '(yas/prev-field-key (quote ("<backtab>" "<S-tab>" "C-<space>" "S-<return>"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-fine-diff-B ((t (:background "dark olive green"))))
 '(error ((t (:foreground "#dc322f"))))
 '(slime-error-face ((t (:foreground "#dc322f")))))
