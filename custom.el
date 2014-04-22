
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(add-log-current-defun-function (quote ignore))
 '(c-require-final-newline (quote ((c-mode . t) (c++-mode . t) (objc-mode . t) (java-mode))))
 '(compilation-ask-about-save nil)
 '(compilation-error-regexp-alist (quote (("^ + [0-9]+>\\([^(>]+\\)(\\([0-9]+\\)): [^ ]" 1 2) ("^\\([^ :]+\\):\\([0-9]+\\): [^ ]" 1 2) absoft ada aix ant bash borland python-tracebacks-and-caml comma cucumber edg-1 edg-2 epc ftnchek iar ibm irix java jikes-file maven jikes-line gcc-include ruby-Test::Unit gnu lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint)))
 '(compilation-skip-threshold 2)
 '(fill-column 80)
 '(global-auto-revert-ignore-modes (quote ((quote c++-mode))))
 '(gud-gdb-command-name "~/bin/egdbHgCompose --fsm-compose 0")
 '(ido-use-filename-at-point nil)
 '(inferior-octave-prompt "\\(^octave\\(\\|.bin\\|.exe\\)?\\(-[.0-9]+\\)?\\(:[0-9]+\\)?\\|^debug\\|^\\)>+ ")
 '(isearchp-initiate-edit-commands (quote (backward-char)))
 '(js2-allow-rhino-new-expr-initializer nil)
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-enter-indents-newline t)
 '(js2-global-externs (quote ("module" "require" "jQuery" "$" "_" "buster" "sinon" "ZOMBIE")))
 '(js2-idle-timer-delay 0.5)
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode nil)
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(kill-whole-line t)
 '(line-move-visual t)
 '(make-modes (quote (make-mode makefile-mode makefile-gmake-mode jam-mode fundamental-mode conf-mode conf-unix-mode change-log-mode)))
 '(next-error-highlight t)
 '(next-error-highlight-no-select (quote fringe-arrow))
 '(next-error-recenter (quote (4)))
 '(ns-use-native-fullscreen nil t)
 '(recentf-max-menu-items 30)
 '(recentf-max-saved-items 300)
 '(safe-local-variable-values (quote ((eval when (fboundp (quote rainbow-mode)) (rainbow-mode 1)) (indent-tabs-mode . 1) (encoding . utf-8))))
 '(text-mode-hook (quote (er/add-text-mode-expansions (lambda nil (set-fill-column 70)) text-mode-hook-identify)))
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
 )
