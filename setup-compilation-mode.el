;; compile- + - one of these is killing my ctrl-o binding in global map.
(require 'compile-)
;;(require 'compile-20)
(require 'compile)
;;(require 'compile+20)
(require 'compile+)
(require 'compile-defuns)
(require 'cmake-mode)

(defun my-compilation-mode-hook ()
;;  (setq truncate-lines nil)
;;  (add-to-list 'compilation-finish-functions 'compilation-recenter-end-at-finish)
  )
                                        ; Don't truncate lines in the compilation window
;;(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(add-to-list 'compilation-error-regexp-alist '("^\\([^ :]+\\):\\([0-9]+\\): [^ ]" 1 2))
(add-to-list 'compilation-error-regexp-alist '("^ + [0-9]+>\\([^(]+\\)(\\([0-9]+\\)): [^ ]" 1 2))

(setq debug-on-error t)


(require 'compile-defuns)

(add-hook 'c-mode-hook 'my-c-mode-compile-dwim)
(add-hook 'c++-mode-hook 'my-c++-mode-compile-dwim)
(add-hook 'latex-mode-hook 'my-latex-mode-compile-dwim)

(add-hook 'compilation-finish-functions #'qtmstr-compile-finish)
(add-hook 'compilation-mode-hook #'qtmstr-setup-compile-mode)

(setq compilation-skip-threshold 1)
(setq compilation-skip-threshold 2)
;;(setq next-error-recenter (quote (4)))

(setq compilation-frame-spec '("*compilation*" (minibuffer . nil) (unsplittable . t) (menu-bar-lines . 0)))
(setq gr-dedicated-compilation-frame nil)

(if gr-on-24-3 (progn
                 (setq display-buffer-alist nil)
                 (if gr-dedicated-compilation-frame
                     (pushnew compilation-frame-spec display-buffer-alist)
                   (setq special-display-buffer-names (remove compilation-frame-spec display-buffer-alist))))
  (if gr-dedicated-compilation-frame
      (pushnew compilation-frame-spec special-display-buffer-names)
    (setq special-display-buffer-names (remove compilation-frame-spec special-display-buffer-names)))
  )

(setq fit-frame-max-width-percent 40)
(setq fit-frame-max-height-percent 75)
(provide 'setup-compilation-mode)
;;(add-to-list 'compilation-finish-functions 'compilation-recenter-end-at-finish)
