;; compile- + - one of these is killing my ctrl-o binding in global map.
(require 'compile-)
(require 'compile)
;;(require 'compile+)
;;(require 'compile-defuns)
(require 'cmake-mode)

(defun my-compilation-mode-hook ()
  (setq truncate-lines nil)
  (add-to-list 'compilation-finish-functions 'compilation-recenter-end-at-finish)
  )
                                        ; Don't truncate lines in the compilation window
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

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

(pushnew '("*compilation*"
           (minibuffer . nil)
           (unsplittable . t)
           (menu-bar-lines . 0))
         special-display-buffer-names)


(provide 'setup-compilation-mode)
;;(add-to-list 'compilation-finish-functions 'compilation-recenter-end-at-finish)
