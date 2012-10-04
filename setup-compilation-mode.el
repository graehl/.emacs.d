;; compile- + - one of these is killing my ctrl-o binding in global map.
(require 'compile-)
(require 'compile)
;;(require 'compile+)
(require 'compile-defuns)
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

(defun my-recompile ()
  "Run recompilation but put the point at the *end* of the buffer
so we can watch errors as they come up"
  (interactive)
  (if (and (my-buffer-exists "*compilation*")
           compile-command)
      (save-excursion
        ;; switching to the compilation buffer here causes the compile command to be
        ;; executed from the same directory it originated from.
        (pop-to-buffer "*compilation*")
        (recompile)
        (pop-to-buffer "*compilation*")
        (bufend)
        (other-window 1)
        )
    ;; else
    (call-interactively 'my-compile))
  ;; force scrolling despite save-excursion
  (my-end-of-current-compilation-buffer))

(provide 'setup-compilation-mode)
(setq compilation-skip-threshold 1)
(setq compilation-skip-threshold 2)
;;(setq next-error-recenter (quote (4)))
