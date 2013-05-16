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
        (flet ((yes-or-no-p (&rest args) t)
               (y-or-n-p (&rest args) t))
          (recompile))
        ;;(font-lock-mode -1)
        ;;(setq truncate-lines t)
        ;;(toggle-word-wrap -1)
        (toggle-word-wrap 1)
        (pop-to-buffer "*compilation*")
        (bufend)
        (other-window 1)
        )
    ;; else
    (call-interactively 'my-compile))
  ;; force scrolling despite save-excursion

  ;; testing turning this off:
  (my-end-of-current-compilation-buffer)
  )

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

(defun my-yes-or-mumble-p (prompt)
  "PROMPT user with a yes-or-no question, but only test for yes."
  (if (string= "yes"
               (downcase
                (read-from-minibuffer
                 (concat prompt "(yes or no) "))))
      t
    nil))

;;(defalias 'yes-or-no-p 'my-yes-or-mumble-p)
;; doesn't work for compile mode


(provide 'setup-compilation-mode)
;;(add-to-list 'compilation-finish-functions 'compilation-recenter-end-at-finish)
