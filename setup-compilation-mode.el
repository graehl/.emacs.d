;; compile- + - one of these is killing my ctrl-o binding in global map.
(require 'compile-)
(require 'compile)
(require 'compile+)
(require 'compile-defuns)
(require 'cmake-mode)

(defun my-compilation-mode-hook ()
  ;;  (setq truncate-lines nil)
  ;;  (add-to-list 'compilation-finish-functions 'compilation-recenter-end-at-finish)
  ;; (global-set-key (kbd "C-o") 'other-window)
  (define-key compilation-mode-map (kbd "C-o") 'other-window)
  (setq bidi-paragraph-direction 'left-to-right)
  )
                                        ; Don't truncate lines in the compilation window
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(add-to-list 'compilation-error-regexp-alist '("^\\([^ :]+\\):\\([0-9]+\\): [^ ]" 1 2))
(add-to-list 'compilation-error-regexp-alist '("^ + [0-9]+>\\([^(]+\\)(\\([0-9]+\\)): [^ ]" 1 2))


(require 'compile-defuns)

(add-hook 'c-mode-hook 'my-c-mode-compile-dwim)
(add-hook 'c++-mode-hook 'my-c++-mode-compile-dwim)
(add-hook 'latex-mode-hook 'my-latex-mode-compile-dwim)

(add-hook 'compilation-finish-functions #'qtmstr-compile-finish)
(add-hook 'compilation-mode-hook #'qtmstr-setup-compile-mode)
 ;; i don't like this - it forces a dedicated frame
(remove-hook 'compilation-finish-functions 'fit-1-window-frames-on)

(setq compilation-skip-threshold 1)
;;(setq compilation-skip-threshold 2)
;;(setq next-error-recenter (quote (4)))

(setq gr-dedicated-compilation-frame nil)
(setq compilation-frame-spec '("*compilation*" (minibuffer . nil) (unsplittable . t) (menu-bar-lines . 0)))

(setq fit-frame-max-width-percent 40)
(setq fit-frame-max-height-percent 75)
(if gr-on-24-3 (progn
                 (setq display-buffer-alist nil)
                 (if gr-dedicated-compilation-frame
                     (pushnew compilation-frame-spec display-buffer-alist)
                   (setq special-display-buffer-names (remove compilation-frame-spec display-buffer-alist))))
  (if gr-dedicated-compilation-frame
      (pushnew compilation-frame-spec special-display-buffer-names)
    (setq special-display-buffer-names (remove compilation-frame-spec special-display-buffer-names)))
  )

(defun growl (title message)
  (when gr-on-mac
    (start-process "growl" " growl" "growlnotify" title "-a" "Emacs")
    (process-send-string " growl" message)
    (process-send-string " growl" "\n")
    (process-send-eof " growl")))

(defun growl-compilation-result(buffer msg)
  (when (string-match "^*com" (buffer-name buffer))
    (if (string-match "^finished" msg)
        (progn
          (growl "Emacs compilation" "Compilation Successful :-)"))
      (growl "Emacs compilation" "Compilation Failed :-("))))

(add-hook 'compilation-finish-functions 'growl-compilation-result)

(provide 'setup-compilation-mode)
;;(add-to-list 'compilation-finish-functions 'compilation-recenter-end-at-finish)
