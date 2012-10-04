;; Load and initialize yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Develop in ~/emacs.d/snippets, but also
;; include snippets that come with yasnippet
(setq yas-root-directory `(,(expand-file-name "snippets" dotfiles-dir)
                           ,(expand-file-name "yasnippet/snippets" site-lisp-dir)))

(mapc 'yas-load-directory yas-root-directory)

;; Include snippets for Buster.js
                                        ;(require 'buster-snippets)

;; No dropdowns please, yas
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

(setq yas-wrap-around-region 'cua)

(defvar flymake-is-active-flag nil)

(defadvice yas-expand-snippet
  (before inhibit-flymake-syntax-checking-while-expanding-snippet activate)
  (setq flymake-is-active-flag
        (or flymake-is-active-flag
            (assoc-default 'flymake-mode (buffer-local-variables))))
  (when flymake-is-active-flag
    (flymake-mode-off)))

(add-hook 'yas-after-exit-snippet-hook
          '(lambda ()
             (when flymake-is-active-flag
               (flymake-mode-on))))

(provide 'setup-yasnippet)
