;; Seed the random-number generator
(random t)

;; Whitespace-style
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

;; Trailing white-space. Just say no.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Misc in misc
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t)

(setq auto-save-default t)              ; Yes auto save good
(setq auto-save-interval 1000)         ; Number of input chars between auto-saves
(setq auto-save-timeout 3000)      ; Number of seconds idle time before auto-save
(setq backup-by-copying t)              ; don't clobber symlinks
                                        ;(setq backup-directory-alist '(("." . "~/.backups"))) ; don't litter my fs tree
(setq delete-old-versions t)            ; clean up a little
(setq kept-new-versions 6)              ; keep 6 new
(setq kept-old-versions 2)              ; keep only 2 old

(auto-compression-mode 1)
(setq case-fold-search t)
(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-distinction nil)

(setq require-final-newline t)

(require 'wrap-region)
(wrap-region-global-mode t)

(provide 'misc)
