(provide 'key-bindings)
;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand)

;; Expand region (increases selected region by semantic units)
;;(global-set-key (kbd "C-@") 'er/expand-region)

;; Mark additional regions matching current region
(global-set-key (kbd "C-å") 'mark-previous-like-this)
(global-set-key (kbd "C-æ") 'mark-next-like-this)

;; Replace rectangle-text with inline-string-rectangle
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; Repeat last command - too cumbersome with C-x z
(global-set-key (kbd "M-z") 'repeat)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'gr-cleanup-buffer)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Use shell-like backspace C-h, rebind help to F1
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;; Killing text
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)

;; Delete region (don't put it in the kill-ring)
(global-set-key (kbd "C-c C-w") 'delete-region)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "M-W") '(lambda () (interactive) (save-region-or-current-line 1)))

;; Make zap-to-char more convenient, and suspend-frame less
(global-set-key (kbd "C-z") 'zap-to-char)
(global-set-key (kbd "C-x C-z") 'suspend-frame)

;; Remap old M-m to M-i (better mnemonic for back-to-indentation)
;; We lose tab-to-tab-stop, which is no big loss in my use cases.
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Font size
(global-set-key (kbd "M-s +") 'zoom-in)
(global-set-key (kbd "M-s -") 'zoom-out)

;; Create new frame (bound to regular mac-command)
(global-set-key (kbd "M-n") 'make-frame-command)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Edit file with sudo
(global-set-key (kbd "M-s e") 'sudo-edit)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
(global-set-key (kbd "C-x -") 'rotate-windows)

;; Indentation help
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x h") 'view-url)


;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; Navigation bindings
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)

;; Convenience on ThinkPad Keyboard: Use back/forward as pg up/down
(global-set-key (kbd "<XF86Back>") 'scroll-down)
(global-set-key (kbd "<XF86Forward>") 'scroll-up)
(global-set-key (kbd "<XF86WakeUp>") 'beginning-of-buffer)

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)

;; Comment out block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Uncomment block
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Indent region
(global-set-key (kbd "C-c M-i") 'indent-region)

;; Eval buffer
(global-set-key (kbd "C-c v") 'eval-buffer)

;; Create scratch buffer
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Move windows, even in org-mode
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

;; Recall position
(global-set-key (kbd "C-c C-s") 'toggle-buffer-pos)

;; Mark all
(global-set-key (kbd "C-c a") 'mark-whole-buffer)

;; Eval lines
(global-set-key (kbd "C-c C-e") 'eval-current-line)
(global-set-key (kbd "C-c M-e") 'eval-output-marked-lines)

;; Magit l=f limit to files. enter file. l view log
(global-set-key (kbd "C-x m") 'magit-status)

;; Clever newlines
(global-set-key (kbd "<C-return>") 'new-line-below)
(global-set-key (kbd "<C-M-up>") 'new-line-above)
(global-set-key (kbd "<C-M-down>") 'new-line-in-between)

;;major-mode
;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Line movement
(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

;; Yank and indent
(global-set-key (kbd "C-S-y") 'yank-indented)

;; Toggle quotes
(global-set-key (kbd "C-\"") 'toggle-quotes)

;; Sorting
(global-set-key (kbd "M-s l") 'sort-lines)

;; Paste URLs as HTML links
(global-set-key (kbd "C-c C-l") 'linkify-from-kill-ring)

;; Buffer file functions
(global-set-key (kbd "C-x t") 'touch-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Easy-mode rgrep
(global-set-key (kbd "M-s s") 'rgrep)

(global-set-key (kbd "M-k") 'kill-whole-line)
(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "C-M-k") 'kill-sentence)
(global-set-key (kbd "<f5>") 'kill-this-buffer)
;;(global-set-key (kbd "<f6>") 'iswitchb-buffer) ; 'ido-switch-buffer
(global-set-key (kbd "<f6>") 'ido-switch-buffer)
;;(global-set-key (kbd "<f6>") 'ibuffer)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f7>") 'save-buffer)
(global-set-key (kbd "<f8>") '(lambda () (interactive) (save-buffer) (server-edit)))

(global-set-key (kbd "<f10>") 'my-recompile)
(global-set-key (kbd "C-<f10>") 'compile)
(global-set-key (kbd "S-<f10>") 'local-compile)
(global-set-key (kbd "<f11>") 'local-shell)
;;(global-set-key (kbd "<f11>") 'shell)
(global-set-key (kbd "S-<f12>") 'previous-error)
(global-set-key (kbd "C-<f12>") 'first-error)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-]") 'same-cursor-and-buffer-other-window)
;;(global-set-key (kbd "M-s") 'fixup-whitespace)
;;(global-set-key [(meta control escape)] 'iconify-or-deiconify-frame) ; minimize
;; Other useful strokes and commands
;; M-: (alt-shift-;) - evaluate lisp expression
;; C-x C-e - evaluate the preceding lisp expression on this line
;; edebug-<tab> a suite of elisp debugging functions (e.g. edebug-defun)
;; M-! (alt-shift-1) - do a shell command, e.g. tlm edit
;; C-x C-f (visit file) to make a buffer modifiable after you've 'tlm edited' it.
(global-set-key (kbd "M-<f9>") 'multi-occur-in-matching-buffers)
(global-set-key (kbd "M-s s") 'rgrep)
(global-set-key (kbd "M-o") 'nuke-line)
(global-set-key (kbd "C-/") 'tweakemacs-comment-dwim-region-or-one-line)
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key (kbd "C-x K") 'kill-others-of-this-name)
(global-set-key (kbd "C-/") 'c-comment-region)
(global-set-key (kbd "<C-,>") 'backward-word)
(global-set-key (kbd "<C-.>") 'forward-word)
(global-set-key (kbd "C-x 4 a") 'my-change-log-entry) ; allow repeats
(global-set-key "\C-x5a" 'my-add-todo-entry) ; like 4a for ChangeLog
(global-set-key "\C-xr\C-k" 'my-kill-rectangle)
(global-set-key "\C-xr\C-y" 'my-yank-replace-rectangle)
(global-set-key "\C-xr\C-w" 'my-save-rectangle)
(global-set-key "\C-x\M-Q" 'my-force-writable)
(global-set-key [( control ?\( )] 'my-matching-paren)
(global-set-key "\C-x\C-k" 'my-kill-buffer) ; without asking
(global-set-key (kbd "C-<tab>") 'tab-to-tab-stop) ; regular spaces-to-next-tabstop, not autoindent
(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)
(global-set-key (kbd "M-<backspace>") 'backward-kill-sexp)
(global-set-key [(control shift prior)] 'upcase-word)
(global-set-key [(control shift next)] 'downcase-word)
(global-set-key (kbd "<C-S-return>") 'fullscreen)
(global-set-key (kbd "M-x") 'execute-extended-command)
(global-set-key (kbd "C-c C-s") 'occur)
(global-set-key (kbd "M-m") 'iy-go-to-char)
(global-set-key "\C-\\" 'set-mark-and-goto-line) ;; -with-feedback
(global-set-key (kbd "C-f") 'forward-word)
(require 'scratch-back)(global-set-key (kbd "M-<f2>") 'scratch-back)
(global-set-key (kbd "M-<f3>") 'back-from-scratch)
(global-set-key "\M-u" '(lambda () (interactive) (backward-word 1) (upcase-word 1))) ; whole word
(global-set-key "\M-l" '(lambda () (interactive) (backward-word 1) (downcase-word 1)))
(global-set-key "\M-\C-u" 'turn-on-auto-capitalize-mode)
(global-set-key (kbd "M-C-o") 'same-buffer-other-window)
(global-set-key (kbd "C-x F") 'find-file)
;;(require 'hobo)(global-set-key (kbd "C-x F") 'hobo-find-file)
(global-set-key (kbd "C-e") 'end-of-line)
(global-set-key (kbd "C-a") 'beginning-of-line)
(global-set-key (kbd "M-m") 'query-replace)
(global-set-key (kbd "M-<f1>") 'replace-string)
(global-set-key (kbd "C-<f1>") 'replace-string)
(global-set-key (kbd "<C-next>") 'mark-next-like-this)

(global-set-key (kbd "<home>") 'beginning-of-visual-line)
(global-set-key (kbd "<end>") 'end-of-visual-line)
(when nil (global-set-key [(meta \])] 'same-buffer-other-window))
(global-set-key (kbd "M-]") 'same-buffer-other-window)
(global-set-key (kbd "C-M-r") 'query-replace)
(global-set-key (kbd "S-M-t") '(lambda () (interactive) (transpose-chars -1)))
(define-key occur-mode-map "C-o" 'other-window)
;;(define-key shell-mode-map "\C-r" 'isearch-backward)
;;(define-key lisp-mode-map (kbd "<return>") 'newline-and-indent)
(global-set-key (kbd "C-c \\") 'indent-buffer)
(global-set-key (kbd "M-<kp-8>") 'up-move-char)
(global-set-key (kbd "M-<kp-2>") 'down-move-char)
(global-set-key (kbd "M-<kp-4>") 'backward-move-char)
(global-set-key (kbd "M-<kp-6>") 'forward-move-char)
(global-set-key (kbd "C-d") 'delete-forward-char)
(global-set-key (kbd "<kp-delete>") 'delete-forward-char)
;;'tweakemacs-delete-region-or-char)
(global-set-key (kbd "C-<return>") 'toggle-windows-split)
(global-set-key (kbd "C-<f5>") 'convert-to-unix-newlines)
(global-set-key (kbd "M-C-d") 'kill-sexp)
(global-set-key (kbd "M-a") 'bol-indent)
(require 'setup-zoom)
(global-set-key (kbd "C-M-z") 'toggle-zoom-full)
(require 'top-mode)
(global-set-key (kbd "C-M-S-t") 'top)
(global-set-key (kbd "M-<f10>") 'menu-bar-open)
;;(define-key isearch-mode-map [next] 'isearch-repeat-forward)
;;(define-key isearch-mode-map [prior] 'isearch-repeat-backward)
(require 'setup-ido)
(global-set-key (kbd "C-x C-r") 'recentf-open-files-compl)
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)
(require 'setup-compilation-mode)
(define-key compilation-minor-mode-map (kbd "C-o") 'other-window)
(global-set-key (kbd "M-/") 'dabbrev-expand)
(require 'editing-defuns)
(global-set-key (kbd "S-<return>") 'gr-line-expand)
(global-set-key (kbd "C-;") 'gr-line-expand)
(global-set-key (kbd "C-'") 'gr-cleanup-always)
(global-set-key (kbd "C-M-R") 'query-replace-case)
(global-set-key (kbd "C-M-r") 'query-replace)
;;isearch-mode-map left/right abort+move
(global-set-key (kbd "C-M-g") 'keyboard-quit)
(require 'minibuffer-defuns)
(global-set-key (kbd "C-g") 'keyboard-really-quit)
(global-set-key (kbd "M-g") '(lambda () (interactive) (exchange-point-and-mark 1)))
(global-set-key (kbd "<home>") 'gud-up)
(global-set-key (kbd "<end>") 'gud-step)
(global-set-key (kbd "<help>") 'gud-finish)
(global-set-key (kbd "S-<right>") 'forward-sentence)
(global-set-key (kbd "S-<left>") 'backward-sentence)

;;(global-set-key (kbd ",") (lambda () (interactive) (insert ", ")))
(global-set-key (kbd ",") 'self-insert-command)
(global-set-key (kbd "C-=") 'gr-include)
(global-set-key (kbd "M-'") 'gr-space-operators)
(global-set-key (kbd "C-@") 'set-mark-command)
(global-set-key (kbd "M-o") '(lambda () (interactive) (switch-to-buffer-other-window (other-buffer))))
;;(global-set-key (kbd "<f12>") 'next-error-no-select)
(global-set-key (kbd "<f12>") 'my-next-error)
(global-set-key (kbd "C-<f12>") 'my-previous-error)
(global-set-key (kbd "C-<f12>") '(lambda () (interactive) (gr-raise-buffer-other-frame "*compilation*")))
(global-set-key (kbd "S-<f12>") '(lambda () (interactive) (gr-raise-buffer-other-window "*compilation*")))
(global-set-key (kbd "C-]") '(lambda () (interactive) (insert "}}}")))
(global-set-key (kbd "M-<f7>") '(lambda () (interactive) (save-some-buffers t)))
(require 'setup-helm)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-x y") 'helm-show-kill-ring)
(require 'smex)(global-set-key (kbd "M-x") 'smex)

(require 'setup-ag)
(require 'setup-ack)

(global-set-key (kbd "<f9>") (if gr-have-ag 'gr-ag-cd 'ack))
(global-set-key (kbd "C-<f9>") (if gr-have-ag 'ag-project 'ack))
;;(require 'multiple-cursors)(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(when gr-on-mac
  (global-set-key (kbd "<kp-3>") 'ignore)
  (global-set-key (kbd "<kp-2>") 'ignore)
  (global-set-key (kbd "<kp-1>") 'ignore)
  (global-set-key (kbd "M-<kp-3>") 'ignore)
  (global-set-key (kbd "M-<kp-2>") 'ignore)
  (global-set-key (kbd "M-<kp-1>") 'ignore)
  (global-set-key (kbd "C-<kp-3>") 'ignore)
  (global-set-key (kbd "C-<kp-2>") 'ignore)
  (global-set-key (kbd "C-<kp-1>") 'ignore)
  )
(global-set-key (kbd "C-d") 'backward-delete-char-untabify)
;;(global-set-key (kbd "<f3>") 'gr-next-error-error)
;;(global-set-key (kbd "<f4>") 'compile)
(global-set-key (kbd "<f3>") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<f4>") 'kmacro-end-or-call-macro)
;;(when gr-on-term (global-set-key (kbd "<f1>") 'shell)
;; Help should search more than just commands
;;(global-set-key (kbd "<f1> a") 'apropos)
(global-set-key (kbd "<f1>") 'help-command)

(setq gr-on-term (eq window-system nil))

(global-set-key (kbd "M-C-]") 'gr-fn-to-cpp)

(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)

(global-set-key (kbd "M-:") 'gr-goto-changelog-end)

(global-set-key (kbd "C-c w")   'copy-word)
(global-set-key (kbd "C-c l")   'copy-line)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-p") 'ac-clang-syntax-check)
(global-set-key (kbd "C-x C--") 'rename-current-buffer-file)

(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)
(global-set-key (kbd "M-<f7>") 'gr-close-with-cleanup)

(if (and nil gr-on-term)
;; ESC, M-O (which is really ESC O) and M-[ (which is really ESC [).
    (progn
      (global-unset-key (kbd "M-O"))
      (global-unset-key (kbd "ESC"))
      (global-unset-key (kbd "M-["))
      (global-set-key (kbd "<f1>") 'default-split))
  (progn
    (global-set-key (kbd "M-[") 'default-split)
    (global-set-key (kbd "<f1>") 'help-command)))

(global-set-key (kbd "C-s") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-g") 'keyboard-really-quit)
(define-key isearch-mode-map (kbd "C-SPC") 'set-mark-command)

(global-set-key (kbd "C-x g") 'git-gutter+-mode) ; Turn on/off in the current buffer
(global-set-key (kbd "C-x G") 'global-git-gutter+-mode) ; Turn on/off globally

(eval-after-load 'git-gutter+
  '(progn
     ;;; Jump between hunks
     (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
;;     (define-key git-gutter+-mode-map (kbd "C-x C-a") 'git-)

     ;;; Act on hunks
     (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x C-h") 'git-gutter+-revert-hunks)
     ;; Stage hunk at point.
     ;; If region is active, stage all hunk lines within the region.
     (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
     (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer)))

;; (eval-after-load 'expand-region '(progn (global-set-key (kbd "C-]") 'er/expand-region)))

(eval-after-load 'csv-mode '(define-key csv-mode-map (kbd "C-'") 'csv-align-fields))
(require 'csv-mode)
(global-set-key (kbd "C-]") 'csv-align-fields)
(global-set-key "\M-p" 'decrease-left-margin)
