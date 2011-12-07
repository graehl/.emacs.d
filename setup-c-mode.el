(defconst graehl-style
  '(
                                        ;        (c-offsets-alist . (
                                        ;                        (defun-block-intro . 2)
    ;; ...no exceptions.
                                        ;                        (substatement-open . 0)
                                        ;                        (inline-open . 0)
                                        ;                        (comment-intro . 0)
                                        ;     ))
    (c-electric-pound . t)
    (c-syntactic-indentation-in-macros . t)
    (c-indent-comments-syntactically-p . t)
;;    (c-offsets-alist . ((innamespace . 0)))
    (c-hanging-braces-alist . (
                               ;; We like hanging open braces.
                               (brace-list-open)
                               (brace-entry-open)
                               (statement-cont)
                               (substatement-open) ;after
                               (block-close) ;c-snug-do-if
                               (extern-lang-open) ;after
                               (inexpr-class-open) ;after
                               (inexpr-class-close) ;before
                               ))
    (c-echo-syntactic-information-p . t)
    (indent-tabs-mode . nil)
    )
  "graehl")


(require 'setup-sourcepair)
(defun gr-c-mode-hook ()
  (interactive)
  (c-add-style "graehl" graehl-style t)
  (c-toggle-auto-hungry-state 1)
  (key-chord-define c-mode-map ";;" "\C-e")
  (c-set-style "bsd")
  (setq c-default-style "bsd"
                                        ;        c-backspace-function 'c-hungry-delete
                                        ;'backward-delete-char
        c-basic-offset 2
        c-tab-always-indent t)

  (modify-syntax-entry ?_ "w")

  ;; Add 2 spaces of indentation when the open brace is on a line by itself
  (c-set-offset 'innamespace 'my-c-namespace-indent)

  ;; indent solo opening braces to the same indentation as the line on
  ;; which the namespace starts
  (c-set-offset 'namespace-open 'my-c-namespace-open-indent)

  ;; indent access labels public/private/protected by 1 space, as in 'M'. I
  ;; kinda like that.
  (c-set-offset 'access-label -3)
  (local-set-key [(control tab)]     ; move to next tempo mark
                 'tempo-forward-mark)
                                        ;  (local-set-key (kbd "<delete>") 'c-hungry-delete-forward)

  ;;
  ;;fixup template indentation
  ;;
  (c-set-offset 'template-args-cont
                (quote
                 (my-lineup-more-template-args
                  my-lineup-template-close
                  my-lineup-first-template-args
                  +)))

  (set-variable 'c-backslash-max-column 200)

  (my-code-mode-hook)

                                        ;  (local-set-key [tab] 'my-c-tab)
                                        ;  (local-set-key [{] 'my-electric-braces)
  (local-set-key [?\M-{] "\C-q{")
  (local-set-key [(control ?{)] 'my-empty-braces)
                                        ;  (local-set-key [(meta \`)] 'my-cpp-toggle-src-hdr)
  (local-set-key [(meta \`)] 'sourcepair-load)
  (local-set-key [(control \`)] 'sourcepair-load)
  (local-set-key [?#] 'my-electric-pound)
  (local-set-key [?<] 'my-electric-pound-<)
  (local-set-key [?>] 'my-c-electric-gt)
  (local-set-key [?\"] 'my-electric-pound-quote)
  (local-set-key [?e] 'my-electric-pound-e)
  (local-set-key [?,] 'my-c-electric-comma)
  (local-set-key (kbd ";") 'self-insert-command)
  (make-local-variable 'parens-require-spaces)
  (setq parens-require-spaces nil)

  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map (kbd "<ret>") 'newline-and-indent)
  (define-key c-mode-base-map (kbd "C-j") 'dabbrev-expand)
  (c-set-offset 'c 'c-lineup-C-comments)

  ;; Set the comments to start where they ought to.
  (setq-default c-comment-continuation-stars "* ")
  )

(require 'gud)
(defun my-gud-run-to-cursor ()
  (gud-tbreak)
  (gud-cont))

(require 'setup-code-modes)
(install-hooks c-modes-hook 'gr-c-mode-hook)
(provide 'setup-c-mode)
