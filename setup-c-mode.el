(require 'gradle-mode)
(add-hook 'java-mode-hook '(lambda() (gr-java-hook)))
;;(gradle-mode 1)
(defun mapcar-first-rest (fn-head fn-rest list)
  "applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))
(defun upper-camel-case-wordlist (wl)
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word))) wl) ""))
(defun lower-camel-case-wordlist (wl)
  (mapconcat 'identity (mapcar-first-rest
                        #'(lambda (word) (downcase word))
                        #'(lambda (word) (capitalize (downcase word))) wl) ""))
(defun underscore-case-wordlist (wl)
  (mapconcat 'downcase wl "_"))
(setq default-underscore-type-suffix "type")
;;(setq default-underscore-type-suffix "t")
(defun upper-underscore-case-wordlist (s)
  "Convert words list to words_list_[default-underscore-type-suffix"
  (underscore-case-wordlist (append s (list default-underscore-type-suffix))))
(defun lower-underscore-case-wordlist (s)
  "Convert words list to words_list Under_scoRe string S to under_score"
  (underscore-case-wordlist s))
(defun with-ctoken-words (s f)
  (save-match-data)
  (funcall f (split-string s "[_ ]")))
(defun rest-of-string (s)
  (if (equal "" s) "" (substring s 1)))
(defun first-of-string (s)
  (if (equal "" s) "" (substring s 0 1)))
(defun uncapitalize (s)
  (concat (downcase (first-of-string s)) (rest-of-string s)))

(defun upper-camel-case (s)
  "Convert under_score string S to CamelCase string."
  (with-ctoken-words s 'upper-camel-case-wordlist))
(defun lower-camel-case (s)
  "Convert under_score string S to CamelCase string."
  (with-ctoken-words s 'lower-camel-case-wordlist))
(defun upper-underscore-case (s)
  (with-ctoken-words s 'upper-underscore-case-wordlist))
(defun lower-underscore-case (s)
  (with-ctoken-words s 'lower-underscore-case-wordlist))
(defalias 'lower-underscore-case 'underscore-case)
(defun c-strip-leading-ns (s) (interactive "s")
  (replace-regexp-in-string "\\([^:<]+\\)[:]+" "" s))
(defun c-strip-all-ns (s) (interactive "s")
  (c-strip-leading-ns (c-strip-leading-ns (c-strip-leading-ns s))))
(defun first-char-uc (s) (interactive "s")
  (if (equal s "") "" (substring (upcase s) 0 1)))
;;(defun string-capitalize (s) (interactive "s") (if (equal s "") "" (concat (first-char-uc s) (substring s 1))))
(defun no-suffix-type (s &optional suffix) (interactive "s")
  (if (eq nil suffix) (setq suffix "_type"))
  (if (string-suffix-p suffix s) (substring s 0 (- (length s) (length suffix))) s))
(defun no-suffix-type2 (s &optional suffix suffix2) (interactive "s")
  (if (eq nil suffix) (setq suffix "_type"))
  (if (eq nil suffix2) (setq suffix2 "_t"))
  (no-suffix-type (no-suffix-type s suffix) suffix2))
(defun template-arg-name-no-type (s) (interactive "s")
  (let ((ns (no-suffix-type2 s)))
    (if (equal s ns) (first-char-uc s) (capitalize ns))))
(defun default-typedef-base-name (s) (interactive "s")
  (c-strip-all-ns (replace-regexp-in-string "<.*$" "" s)))
(defun uc-typedef-name (s) (interactive "s")
  (upper-camel-case (default-typedef-base-name s)))
(defun lc-typedef-name (s) (interactive "s")
  (concat (downcase (default-typedef-base-name s)) "_type"))

(defun c++-strip-ref (s)
  (replace-regexp-in-string "\\( *\\(const\\|&\\|\\*\\)*\\ *\\)*$" "" s))
(defun c++-var-from-type-noref (s &optional suffix suffix2)
  (if (eq nil suffix) (setq suffix "_type"))
  (if (eq nil suffix2) (setq suffix2 "_t"))
  (let ((ns (no-suffix-type2 s)))
    (if (equal s ns)
        (let ((lc (uncapitalize s)))
          (if (equal s lc) (downcase (first-of-string s))
            lc)))))
(defun c++-var-from-type (s)
  (c++-var-from-type-noref (c++-strip-ref s)))

(defalias 'default-typedef-name 'lc-typedef-name)
(defalias 'default-template-arg-name 'template-arg-name-no-type)
(defalias 'default-c-var-case 'lower-underscore-case)
(defalias 'default-c-type-case 'upper-underscore-case)

(defconst graehl-style
  '(
                                        ; (c-offsets-alist . (
                                        ; (defun-block-intro . 2)
    ;; ...no exceptions.
                                        ; (substatement-open . 0)
                                        ; (inline-open . 0)
                                        ; (comment-intro . 0)
                                        ; ))
    (c-electric-pound . nil)
    (c-syntactic-indentation-in-macros . t)
    (c-indent-comments-syntactically-p . t)
    ;; (c-offsets-alist . ((innamespace . 0)))
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
                                        ;(key-chord-define c-mode-map ";;" "\C-e")
  (c-set-style "bsd")
  (setq c-default-style "bsd"
                                        ; c-backspace-function 'c-hungry-delete
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
  (local-set-key [(control tab)] ; move to next tempo mark
                 'tempo-forward-mark)
                                        ; (local-set-key (kbd "<delete>") 'c-hungry-delete-forward)

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

                                        ; (local-set-key [tab] 'my-c-tab)
                                        ; (local-set-key [{] 'my-electric-braces)
  (local-set-key [?\M-{] "\C-q{")
  (local-set-key [(control ?{)] 'my-empty-braces)
                                        ; (local-set-key [(meta \`)] 'my-cpp-toggle-src-hdr)
  (local-set-key [(meta \`)] 'sourcepair-load)
  (local-set-key [(control \`)] 'sourcepair-load)
  (local-set-key [?#] 'self-insert-command)
  (local-set-key [?<] 'my-electric-pound-<)
  (local-set-key [?>] 'my-c-electric-gt)
  (local-set-key [?\"] 'my-electric-pound-quote)
  (local-set-key [?e] 'my-electric-pound-e)
  (local-set-key [?,] 'my-c-electric-comma)
  (local-set-key (kbd ";") 'self-insert-command)
  (local-set-key (kbd "*") 'self-insert-command)
  ;;(local-set-key [?:] (insert-if-comment ":" ": "))
  (make-local-variable 'parens-require-spaces)
  (setq parens-require-spaces nil)

  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map (kbd "<ret>") 'newline-and-indent)
  (define-key c-mode-base-map (kbd "C-j") 'dabbrev-expand)
  (c-set-offset 'c 'c-lineup-C-comments)

  ;; because autopair -> electric paren was bothering me - not sure if this fixes
  ;;  (define-key c-mode-base-map (kbd "(") 'self-insert-command)
  ;;  (define-key c-mode-base-map (kbd ")") 'self-insert-command)

  ;; Set the comments to start where they ought to.
  (setq-default c-comment-continuation-stars "* ")
  )
(require 'cc-engine)

(require 'gud)
(defun my-gud-run-to-cursor ()
  (gud-tbreak)
  (gud-cont))

(require 'setup-code-modes)
(install-hooks c-modes-hook 'google-set-c-style)
(install-hooks c-modes-hook 'gr-c-mode-hook)
(provide 'setup-c-mode)
(defun gr-tab4 () (interactive)
       (setq c-basic-offset 4)
       )
(defun gr-java-hook () (interactive)
       (setq c-basic-offset 4)
       )

(defun gray-assert ()
  (interactive)
  ;; gray out the "assert(...)" wrapper
  (font-lock-add-keywords nil
                          '(("\\<\\(assert\(.*\);\\)" 1 '(:foreground "#444444") t)))

  ;; gray out the stuff inside parenthesis with a slightly lighter color
  (font-lock-add-keywords nil
                          '(("\\<assert\\(\(.*\);\\)" 1 '(:foreground "#666666") t)))
  )

(add-hook 'c-mode-common-hook 'gray-assert)

(require 'google-c-style)
