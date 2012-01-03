(provide 'c-defuns)

(defun my-c-comma-unindent (langelem)
  "Unindent for leading commas"
  (if (my-c-leading-comma-p) '/))

(defun my-c-comma-indent (langelem)
  "Indent for leading commas"
  (if (my-c-leading-comma-p) '*))

(defun my-cleanup-pp-output ()
  "Clean up preprocessor output so that it's at least semi-readable"
  (interactive)

  (let ((selection (my-selection))
        (start (make-marker))
        (end (make-marker))
        )
    (set-marker start (car selection))
    (set-marker end (cdr selection))

    (c++-mode)
    ;; CR before function declaration id
    (subst-re "\\([a-zA-Z0-9_]\\) +\\([a-zA-Z_][a-zA-Z0-9_]*(\\)" "\\1\n\\2" start end)
    (subst-re "\\(\\<return\\>\\|\\<new\\>\\)\n" "\\1 " start end)

    ;; CR after template parameter list
    (subst-re "\\<template\\> *<\\([^<>]+\\)>" "template <\\1>\n" start end)

    (subst-re " *\\(\\s.\\|[()]\\) *" "\\1" start end)
    (subst-re " +" " " start end)

    (subst-re "\\([{}];*\\)" "\\1\n" start end)  ;
    (subst-re "\\([^ ].*\\)\\([{}]\\)" "\\1\n\\2" start end)

    (subst-re ";\\(.\\)" ";\n\\1" start end)

    (subst-re "\\([(]+\\)\\([(]\\)" "\\1\n\\2" start end)
    (subst-re ">\\(\\<struct\\>\\|\\<class\\>\\)" ">\n\\1" start end)
    (indent-region start end nil)
    ))

(defun my-empty-braces ()
  "insert { }"
  (interactive "*")
  (insert "{}")
  (backward-char)
  (indent-according-to-mode)
  )


(defun my-electric-braces ()
  "Insert a pair of braces surrounding a blank line, indenting each according to the mode"
  (interactive "*")
  (let ((bolp
         (save-excursion (skip-chars-backward " \t")
                         (equal (current-column) 0))))
    (insert "{}")
    (if bolp
        (eval (list indent-line-function)))
    )
  (backward-char)
  (newline-and-indent)
  (prev-line 1)
  (end-of-line)
  (newline-and-indent))

(defun indent-cpp-buffer ()
  "text-mode then c++-mode - workaround bad-state bug without saving + reopening buffer"
  (interactive)
  (text-mode)
  (c++-mode)
  (indent-buffer))

(defun c-comment-region ()
  (interactive)
  "insert /* */ around region, no matter what (TODO: toggle comment state, idempotent, ??)"
  (save-excursion
    (let ((start (car (my-selection)))
          (finish (cdr (my-selection))))
      (goto-char finish)
      (insert " */")
      (goto-char start)
      (insert "/* "))))


(defun my-include-guard ()
  "Compute the appropriate #include guard based on the current buffer's name"
  (let* ((split-name (my-split-current-filename))
         (time (decode-time))
         (prefix (car split-name))
         (ext (cdr split-name))
         (extension (if ext (concat "_" ext) ""))
         )
    (upcase
     (concat
      prefix "_" my-initials
      (number-to-string (nth 5 time))
      (number-to-string (nth 4 time))
      (number-to-string (nth 3 time)) extension))))


(defun my-copyright (&optional copyright)
  "Insert a commented COPYRIGHT string. If COPYRIGHT
is not supplied, the boost copyright is used by default"
  (interactive)
  (when nil
    (let ((copy-start (point)))
      (insert (or copyright
                  (or (and (my-path-elts) (boost-copyright))
                      (eval (list my-default-copyright))))
              "\n")

      (comment-region copy-start (point)))))

(defun my-prepare-source ()
  (let* ((all-path-elts (my-path-elts))

         ;; prune off the head of path-elts up to the last occurrence
         ;; of boost, if any otherwise, path-elts will be nil

         ;; this is the index of the namespace root in the path
         (index (position-if
                 (lambda (x)
                   (find-if
                    (lambda (y) (equal (car y) x))
                    my-namespace-roots))
                 all-path-elts :from-end 0))

         ;; the name of the root element
         (root (and index (nth index all-path-elts)))

         ;; the path elements to use for the namespace
         (top-path-elts (and index (subseq all-path-elts index)))

         (path-elts
          (if (and top-path-elts
                   (equal root "boost")
                   (equal "libs" (cadr top-path-elts))
                   (equal "src" (cadddr top-path-elts)))
              (append (list root (caddr top-path-elts)) (cddddr top-path-elts))
            top-path-elts))

         (copyright-function
          (and index
               (cdr (find-if (lambda (y) (equal (car y) root)) my-namespace-roots))))

         (copyright
          (and index
               (eval (list copyright-function))))

         )
    (cons path-elts copyright)))

(defun my-begin-header ()
  "Begin a C/C++ header with include guards and a copyright."
  (interactive)
  (let* ((guard (my-include-guard))
         (source-prep (my-prepare-source))
         (path-elts (my-filter-path-elts (car source-prep)))
         (copyright (cdr source-prep)))

    (bufstart)
    (if copyright
        (my-copyright copyright)
      (my-copyright))

    (insert "#ifndef " guard "\n"
            "#define " guard "\n")

    (let ((final nil) ;; final position
          (nsfini (if path-elts "\n" "")))

      ;; opening namespace stuff
      (insert nsfini)
      (mapc (lambda (n) (insert "namespace " n " { "))
            path-elts)
      (insert nsfini)

      (setq final (point))
      (newline)

      (bufend)
      ;; make sure the next stuff goes on its own line
      (if (not (equal (current-column) 0))
          (newline))

      ;; closing namespace stuff
      (mapc (lambda (n) (insert "}")) path-elts)
      (reduce (lambda (prefix n)
                (insert prefix n) "::")
              path-elts
              :initial-value " // namespace ")
      (insert nsfini)
      (insert nsfini)
      (insert "#endif // " guard)
      (goto-char final))
    )
  )


(defun my-begin-source ()
  "Begin a C/C++ source file"
  (interactive)
  (let* ((source-prep (my-prepare-source))
         (path-elts (car source-prep))
         (copyright (cdr source-prep))
         (basename (car (my-split-current-filename)))
         )


    (bufstart)
    (if copyright
        (my-copyright copyright)
      (my-copyright))

    (let ((final nil) ;; final position
          (nsfini (if path-elts "\n" "")))

      ;; opening namespace stuff
      (insert nsfini)
      (if path-elts
          (progn
            (insert "#include \"")
            (mapc (lambda (n) (insert n "/"))
                  path-elts)
            (insert (downcase basename) ".hpp\"\n\n")))

      (mapc (lambda (n) (insert "namespace " n " { "))
            path-elts)

      (insert nsfini)

      (setq final (point))
      (newline)

      (bufend)
      ;; make sure the next stuff goes on its own line
      (if (not (equal (current-column) 0))
          (newline))

      ;; closing namespace stuff
      (mapc (lambda (n) (insert "}")) path-elts)
      (reduce (lambda (prefix n)
                (insert prefix n) "::")
              path-elts
              :initial-value " // namespace ")
      (insert nsfini)
      (goto-char final)
      )
    )
  )


(defun my-at-preprocessor-directive-p ()
  "return non-nil if point is sitting at the beginning of a preprocessor directive name"
  (and
   (save-excursion
     (re-search-backward "^\\([ \t]*\\)#\\([ \t]*\\)" (line-beginning-position) t))
   (>= (point) (match-beginning 2))
   (<= (point) (match-end 2))
   ))

(defun my-preprocessor-indentation ()
  (save-excursion
    (beginning-of-line)
    (re-search-backward "^[ \t]*#[ \t]*" nil t)
    (goto-char (match-end 0))
    (+ (current-column)
       (if (looking-at "\\(if\\)\\|\\(el\\)") 1 0)))) ; fixme: was 1 0

(defun my-electric-pound-< ()
  (interactive)
  (my-maybe-insert-incude "<" ">"))


(defun my-electric-pound ()
  (interactive)
  (insert "#")
  (if (my-at-preprocessor-directive-p)
      (progn
        (delete-region (match-beginning 1) (match-end 1))
        (move-to-column (my-preprocessor-indentation) t))))

(defun my-electric-pound-e ()
  (interactive)

  (if (my-at-preprocessor-directive-p)
      (progn
        (move-to-column (max 1 (- (my-preprocessor-indentation) 1))))) ; #fixme: was 1
  (insert "e"))

(defun my-c-namespace-indent (langelem)
  "Used with c-set-offset, indents namespace scope elements 2 spaces
from the namespace declaration iff the open brace sits on a line by itself."
  (save-excursion
    (if (progn (goto-char (cdr langelem))
                                        ; (setq column (current-column))
               (end-of-line)
               (while (and (search-backward "{" nil t)
                           (assoc 'incomment (c-guess-basic-syntax))))
               (skip-chars-backward " \t")
               (bolp))
        2)))

(defun my-c-backward-template-prelude ()
  "Back up over expressions that end with a template argument list.

Examples include:

        typename foo<bar>::baz::mumble

        foo(bar, baz).template bing
"
  (while
      (save-excursion
        ;; Inspect the previous token or balanced pair to
        ;; see whether to skip backwards over it
        (c-backward-syntactic-ws)
        (or
         ;; is it the end of a nested template argument list?
         (and
          (eq (char-before) ?>)
          (c-backward-token-2 1 t) ;; skips over balanced "<>" pairs
          (eq (char-after) ?<))

         (and
          (c-backward-token-2 1 t)
          (looking-at "[A-Za-z_\\[(.]\\|::\\|->"))))

    (c-backward-token-2 1 t)))

(defun my-c-leading-comma-p ()
  (save-excursion
    (beginning-of-line)
    (c-forward-token-2 0 nil (c-point 'eol))
    (eq (char-after) ?,)))

(defun my-lineup-first-template-args (langelem)
  "Align lines beginning with the first template argument.

To allow this function to be used in a list expression, nil is
returned if we don't appear to be in a template argument list.

Works with: template-args-cont."
  (let ((leading-comma (my-c-leading-comma-p)))
    (save-excursion
      (c-with-syntax-table c++-template-syntax-table
        (beginning-of-line)
        (backward-up-list 1)
        (if (eq (char-after) ?<)

            (progn
              (my-c-backward-template-prelude)

              (vector
               (+ (current-column)
                  (if leading-comma (/ c-basic-offset 2) c-basic-offset)
                  ))

              ))))))


(defun my-lineup-more-template-args (langelem)
  "Line up template argument lines under the first argument,
adjusting for leading commas. To allow this function to be used in
a list expression, nil is returned if there's no template
argument on the first line.

Works with: template-args-cont."
  (let ((result (c-lineup-template-args langelem)))
    (if (not (eq result nil))
        (if (my-c-leading-comma-p)
            (vector (- (aref result 0) (/ c-basic-offset 2)))
          result))))

(defun my-lineup-template-close (langelem)
  (save-excursion
    (c-with-syntax-table c++-template-syntax-table
      (beginning-of-line)
      (c-forward-syntactic-ws (c-point 'eol))
      (if (and
           (eq (char-after) ?>)
           (progn
             (forward-char)
             (c-backward-token-2 1 t)
             (eq (char-after) ?<)))
          (progn
            (my-c-backward-template-prelude)
            (vector (current-column)))))))

(defun my-c-electric-comma (arg)
  "Amend the regular comma insertion by possibly appending a
  space."
  (interactive "*P") ; Require a writable buffer/take a prefix arg in raw form

  ;; Do the regular action. Perhaps we should be using defadvice here?
  (c-electric-semi&comma arg)

  ;; Insert the space if this comma is the first token on the line, or
  ;; if there are preceding commas followed by a space.
  (and (eq (char-before) ?,)
       (save-excursion
         (backward-char)
         (skip-syntax-backward " ")
         (bolp)
         )
       (insert " "))
  )

(defun my-electric-pound-quote ()
  (interactive)
  (my-maybe-insert-incude "\"" "\""))

(defun my-maybe-insert-incude (open close)
  (if (my-at-preprocessor-directive-p)
      (progn
        (move-to-column (my-preprocessor-indentation) t)
        (insert "include " open)
        (save-excursion
          (insert close)))
    (insert open)))

(defun my-c-electric-gt (arg)
  "Insert a greater-than character.
The line will be re-indented if the buffer is in C++ mode.
Exceptions are when a numeric argument is supplied, point is inside a
literal, or `c-syntactic-indentation' is nil, in which case the line
will not be re-indented."
  (interactive "*P")
  (let ((indentp (and c-syntactic-indentation
                      (not arg)
                      (not (c-in-literal))))
        ;; shut this up
        (c-echo-syntactic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
        (indent-according-to-mode))))

(defun my-c-namespace-open-indent (langelem)
  "Used with c-set-offset, indents namespace opening braces to the
same indentation as the line on which the namespace declaration
starts."
  (save-excursion
    (goto-char (cdr langelem))
    (let ((column (current-column)))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (- (current-column) column))))

(require 'editing-defuns)
(defun gr-c-class-name (&optional else) (interactive)
  (save-excursion
    (if (re-search-backward "^[[:blank:]]*\\(struct\\|class\\) +\\([^\n[:blank:]]+\\)")
        (gr-match-string 2)
      else)))
