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
         (path-elts (lw-filter-path-elts (car source-prep)))
         (copyright (cdr source-prep)))

    (bufstart)
    (if copyright
        (my-copyright copyright)
      (my-copyright))

    (insert "#ifndef " guard "\n"
            "# define " guard "\n")

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
