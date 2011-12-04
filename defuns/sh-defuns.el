(defun my-sh-indentation ()
  (save-excursion
    (set-mark (point)) ; {
    (if (re-search-backward "^[^#\n]*\\(\\[\\|]\\|(\\|)\\|{\\|}\\)" nil t)
        (+ (current-indentation)
           (progn
             (goto-char (match-beginning 1))
             (if (looking-at "[[{(]") 4 0)))
      0)))

(defun my-sh-newline-and-indent ()
  (interactive "*")
  (newline)
  (indent-line-to
   (save-excursion
     (skip-chars-backward " \t\n")
     (+ (my-sh-indentation)
        (let ((start (point)))
          (if (and (re-search-backward "^[^#\n]*[;{}]" (line-beginning-position) t)
                   (equal (match-end 0) start))
              0 4))))))

(defun my-sh-electric-braces ()
  (interactive "*")
  (let ((indentation (my-sh-indentation)))
    (if (equal (current-indentation) (current-column))
        (indent-line-to indentation))
    (insert "{}")
    (backward-char)
    (newline)
    (newline)
    (indent-line-to indentation)
    (prev-line 1)
    (indent-to (+ indentation 4))))

(defun my-sh-electric-open-brace ()
  (interactive "*")
  (let ((indentation (my-sh-indentation)))
    (if (equal (current-indentation) (current-column))
        (indent-line-to indentation))
    (insert "{")
    (newline)
    (indent-line-to (+ indentation 4))))

;; Stolen from lisp-mode.el, with slight modifications for reformatting comments
;;
(defun my-sh-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Emacs Lisp comments.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial semicolons."
  (interactive "P")
  (let (
        ;; Non-nil if the current line contains a comment.
        has-comment

        ;; Non-nil if the current line contains code and a comment.
        has-code-and-comment

        ;; If has-comment, the appropriate fill-prefix for the comment.
        comment-fill-prefix
        )

    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond

       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*#[# \t]*")
        (setq has-comment t
              comment-fill-prefix (buffer-substring (match-beginning 0)
                                                    (match-end 0))))

       ;; A line with some code, followed by a comment?  Remember that the
       ;; semi which starts the comment shouldn't be part of a string or
       ;; character.
       ((condition-case nil
            (save-restriction
              (narrow-to-region (point-min)
                                (save-excursion (end-of-line) (point)))
              (while (not (looking-at "#\\|$"))
                (skip-chars-forward "^#\n\"\\\\?")
                (cond
                 ((eq (char-after (point)) ?\\) (forward-char 2))
                 ((memq (char-after (point)) '(?\" ??)) (forward-sexp 1))))
              (looking-at "#+[\t ]*"))
          (error nil))
        (setq has-comment t has-code-and-comment t)
        (setq comment-fill-prefix
              (concat (make-string (/ (current-column) 8) ?\t)
                      (make-string (% (current-column) 8) ?\ )
                      (buffer-substring (match-beginning 0) (match-end 0)))))))

    (if (not has-comment)
        ;; `paragraph-start' is set here (not in the buffer-local
        ;; variable so that `forward-paragraph' et al work as
        ;; expected) so that filling (doc) strings works sensibly.
        ;; Adding the opening paren to avoid the following sexp being
        ;; filled means that sexps generally aren't filled as normal
        ;; text, which is probably sensible.  The `;' and `:' stop the
        ;; filled para at following comment lines and keywords
        ;; (typically in `defcustom').
        (let ((paragraph-start (concat paragraph-start
                                       "\\|\\s-*[\(#:\"]")))
          (fill-paragraph justify))

      ;; Narrow to include only the comment, and then fill the region.
      (save-excursion
        (save-restriction
          (beginning-of-line)
          (narrow-to-region
           ;; Find the first line we should include in the region to fill.
           (save-excursion
             (while (and (zerop (prev-line 1))
                         (looking-at "^[ \t]*#")))
             ;; We may have gone too far.  Go forward again.
             (or (looking-at ".*#")
                 (forward-line 1))
             (point))
           ;; Find the beginning of the first line past the region to fill.
           (save-excursion
             (while (progn (forward-line 1)
                           (looking-at "^[ \t]*#")))
             (point)))

          ;; Lines with only semicolons on them can be paragraph boundaries.
          (let* ((paragraph-start (concat paragraph-start "\\|[ \t#]*$"))
                 (paragraph-separate (concat paragraph-start "\\|[ \t#]*$"))
                 (paragraph-ignore-fill-prefix nil)
                 (fill-prefix comment-fill-prefix)
                 (after-line (if has-code-and-comment
                                 (save-excursion
                                   (forward-line 1) (point))))
                 (end (progn
                        (forward-paragraph)
                        (or (bolp) (newline 1))
                        (point)))
                 ;; If this comment starts on a line with code,
                 ;; include that like in the filling.
                 (beg (progn (backward-paragraph)
                             (if (eq (point) after-line)
                                 (prev-line 1))
                             (point))))
            (fill-region-as-paragraph beg end
                                      justify nil
                                      (save-excursion
                                        (goto-char beg)
                                        (if (looking-at fill-prefix)
                                            nil
                                          (re-search-forward comment-start-skip)
                                          (point))))))))
    t))
(require 'sh-script)
;; not very useful
(defun my-sh-electric-close-brace ()
  (interactive "*")
  (let ((indentation
         (progn
           (delete-region (point)
                          (progn
                            (or (zerop (skip-chars-backward " \t\n"))
                                (if (sh-quoted-p)
                                    (forward-char)))
                            (point)))
           (if (equal (char-before) 123) (current-indentation)
             (- (current-indentation) 4)))))
    (newline)
    (indent-to indentation)
    (insert "}")
    (newline)
    (indent-to indentation)))

