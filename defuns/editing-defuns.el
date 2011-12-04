;; Basic text editing defuns

(defun new-line-below ()
  (interactive)
  (if (eolp)
      (newline)
    (end-of-line)
    (newline)
    (indent-for-tab-command)))

(defun new-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-for-tab-command))

(defun new-line-in-between ()
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (duplicate-region arg)
    (duplicate-current-line arg)))

(defun duplicate-region (num &optional start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used. Adds the duplicated text to the kill ring."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (kill-ring-save start end)
    (goto-char end)
    (dotimes (i num)
      (insert region))))

(defun duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (duplicate-region num (point-at-bol) (1+ (point-at-eol)))
  (goto-char (1- (point))))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines 1))
    (next-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun yank-indented ()
  (interactive)
  (let ((start (point)))
    (yank)
    (indent-region start (point))))

;; toggle quotes

(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun move-point-forward-out-of-string ()
  (while (point-is-in-string-p) (forward-char)))

(defun move-point-backward-out-of-string ()
  (while (point-is-in-string-p) (backward-char)))

(defun alternate-quotes-char ()
  (if (eq ?' (current-quotes-char)) ?\" ?'))

(defun toggle-quotes ()
  (interactive)
  (if (point-is-in-string-p)
    (let ((old-quotes (char-to-string (current-quotes-char)))
          (new-quotes (char-to-string (alternate-quotes-char)))
          (start (make-marker))
          (end (make-marker)))
      (save-excursion
        (move-point-forward-out-of-string)
        (backward-delete-char 1)
        (set-marker end (point))
        (insert new-quotes)
        (move-point-backward-out-of-string)
        (delete-char 1)
        (insert new-quotes)
        (set-marker start (point))
        (replace-string new-quotes (concat "\\" new-quotes) nil start end)
        (replace-string (concat "\\" old-quotes) old-quotes nil start end)))
    (error "Point isn't in a string")))

;; kill region if active, otherwise kill backward word

(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

;; copy region if active
;; otherwise copy to end of current line
;;   * with prefix, copy N whole lines

(defun copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(defun copy-whole-lines (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun copy-line (arg)
  "Copy to end of line, or as many lines as prefix argument"
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

(defun kill-and-retry-line ()
  "Kill the entire current line and reposition point at indentation"
  (interactive)
  (back-to-indentation)
  (kill-line))

(defun replace-next-underscore-with-camel (arg)
  (interactive "p")
  (if (> arg 0)
      (setq arg (1+ arg))) ; 1-based index to get eternal loop with 0
  (while (not (= arg 1))
    (search-forward-regexp "_\\sw")
    (forward-char -2)
    (delete-char 1)
    (capitalize-word 1)
    (setq arg (1- arg))))

(defun kill-whole-line ()
  "delete from start of current line instead of cursor as per normal kill-line"
  (interactive)
  (let ((c (current-column)))
    (beginning-of-line)
    (kill-line)
    (move-to-column c)))

(defun nuke-line ()
  "Delete current line without sending to kill ring."
  (interactive)
  (setq previous-column (current-column))
  (delete-region (line-beginning-position) (line-end-position))
  (delete-char 1)
  (move-to-column previous-column))


(defun backward-delete-whitespace-to-column ()
  "delete back to the previous column of whitespace, or just one
    char if that's not possible. This emulates vim's softtabs
    feature."
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char-untabify)
    ;; let's get to work
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      ;; brain freeze, should be easier to calculate goal
      (when (= movement 0) (setq movement tab-width))
      (if (save-excursion
            (backward-char movement)
            (string-match "^\\s-+$" (buffer-substring-no-properties (point) p)))
          (delete-region (- p movement) p)
        (call-interactively 'backward-delete-char-untabify)))))


(defun tweakemacs-delete-region-or-char ()
  "Delete a region or a single character."
  (interactive)
  (if mark-active
      (delete-region (region-beginning) (region-end))
    (backward-delete-whitespace-to-column)))

(defun tweakemacs-move-one-line-downward ()
  "Move current line downward once."
  (interactive)
  (forward-line)
  (transpose-lines 1)
  (prev-line 1))

(defun tweakemacs-move-one-line-upward ()
  "Move current line upward once."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun tweakemacs-comment-dwim-region-or-one-line (arg)
  "When a region exists, execute comment-dwim, or if comment or uncomment the current line according to if the current line is a comment."
  (interactive "*P")
  (if mark-active
      (comment-dwim arg)
    (save-excursion
      (let ((has-comment? (progn (beginning-of-line) (looking-at (concat "\\s-*" (regexp-quote comment-start))))))
	(push-mark (point) nil t)
	(end-of-line)
	(if has-comment?
	    (uncomment-region (mark) (point))
	  (comment-region (mark) (point)))))))

(defun my-add-todo-entry ()
  "like add-change-log-entry but uses filename of TODO"
  (interactive)
  (add-change-log-entry nil "TODO" t t)
  )

(defvar my-change-log-file "~/x/ChangeLog")
(defun my-change-log-entry ()
  "allow repeated entries, custom file"
  (interactive)
  (add-change-log-entry nil my-change-log-file t t)
  )
(defvar my-notes-file "~/x/NOTES")


(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun my-matching-paren (arg)
  (interactive "P")
  (if arg
      () ;;(insert "%")  ; insert the character we're bound to
    (cond ((looking-at "[[({]")
           (forward-sexp 1)
           (forward-char -1))
          ((looking-at "[]})]")
           (forward-char 1)
           (forward-sexp -1))
          (t
           ;; (insert "%")  ; insert the character we're bound to
           ))))

(defun slash-to-backslash (text)
  (substitute ?\\ ?/ text))
