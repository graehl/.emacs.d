;; Buffer-related defuns

(require 'imenu)

(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (if (= n 1) (lisp-interaction-mode)) ; 1, because n was incremented
    ))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;;; These belong in coding-hook:

;; We have a number of turn-on-* functions since it's advised that lambda
;; functions not go in hooks. Repeatedly evaling an add-to-list with a
;; hook value will repeatedly add it since there's no way to ensure
;; that a lambda doesn't already exist in the list.

(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-hl-line-mode ()
  (if window-system (hl-line-mode t)))

(defun turn-on-save-place-mode ()
  (setq save-place t))

(defun turn-on-whitespace ()
  (whitespace-mode t))

(defun turn-off-tool-bar ()
  (tool-bar-mode -1))

(add-hook 'coding-hook 'local-column-number-mode)
(add-hook 'coding-hook 'local-comment-auto-fill)
(add-hook 'coding-hook 'turn-on-hl-line-mode)

;; The regexp "\\s-+$" is too general, since form feeds (\n), carriage
;; returns (\r), and form feeds/page breaks (C-l) count as whitespace in
;; some syntaxes even though they serve a functional purpose in the file.
(defconst whitespace-regexp "[ \t]+$"
  "Regular expression which matches trailing whitespace.")

;; Match two or more trailing newlines at the end of the buffer; all but
;; the first newline will be deleted.
(defconst whitespace-eob-newline-regexp "\n\n+\\'"
  "Regular expression which matches newlines at the end of the buffer.")

(defun delete-trailing-newlines () "delete extra end-of-buffer newlines"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward whitespace-eob-newline-regexp nil t)
         (delete-region (1+ (match-beginning 0)) (match-end 0)))))

(defmacro with-whole-buffer (fn)
  `(save-excursion
     (save-restriction
       (widen) (goto-char (point-min))
       (save-match-data (progn ,fn)))))


(defconst excessive-newlines-regexp "\n\n\n\n+" "regexp to replace")
(defconst excessive-newlines-replacement "\n" "replacement")
(defconst excessive-newlines-replacement-n 3 "use this may repetitions of excessive-newlines-replacement")
(defun excessive-newlines-compress (&optional nrepl)
  "replace excessive-newlines-regexp with nrepl newlines in the whole buffer"
  (interactive "p")
  (when (eq nrepl nil) (setq nrepl excessive-newlines-replacement-n))
  (with-whole-buffer
   (while (re-search-forward excessive-newlines-regexp (point-max) t)
     (delete-region (match-beginning 0) (match-end 0))
     (dotimes (i nrepl) (insert excessive-newlines-replacement)))))


(defvar untabify-except-modes '(makefile-mode make-mode)
  "A list of modes in which saving shouldn't remove tabs.")

(defun do-untabify() (not (member major-mode untabify-except-modes)))
(defun untabify-buffer ()
  (interactive)
  (when (do-untabify)
    (untabify (point-min) (point-max))))

(defvar cleanup-buffer-excessive-newlines nil)
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (unless (eq nil cleanup-buffer-excessive-newlines)
    (excessive-newlines-compress cleanup-buffer-excessive-newlines))
  (delete-trailing-whitespace)
  (delete-trailing-newlines))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun remove-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun kill-others-of-this-name () "" (interactive) (kill-other-buffers-of-this-file-name nil))
(defun kill-other-buffers-of-this-file-name (&optional buffername)
  "Kill all other buffers visiting files of the same base name."
  (interactive "bBuffer to make unique: ")
  (let ((buffer (if buffername (get-buffer buffername) (current-buffer))))
    (cond ((buffer-file-name buffer)
           (let ((name (file-name-nondirectory (buffer-file-name buffer))))
             (loop for ob in (buffer-list)
                   do (if (and (not (eq ob buffer))
                               (buffer-file-name ob)
                               (let ((ob-file-name (file-name-nondirectory (buffer-file-name ob))))
                                 (or (equal ob-file-name name)
                                     (string-match (concat name "\\(\\.~.*\\)?~$") ob-file-name))) )
                          (kill-buffer ob)))))
          (message "This buffer has no file name."))))

(defun my-mark-or-point ()
  "Return the mark if it is active, otherwise the point."
  (if (mark-active) (mark) (point)))

(defun my-selection ()
  "Return a pair [start . finish) delimiting the current selection"
  (let ((start (make-marker))
        (finish (make-marker)))
    (set-marker start (min (my-mark-or-point) (point)))
    (set-marker finish (max (my-mark-or-point) (point)))
    (cons start finish)))

(defun my-replace-in-region (start finish key replacement)
  "In the range [START, FINISH), replace text matching KEY with REPLACEMENT"
  (goto-char start)
  (while (search-forward key finish t)
    (replace-match replacement)))

(defun my-activate-mark ()
  "Make the mark active if it is currently inactive"
  (set-mark (mark t)))

(defun my-force-writable ()
  "Make this buffer and its file writable.  Has no effect on
  buffers not associated with a file"
  (interactive)
  (let ((f (buffer-file-name)))
    (if f
        (let* ((modes (file-modes f))
               (newmodes (logior ?\200 modes))
               )
          (if (not (equal modes newmodes))
              (progn
                (set-file-modes f newmodes)
                (if (not (buffer-modified-p))
                    (revert-buffer nil t t))
                ))))))

(defun my-kill-buffer ()
  "Just kill the current buffer without asking, unless of course it's a
modified file"
  (interactive)
  (kill-buffer (current-buffer)))


(defun revert-all-buffers()
  "Refreshs all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (if (string-match "\\*" (buffer-name buffer))
          (progn
            (setq list (cdr list))
            (setq buffer (car list)))
        (progn
          (set-buffer buffer)
          (revert-buffer t t t)
          (setq list (cdr list))
          (setq buffer (car list))))))
  (message "Refreshing open files"))


(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-buffer () "whole buffer!"
  (interactive)
  (when (do-untabify)
    (save-excursion (indent-region (point-min) (point-max) nil))))

(defun prev-line (n) (forward-line (- n)))
(defun bufend() (goto-char (point-max)))
(defun bufstart() (goto-char (point-min)))

(defun same-buffer-other-window ()
  "switch to the current buffer in the other window"
  (interactive)
  (switch-to-buffer-other-window (current-buffer))
  )

(defun set-mark-and-goto-line (line)
  "Set mark and prompt for a line to go to."
  (interactive "NLine #: ")
  (push-mark nil t nil)
  (goto-line line))
