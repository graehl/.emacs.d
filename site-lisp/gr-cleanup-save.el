;;; minor mode for stripping extra whitespace, tabs->spaces, indent on save
;;; author: Jonathan Graehl (graehl@gmail.com)

(defun gr-install-hook (h f &optional append local)
  (remove-hook h f local)
  (add-hook h f append local))

(defcustom gr-cleanup-save-except-modes '(calc-mode dired-mode)
  "A list of modes in which `gr-cleanup-save-mode' should not be activated." :type '(sybmol) :group 'gr-cleanup-save)
(defcustom gr-cleanup-save-excessive-spaces 1 "after initial hanging indent, replace > this many whitespace chars with this many spaces" :type 'integer :group 'gr-cleanup-save)

(defvar gr-cleanup-save-hook nil
  "Called when `gr-cleanup-save-mode' is turned on.")

(defun gr-cleanup-ok-mode () (not (member major-mode gr-cleanup-save-except-modes)))

(defun gr-cleanup-enabled ()
  (and (gr-cleanup-ok-mode)))
                                        ;gr-cleanup-save-mode
(defun gr-cleanup-save-maybe ()
  ""
  (interactive)
  (message "gr-cleanup-save-maybe?")
  (when (gr-cleanup-enabled) (gr-cleanup-buffer-save)))

;;;###autoload
(defun turn-on-gr-cleanup-save-mode ()
  "Turn on `gr-cleanup-save-mode'"
  (interactive)
  (gr-install-hook 'before-save-hook 'gr-cleanup-save-maybe) ;; before-save-hook
  (when (gr-cleanup-ok-mode)
    (loop for h in gr-cleanup-save-hook do (funcall h))
    (gr-cleanup-save-mode +1)))

;;;###autoload
(defun turn-off-gr-cleanup-save-mode ()
  "Turn off `gr-cleanup-save-mode'"
  (interactive)
  (when (gr-cleanup-ok-mode)
    (gr-cleanup-save-mode -1)))

;;;###autoload
(define-globalized-minor-mode gr-cleanup-save-global-mode
  gr-cleanup-save-mode
  turn-on-gr-cleanup-save-mode)

;;;###autoload
(define-minor-mode gr-cleanup-save-mode
  "Wrap the buffer text with adaptive filling."
  :init-value nil
  :lighter " CS"
  )


;;; impl:


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


(defconst excessive-newlines-regexp "\n\n\n\n+" "regexp to replace")
(defconst excessive-newlines-replacement "\n" "replacement")
(defconst excessive-newlines-replacement-n 3 "use this may repetitions of excessive-newlines-replacement")
(defun excessive-newlines-compress (&optional nrepl)
  "replace excessive-newlines-regexp with nrepl newlines in the whole buffer"
  (interactive "p")
  (when (eq nrepl nil) (setq nrepl excessive-newlines-replacement-n))
  (while (re-search-forward excessive-newlines-regexp (point-max) t)
    (delete-region (match-beginning 0) (match-end 0))
    (dotimes (i nrepl) (insert excessive-newlines-replacement))))

(defvar make-modes '(makefile-gmake-mode makefile-mode fundamental-mode))
(defun do-untabify () (not (member major-mode make-modes)))
(defun gr-indent-buffer () "indent whole buffer!"
  (interactive)
  (when (do-untabify)
    (indent-region (point-min) (point-max) nil)))

(defun gr-untabify-buffer ()
  (interactive)
  (when (do-untabify)
    (untabify (point-min) (point-max))))

(defun gr-compress-whitespace (&optional over limit)
  "starting from line-initial non-space char (after hanging indent), replace more than [over] spaces in the line or region. operates only on ascii space. if line is all spaces, no change. note: this doesn't skip string constants. [limit] is eol by DEFAULT"
  (interactive)
  (when (eq nil over) (setq over compress-whitespace-over))
  (when (eq nil limit) (setq limit (point-at-eol)))
  (push-mark)
  (set-mark (point))
  (let ((maxsp (make-string over ? )) (ndel 0) (s nil) (col (current-column)))
    (backward-to-indentation)
    (loop until (>= (point) limit)
          do (skip-syntax-forward "^\s" limit)
          until (>= (point) limit)
          do (setq s (point))
          do (skip-syntax-forward "\s" limit)
          until (>= (point) limit)
          do (let ((ex (- (point) (mark) over)))
               (when (> over 0)
                 (setq ndel (+ ndel over))
                 (delete-region s (point))
                 (setq limit (point-at-eol))
                 (insert maxsp))))
    (goto-char (mark))
    (move-to-column col)
    (pop-mark)
    ndel
    ))

(defun gr-cleanup-buffer-compress-whitespace-impl ()
  (goto-char (point-min))
  (loop do (gr-compress-whitespace gr-cleanup-save-excessive-spaces)
        while (> 0 (forward-line))
        ))


(defmacro gr-safe-wrap (&rest body)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,@body))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)))

(defvar gr-cleanup-buffer-excessive-newlines 3)
(defun gr-cleanup-buffer-impl ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (gr-safe-wrap
   (gr-indent-buffer)
   (gr-untabify-buffer)
   (unless (eq nil gr-cleanup-buffer-excessive-newlines)
     (excessive-newlines-compress gr-cleanup-buffer-excessive-newlines))
   (delete-trailing-whitespace)
   (delete-trailing-newlines)
   (gr-cleanup-buffer-compress-whitespace-impl)
   ))

(defun gr-cleanup-buffer-save ()
  "gr-cleanup-buffer catching errors"
  (message "cleaning up buffer on save (catching errors) ...")
  (save-excursion
    (gr-cleanup-buffer-impl)))
(defun gr-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (with-whole-buffer (gr-cleanup-buffer-impl)))


(provide 'gr-cleanup-save)
