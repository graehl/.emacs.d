;;; minor mode for stripping extra whitespace, tabs->spaces, indent on save
;;; author: Jonathan Graehl (graehl@gmail.com)

(defun gr-install-hook (h f &optional append local)
  (remove-hook h f local)
  (add-hook h f append local))

(defcustom gr-cleanup-save-except-modes '(calc-mode dired-mode)
  "A list of modes in which `gr-cleanup-save-mode' should not be activated." :type '(symbol) :group 'gr-cleanup-save)
(defcustom make-modes nil
  "A list of modes in which `gr-cleanup-untabify' and `gr-cleanup-indent' should not be activated." :type '(symbol) :group 'gr-cleanup-save)
(defcustom gr-cleanup-skip-compress-whitespace-modes '(fundamental-mode change-log-mode sh-mode shell-mode cmake-mode)
  "A list of modes in which `gr-cleanup-compress-whitespace' should not be activated." :type '(symbol) :group 'gr-cleanup-save)
(defcustom gr-cleanup-save-max-spaces 1 "after initial hanging indent, replace > this many whitespace chars with this many spaces" :type 'integer :group 'gr-cleanup-save)
(defcustom gr-cleanup-never-indent t "never automatically indent buffer when saving (this is often slow)" :type 'boolean :group 'gr-cleanup-save)
(defcustom gr-cleanup-never-compress t "never compress extra spaces when saving (this is often VERY slow)" :type 'boolean :group 'gr-cleanup-save)
(defcustom gr-cleanup-never-untabify nil "never untabify when saving (this is often slow)" :type 'boolean :group 'gr-cleanup-save)
(setq gr-cleanup-never-untabify nil)
(defcustom gr-cleanup-buffer-excessive-newlines 3 "if not nil or 0, replace excessive newlines with this many" :type 'integer :group 'gr-cleanup-save)
(defcustom gr-cleanup-compress-whitespace-fast t "use simple regex rather than syntax tables - may affect comments/strings" :type 'boolean :group 'gr-cleanup-save)
(defvar make-modes '(conf-mode conf-unix-mode makefile-bsdmake-mode makefile-gmake-mode makefile-mode fundamental-mode) "skip indent on cleanup for these modes")
(defvar shell-modes '(shell-script shell-mode sh-mode)) ;; don't add space around var=val
(defun gr-save-with-cleanup () "gr-cleanup-always and save-buffer" (interactive) (gr-cleanup-always) (save-buffer))
(defun gr-cleanup-skip-save-p () (member major-mode gr-cleanup-save-except-modes))
(defun gr-cleanup-skip-indent-p () (or gr-cleanup-never-indent (member major-mode make-modes)))
(defun gr-cleanup-skip-untabify-p () (or gr-cleanup-never-untabify (member major-mode make-modes)))
(defun gr-skip-manual-compress-whitespace-p () (member major-mode gr-cleanup-skip-compress-whitespace-modes))
(defun gr-cleanup-skip-compress-whitespace-p () (or gr-cleanup-never-compress (gr-skip-manual-compress-whitespace-p)))
(defun gr-cleanup-space-assign () (not (or (member major-mode shell-modes) (member major-mode make-modes))))
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
  (remove-hook 'before-save-hook 'gr-cleanup-save-maybe)
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
;; the first newline will be
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
  (message "compress blank lines...")
  (when (eq nrepl nil) (setq nrepl excessive-newlines-replacement-n))
  (while (re-search-forward excessive-newlines-regexp (point-max) t)
    (delete-region (match-beginning 0) (match-end 0))
    (dotimes (i nrepl) (insert excessive-newlines-replacement))))

(defun gr-indent-buffer-maybe () "indent whole buffer!"
  (interactive)
  (when (not (gr-cleanup-skip-indent-p))
    (gr-indent-buffer)))

(defun gr-indent-buffer () "indent whole buffer!"
  (interactive)
  (message "indent...")
  (widen)
  (indent-region (point-min) (point-max) nil))

(defun gr-buffer-contains-substring (string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun gr-untabify-buffer ()
  (interactive)
  (if (gr-cleanup-skip-untabify-p)
      (message "skip untabify (gr-cleanup-skip-untabify-p)")
    (progn
      (message "untabify...")
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "\t" nil t)
          (untabify (1- (point)) (point-max)))))))

;; this is fast but will mess with comments and string constants (may be surprising)
(defun gr-compress-whitespace-fast-impl (&optional over)
  "starting from line-initial non-space char (after hanging indent), replace more than [over] spaces in the line or region. operates only on ascii space. if line is all spaces, no change. note: this doesn't skip string constants. [limit] is eol by DEFAULT"
  (interactive)
  (when (eq nil over) (setq over gr-cleanup-save-max-spaces))
  (let* ((maxsp (make-string over ? )) (repl (concat "\\b" maxsp " +")))
    (replace-regexp repl maxsp)))

;; this is very slow but should skip comments and strings.
(defun gr-compress-whitespace-impl (&optional over)
  "starting from line-initial non-space char (after hanging indent), replace more than [over] spaces in the line or region. operates only on ascii space. if line is all spaces, no change. note: this doesn't skip string constants. [limit] is eol by DEFAULT"
  (interactive)
  (when (eq nil over) (setq over gr-cleanup-save-max-spaces))
  (let ((maxsp (make-string over ? )) (ndel 0) (limit (point-at-eol)) s (col (current-column)))
    (backward-to-indentation 0)
    (loop until (>= (point) limit)
          do (skip-syntax-forward "^\s" limit)
          until (>= (point) limit)
          do (setq s (point))
          do (skip-syntax-forward "\s" limit)
          until (>= (point) limit)
          do (let ((ex (- (point) s over)))
               (when (> over 0)
                 (setq ndel (+ ndel over))
                 (delete-region s (point))
                 (setq limit (point-at-eol))
                 (insert maxsp))))
    (move-to-column col)
    ndel
    ))

(defun gr-compress-whitespace-impl (&optional over)
  (interactive)
  (when (eq nil over) (setq over gr-cleanup-save-max-spaces))
  (goto-char (point-min))
  (if gr-cleanup-compress-whitespace-fast
      (gr-compress-whitespace-fast-impl)
    (loop do (gr-compress-whitespace-line-impl over)
          while (= 0 (forward-line))
          )))

(defun gr-narrow-dwim-buffer ()
  (interactive)
  (if (region-active-p)
      (narrow-to-region (region-beginning) (region-end))
    (widen)))

(defun gr-narrow-dwim-line ()
  (interactive)
  (if (region-active-p)
      (narrow-to-region (region-beginning) (region-end))
    (narrow-to-region (point-at-bol) (point-at-eol))))

(defun gr-compress-whitespace-line (&optional over)
  "region if active, else line"
  (interactive)
  (save-excursion
    (save-restriction
      (gr-narrow-dwim-line)
      (gr-compress-whitespace-impl over))))

(defun gr-compress-whitespace-buffer (&optional over)
  "region if active, else buffer"
  (interactive)
  (message "compressing spaces...")
  (save-excursion
    (save-restriction
      (gr-narrow-dwim-buffer)
      (gr-compress-whitespace-impl over))))

;;\\|[?:]
(defconst gr-comma-regexp "[,;][^,@;) ]" "comma - space after")
(defconst gr-assign-regexp "\\([-+*/^|&]?=\\|||\\)" "assignment, || - space before and after")
(defconst gr-no-space-regexp "[^-+*/^|& =\"'><!:]" "not-space (and not-quote - substitute for visiting only text outside of strings). also hack to avoid separating == :: >= <= etc")
(defconst gr-access-spec "\\(public\\|private\\|\protected\\) :" "c++ access specifiers - no extra space before colon")
(defconst gr-cond-spec "\\(if\\|for\\|foreach\\|while\\)" "c++ conditional/loop")

(defun gr-what-face (pos)
  (interactive "d")
  (jit-lock-fontify-now pos (+ 500 pos))
  (or (get-char-property (point) 'read-face-name)
      (get-char-property (point) 'face)))

(defun gr-face-is-code (face)
  (not (or (eq face 'font-lock-string-face)
           (eq face 'font-lock-comment-face)
           (eq face 'font-lock-doc-face)
           (eq face 'font-lock-comment-delimiter-face)
           (eq face 'sh-heredoc)
           (eq face 'sh-quoted-exec)
           (equal face '(font-lock-regexp-grouping-construct font-lock-string-face))
           (equal face '(font-lock-regexp-grouping-backslash font-lock-string-face))
           (equal face '(font-lock-regexp-negation-char-face font-lock-string-face))
           )))

(defun gr-what-face-is-code (pos)
  (interactive "d")
  (gr-face-is-code (gr-what-face pos)))

(defun gr-what-face-msg (pos)
  (interactive "d")
  (let ((face (gr-what-face pos)))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun gr-what-face-is-code-msg (pos)
  (interactive "d")
  (message (if (gr-what-face-is-code pos) "CODE" "comment or string")))

;; not reliable - why?
(defun gr-force-fontify ()
  (interactive)
  (font-lock-fontify-buffer)
  ;;(jit-lock-refontify (point-min) (point-max))
  (jit-lock-fontify-now)
  (sleep-for 0 20)
  )

(defun gr-syntax-at-point ()
  (thing-at-point 'syntax))

(defun show-syntax ()
  (interactive)
  (message "syntax-at-point: %s" (gr-syntax-at-point)))

(defun gr-next-assignment-nospace ()
  (interactive)
  (when (re-search-forward (concat gr-no-space-regexp gr-assign-regexp gr-no-space-regexp) (point-max) t)
    (progn
      (goto-char (- (match-end 0) 1))
      t)))

(defun gr-replace-regexp (regexp to case-fold)
  (let ((case-fold-search case-fold))
    (while (re-search-forward regexp nil t)
      (replace-match to nil nil))))

(defun gr-space-operators-impl ()
  (interactive)
  (message (concat "space operators - after comma: " gr-comma-regexp ", before/after assignment=:" gr-assign-regexp " ..."))
  (let ((case-fold-search nil))
    (save-excursion
      (gr-force-fontify)
      (goto-char (point-min))
      ;;(replace-regexp "\\([^ \n]\\){" "\\1 {") ;; need to make this avoid string-constant font-face
      (goto-char (point-min))
      (gr-replace-regexp (concat "\\([ ]*" gr-cond-spec "\\)(") "\\1 (" nil)
      (goto-char (point-min))
      (while (re-search-forward (concat gr-comma-regexp) (point-max) t)
        (goto-char (- (match-end 0) 1))
        (when (gr-what-face-is-code (point))
          (insert " ")))
      (when (gr-cleanup-space-assign)
        (goto-char (point-min))
        (while (gr-next-assignment-nospace)
          (goto-char (- (match-end 0) 1))
          (when (gr-what-face-is-code (point))
            (insert " ")
            (goto-char (+ (match-beginning 0) 1))
            (insert " ")))
        )
      (goto-char (point-min))
      (while (re-search-forward gr-access-spec (point-max) t)
        (goto-char (- (match-end 0) 2))
        (when (gr-what-face-is-code (point))
          (delete-char 1)))
      (goto-char (point-min))
      (replace-regexp "^ {){" "{")
      )))

(defun gr-space-operators (&optional over)
  "region if active, else buffer"
  (interactive)
  (if (gr-skip-manual-compress-whitespace-p)
      (message (format "skipping gr-space-operators for mode %s" major-mode))
    (save-restriction
      (message "adding some spaces for code face ...")
      (gr-narrow-dwim-buffer)
      (gr-space-operators-impl))))

(defmacro gr-safe-wrap (&rest body)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,@body))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)))

(defun gr-cleanup-always ()
  (interactive)
  (save-excursion
    (gr-untabify-buffer)
    (gr-indent-buffer)
    (gr-compress-whitespace-buffer)
    (excessive-newlines-compress)
    (message "gr-cleanup done.")))

(defun gr-delete-trailing-whitespace-except-tab ()
  "Nuke all trailing whitespace in the buffer except tab."
  (interactive)
  (let ((bname (buffer-name)))
    (cond ((or
            (string= major-mode "rmail-mode")
            (string= bname "RMAIL")
            nil)); do nothing..

          (t
           (and (not buffer-read-only)
                (save-match-data
                  (save-excursion
                    (save-restriction
                      (widen)
                      (goto-char (point-min))
                      (while (re-search-forward "[ ]+$" (point-max) t)
                        (delete-region (match-beginning 0)
                                       (match-end 0)))))))))
    ;;(query-replace-regexp "[ \t]+$" "")))))))))

    ;; always return nil, in case this is on write-file-hooks.
    nil))

(defun gr-cleanup-buffer-impl ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (gr-safe-wrap
   (gr-indent-buffer-maybe)
   (gr-untabify-buffer)
   (unless (or (= 0 gr-cleanup-buffer-excessive-newlines) (eq nil gr-cleanup-buffer-excessive-newlines))
     (excessive-newlines-compress gr-cleanup-buffer-excessive-newlines))
   (gr-delete-trailing-whitespace-except-tab)
   (delete-trailing-newlines)
   (if (gr-cleanup-skip-compress-whitespace-p)
       (message (format "skipping whitespace compression for mode %s" major-mode))
     (gr-compress-whitespace-impl))
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
