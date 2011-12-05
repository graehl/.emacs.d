;;; visual line mode nested indent wrapping.
;;; (gr-adaptive-wrap-global-mode)
;;; author: Jonathan Graehl (graehl@gmail.com)
;;; via Stephen Berman (srb):
;;; http://lists.gnu.org/archive/html/help-gnu-emacs/2009-09/msg00348.html

(defvar gr-adaptive-wrap-no-fringe nil "if t, don't display fringe continuation glyph")

(defvar gr-adaptive-wrap-except-modes '(calc-mode dired-mode)
  "A list of modes in which `gr-adaptive-wrap-mode' should not be activated.")

(defvar gr-adaptive-wrap-hook nil
  "Called when `gr-adaptive-wrap-mode' is turned on.")

(defun adaptive-indent (beg end)
  "Indent the region between BEG and END with adaptive filling."
  (goto-char beg)
  (while
      (let ((lbp (line-beginning-position))
            (lep (line-end-position)))
        (put-text-property lbp lep 'wrap-prefix (fill-context-prefix lbp lep))
        (search-forward "\n" end t))))

(defun gr-adaptive-wrap-perform ()
  (save-excursion
    (save-restriction
      (widen)
      (let ((buffer-undo-list t)
            (inhibit-read-only t)
            (mod (buffer-modified-p)))
        (if gr-adaptive-wrap-mode
            (progn
              (when gr-adaptive-wrap-no-fringe
                (setq word-wrap t)
                (unless (member '(continuation) fringe-indicator-alist)
                  (push '(continuation) fringe-indicator-alist)))
              (jit-lock-register 'adaptive-indent))
          (jit-lock-unregister 'adaptive-indent)
          (remove-text-properties (point-min) (point-max) '(wrap-prefix pref))
          (when gr-adaptive-wrap-no-fringe
            (setq fringe-indicator-alist
                  (delete '(continuation) fringe-indicator-alist))
            (setq word-wrap nil)))
        (restore-buffer-modified-p mod)))))


;;;###autoload
(defun turn-on-gr-adaptive-wrap-mode ()
  "Turn on `gr-adaptive-wrap-mode'"
  (interactive)
  (unless (member major-mode gr-adaptive-wrap-except-modes)
    (gr-adaptive-wrap-hook)
    (gr-adaptive-wrap-mode +1)))

;;;###autoload
(defun turn-off-gr-adaptive-wrap-mode ()
  "Turn off `gr-adaptive-wrap-mode'"
  (interactive)
  (unless (member major-mode gr-adaptive-wrap-except-modes)
    (gr-adaptive-wrap-mode -1)))

;;;###autoload
(define-globalized-minor-mode gr-adaptive-wrap-global-mode
  gr-adaptive-wrap-mode
  turn-on-gr-adaptive-wrap-mode)

(defvar gr-adaptive-wrap-except-modes '(calc-mode dired-mode)
  "A list of modes in which `gr-adaptive-wrap-mode' should not be activated.")

;;;###autoload
(define-minor-mode gr-adaptive-wrap-mode
  "Wrap the buffer text with adaptive filling."
  :init-value nil
  :lighter " aw"
  (when gr-adaptive-wrap-mode
    (gr-adaptive-wrap-perform)
    (run-hooks 'gr-adaptive-wrap-hook)))

(provide 'gr-adaptive-wrap)
