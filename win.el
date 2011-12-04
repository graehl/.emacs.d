(when nil
  (gnu-font-sz "Verdana" "8")
  (gnu-font "Axel")
  (gnu-font "Franklin Gothic Medium"))
(gnu-font "Consolas")

;; We don't know what this does but Brad swears it helps with NT
;;
(require 'comint)
(fset 'original-comint-exec-1 (symbol-function 'comint-exec-1))
(defun comint-exec-1 (name buffer command switches)
  (let ((binary-process-input t)
        (binary-process-output nil))
    (original-comint-exec-1 name buffer command switches)))
(setq comint-completion-addsuffix t)
;; (setq comint-process-echoes t) ;; reported that this is no longer needed
(setq comint-eol-on-send t)
(setq w32-quote-process-args ?\")

(defadvice find-file (after my-gud-translate-cygwin-paths activate)
  ;; if the file doesn't exist yet and is empty
  (if (and (equal (buffer-size) 0)
           (not (file-exists-p (buffer-file-name))))

      ;; try to find an initialization function
      (let ((initializer
             (find-if
              (lambda (pair) (string-match (car pair) (buffer-file-name)))
              my-buffer-initialization-alist)))

        ;; if found, call it
        (if initializer
            (progn (eval (list (cdr initializer)))
                   (set-buffer-modified-p nil))))))

(provide 'win)
