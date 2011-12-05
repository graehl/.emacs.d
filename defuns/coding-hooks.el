;; usage: (install-hook 'coding-hooks '(lambda () (message "hi"))) (replace-coding-hooks append)
;; which causes (run-coding-hooks) to run (append=t or nil) for all declared coding modes

(setq coding-hooks '())
(defun install-hook (h f &optional append local)
  (remove-hook h f local)
  (add-hook h f append local))

(defun install-hooks (hs f &optional append local)
  (loop for h in hs do (install-hook h f append local)))

(defun install-coding-hooks (&optional append local)
  (interactive)
  (loop for m in all-code-modes-hook do
        (loop for f in coding-hooks do
              (install-hook m f append local))))

(defun remove-coding-hooks (&optional local)
  (loop for m in all-code-modes-hook do
        (loop for f in coding-hooks do
              (remove-hook m f local))))

(defmacro set-coding-modes-sym-2 (MuNiQ HuNiQ &rest Usyms)
  `(progn
     (set ,MuNiQ (mapcar 'symbol-plus-mode ',Usyms))
     (set ,HuNiQ (mapcar 'symbol-plus-mode-hook ',Usyms))))

(defmacro set-coding-modes (uNiQcategory &rest Ynsyms)
  `(set-coding-modes-sym-2 (sym-dash-sym ,uNiQcategory 'modes) (sym-dash-sym ,uNiQcategory 'modes-hook) ,@Ynsyms))

(defun sym-dash-sym (x y) (intern (concat (symbol-name x) "-" (symbol-name y))))
(defun symbol-plus-mode (x) (sym-dash-sym x 'mode))
(defun symbol-plus-mode-hook (x) (sym-dash-sym x 'mode-hook))


(defun run-coding-hooks () (loop for h in coding-hooks do (funcall h)))

(setq all-code-modes nil)
(setq all-code-modes-hook '(c-mode-common-hook))

(defvar make-modes '(fundamental-mode))
