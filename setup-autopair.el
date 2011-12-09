;; Autopair () {} <> "" '' etc

(require 'autopair)
(autopair-global-mode)
(setq autopair-blink nil)

(defun autopair-dont ()
  (interactive)
  (setq autopair-dont-activate t))

(add-hook 'emacs-lisp-mode-hook 'autopair-dont)
(add-hook 'lisp-interaction-mode 'autopair-dont)

(require 'setup-code-modes)
(install-hooks lisp-modes-hook 'autopair-dont)

(set-default 'autopair-dont-activate #'(lambda ()
                                         (eq major-mode 'term-mode)))

(setq autopair-autowrap t)

(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))


(add-hook 'c++-mode-hook
          #'(lambda ()
              (push ?{
                    (getf autopair-dont-pair :comment))))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (push '(?< . ?>)
                    (getf autopair-extra-pairs :code))))

(defun autopair-paren () (push '(?\( . ?\)) (getf autopair-extra-pairs :code)))

(add-hook 'latex-mode-hook
          #'(lambda ()
              (set (make-local-variable 'autopair-handle-action-fns)
                   (list #'autopair-default-handle-action
                         #'autopair-latex-mode-paired-delimiter-action))))

(provide 'setup-autopair)
