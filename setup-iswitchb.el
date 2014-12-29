(require 'setup-ag)
(require 'iswitchb)
(defun iswitchb-ag-cd ()
  (interactive)
  (let ((match iswitchb-common-match-string))
    (iswitchb-exit-minibuffer)
    (gr-ag-cd iswitchb-common-match-string)
  ))

      (when t
;; awesome: adds arrow keys to iswitchb
  (require 'edmacro)
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             )
          ("<f9>" . iswitchb-ag-cd)
          )))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
;; /awesome
(iswitchb-mode 1)
)

(icomplete-mode 99)
(require 'icomplete+)
(setq ido-max-directory-size 100000)


(require 'ido)
;; (ido-mode 'buffers) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
                           "*Messages*" "Async Shell Command"))

(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

(provide 'setup-iswitchb)
