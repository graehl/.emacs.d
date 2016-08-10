(require 'auto-complete)
(require 'yasnippet)
(require 'flymake)

(defun ac-yasnippet-candidate ()
  (let ((table (yas/get-snippet-tables major-mode)))
    (if table
      (let (candidates (list))
            (mapcar (lambda (mode)
              (maphash (lambda (key value)
                (push key candidates))
              (yas/snippet-table-hash mode)))
            table)
        (all-completions ac-prefix candidates)))))

(defface ac-yasnippet-candidate-face
  '((t (:background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate.")

(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white")))
  "Face for the yasnippet selected candidate.")

(defvar ac-source-yasnippet
  '((candidates . ac-yasnippet-candidate)
    (action . yas/expand)
    (limit . 3)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face))
  "Source for Yasnippet.")

(require 'auto-complete-clang-async)
(setq gr-ac-clang-cflags '("-I.." "-I../.." "-I../include"))
(defun kimim/c-mode-ac-complete()
  (global-auto-complete-mode t)
  (setq ac-clang-complete-executable "clang-complete")
  (add-to-list 'ac-sources 'ac-source-clang-async)
  (if ac-clang-cflags
      (setq ac-clang-cflags (cons ac-clang-cflags gr-ac-clang-cflags))
    (setq ac-clang-cflags gr-ac-clang-cflags))
  (ac-clang-launch-completion-process)
  (ac-clang-update-cmdlineargs))

;;(add-hook 'c-mode-common-hook (lambda () (kimim/c-mode-ac-complete)))

(provide 'setup-ac-yasnippet)
