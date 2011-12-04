(when nil
  (gnu-font-sz "Verdana" "8")
  (gnu-font "Axel")
  (gnu-font "Franklin Gothic Medium"))
(gnu-font-sz "Consolas" "10")

;; We don't know what this does but Brad swears it helps with NT
;;
(require 'comint)
(fset 'original-comint-exec-1 (symbol-function 'comint-exec-1))
(defun comint-exec-1 (name buffer command switches)
  (let ((binary-process-input t)
        (binary-process-output nil))
    (original-comint-exec-1 name buffer command switches)))
(setq comint-completion-addsuffix t)

(setq comint-scroll-show-maximum-output 'this)
;; (setq comint-process-echoes t) ;; reported that this is no longer needed
(setq comint-eol-on-send t)
                                        ;          (setq w32-quote-process-args ?\")
                                        ;(make-variable-buffer-local 'comint-completion-addsuffix) ; shell-mode-hook
;; (setq comint-process-echoes t) ;; reported that this is no longer needed
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

;;
;; Path translation for cygwin
;;
(defun my-translate-cygwin-paths (file)
  "Adjust paths generated by cygwin so that they can be opened by tools running under emacs."

  ;; If it's not a windows system, or the file doesn't begin with /, don't do any filtering
  (if (and (eq system-type 'windows-nt) (string-match "^/" file))

      ;; Replace paths of the form /cygdrive/c/... or //c/... with c:/...
      (if (string-match "^\\(//\\|/cygdrive/\\)\\([a-zA-Z]\\)/" file)
          (setq file (file-truename (replace-match "\\2:/" t nil file)))

        ;; ELSE
        ;; Replace names of the form /... with <cygnus installation>/...
        ;; try to find the cygwin installation
        (let ((paths (parse-colon-path (getenv "path"))) ; Get $(PATH) from the environment
              (found nil))

          ;; While there are unprocessed paths and cygwin is not found
          (while (and (not found) paths)
            (let ((path (car paths))) ; grab the first path
              (setq paths (cdr paths)) ; walk down the list
              (if (and (string-match "/bin/?$" path) ; if it ends with /bin
                       (file-exists-p                ; and cygwin.bat is in the parent
                        (concat
                         (if (string-match "/$" path) path (concat path "/"))
                         "../cygwin.bat")))
                  (progn
                    (setq found t) ; done looping
                    (string-match "^\\(.*\\)/bin/?$" path)
                    (setq file (file-truename (concat (match-string 1 path) file))))
                ))))))
  file)

                                        ;(defadvice gud-find-file (before my-gud-translate-cygwin-paths activate)  (ad-set-arg 0 (my-translate-cygwin-paths (ad-get-arg 0)) t))
                                        ;(defadvice compilation-find-file (before my-compilation-translate-cygwin-paths activate)  (ad-set-arg 1 (my-translate-cygwin-paths (ad-get-arg 1)) t))


                                        ;(size-frame 200 40)

(default-size-frame 160 45)
(provide 'win)
(setq win32-tempdir "c:/windows/temp")


(defun add-PATH (d) "" (interactive)
  (add-to-list 'exec-path d)
  (setenv "PATH" (concat (my-backward-slashes d) ";" (getenv "PATH"))))

(defun my-backward-slashes (filename)
  "convert each slash in filename to a backward slash"
  (concat (mapcar (function (lambda (c)
                              (if (= c ?/) ?\\ c)))
                  filename)))
(require 'setup-cygwin)
