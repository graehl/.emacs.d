(require 'compile-)
(catch 'found
  (dolist (d (mapcar 'file-name-as-directory load-path))
    (let* ((f (expand-file-name (concat d "compile")))
           (regexp (concat (mapconcat 'regexp-quote (get-load-suffixes) "\\|") "$"))
           (seek (car (split-string load-file-name regexp))))
      (and (not (string= f seek))
           (require 'compile f 'noerror)
           (throw 'found nil)))))
