;; Since pretty much all my .h files are actually C++ headers, use c++-mode instead of
;; c-mode for these files.
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))

(add-to-list 'auto-mode-alist '("\\.ipp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . c++-mode)) ; e.g. boost

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))
;;(add-to-list 'auto-mode-alist '("\\.fs\\w?" . tuareg-mode))

;; YAML
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("jsTestDriver\\.conf$" . yaml-mode))

;; Jade and Stylus (sws = significant whitespace)
(add-to-list 'load-path "~/.emacs.d/site-lisp/jade-mode")
(autoload 'sws-mode "sws-mode")
(autoload 'jade-mode "jade-mode")
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.jspf$" . html-mode))
(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

;; Ruby
(autoload 'rhtml-mode "rhtml-mode")
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.watchr$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; JavaScript
(autoload 'js2-mode "setup-js2-mode")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))

;; Oppdrag
(require 'oppdrag-mode)

(add-hook 'find-file-hook
          (lambda ()
            (let* ((file (buffer-file-name))
                   (len (length file)))
              (if (string-match-p "oppdrag-services" file) (oppdrag-mode)))))


;; Buster.JS
                                        ;(autoload 'buster-mode "buster-mode")
                                        ;(setq buster-node-executable "/usr/local/bin/node")
                                        ;(add-file-find-hook-with-pattern "test\\.js$" (lambda () (buster-mode)) "require(\\(\"\\|'\\)buster")

;; Markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; PHP
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Apache config
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(provide 'mode-mappings)
