

(defun my-convert-html-literals ()
  "convert special characters in the region as follows:

   \"&\" => \"&amp;\"     \"<\" => \"&lt;\"      \">\" => \"&gt;\"    \"\\\"\" => \"&quot;\"
This makes a region of source code appear correctly in an HTML file."
  (interactive)
  (save-excursion
    (let ((start (car (my-selection)))
          (finish (cdr (my-selection))))
      (my-replace-in-region start finish "&" "&amp;")
      (my-replace-in-region start finish "<" "&lt;")
      (my-replace-in-region start finish ">" "&gt;")
      (my-replace-in-region start finish "\"" "&quot;")
      )))

(defun my-code-tag ()
  "Surround the region with <code>...</code>, formatting it as code."
  (interactive)
  (sgml-tag "code"))

(setq sgml-entity-insert-case 'lower)

(defun my-sgml-validate-writeback ()
  (interactive)
  (let* ((file (buffer-file-name))
         (cmd
          (format "tidy -m -i -wrap 78 --force-output 1 --keep-time 0 --gnu-emacs 1  %s" file) ))

    (save-some-buffers (not compilation-ask-about-save) nil)
                                        ;compile-internal
    (compilation-start cmd "No more errors")
                                        ;  (sgml-validate cmd)
    ))

(defun my-convert-html-literals ()
  "convert special characters in the region as follows:

   \"&\" => \"&amp;\"     \"<\" => \"&lt;\"      \">\" => \"&gt;\"    \"\\\"\" => \"&quot;\"
This makes a region of source code appear correctly in an HTML file."
  (interactive)
  (save-excursion
    (let ((start (car (my-selection)))
          (finish (cdr (my-selection))))
      (my-replace-in-region start finish "&" "&amp;")
      (my-replace-in-region start finish "<" "&lt;")
      (my-replace-in-region start finish ">" "&gt;")
      (my-replace-in-region start finish "\"" "&quot;")
      )))

(defun my-preformatted ()
  "Surround the region with <pre>...</pre> and convert
special characters contained within as follows:

   \"&\" => \"&amp;\"     \"<\" => \"&lt;\"      \">\" => \"&gt;\"    \"\\\"\" => \"&quot;\"
This makes a region of source code appear correctly in an HTML file."
  (interactive)
  (my-convert-html-literals)
  (sgml-tag "pre")
  )

(defun my-yank-code ()
  "Yank whatever was last killed, add HTML formatting as blockquoted,
preformatted text, and translate the special characters \"<\\\">&\" to their HTML
equivalents."
  (interactive)
  (yank)
  (my-activate-mark)
  (my-convert-html-literals))


; I don't use psgml - investiate if this is worth anything:

;; don't insert newlines around <code>...</code> tags
;;; Set up PSGML
                                        ; Add PSGML to load-path so Emacs can find it.
                                        ; Note the forward slashes in the path... this is platform-independent so I
                                        ; would suggest using them over back slashes. If you use back slashes, they
                                        ; MUST BE doubled, as Emacs treats backslash as an escape character.
                                        ;(setq load-path (cons "~/elisp/psgml-1.3.1/" load-path))
                                        ; Use PSGML for sgml and xml major modes.
(when nil
  (condition-case nil
      (progn
        (require 'psgml)
        (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
        (autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
                                        ; override default validate command to utilize OpenSP's onsgmls executable
        (setq sgml-validate-command "onsgmls -s %s %s")
        (add-to-list 'sgml-catalog-files "~/dtd/CATALOG")

                                        ; override default xml-mode validate command to utilize OpenSP's onsgmls
                                        ; executable by using a mode-hook, since there appears to be no other means
                                        ; to accomplish it.
        (defun my-psgml-xml-hook ()
          (setq sgml-validate-command "onsgmls -s %s %s")
          (setq sgml-declaration "c:/cygwin/usr/local/lib/sgml/dtd/html/xml.dcl")
          )
        (add-hook 'xml-mode-hook 'my-psgml-xml-hook)

        (defun my-sgml-electric-less-than (&optional arg)
          (interactive "*P")
          (if arg (insert "<")
            (call-interactively
             (if (is-mark-active)  'sgml-tag-region 'sgml-insert-element))
            ))

        (defun my-psgml-hook ()
                                        ; From Lennart Staflin - re-enabling launch of browser (from original HTML mode)
          (local-set-key "\C-c\C-b" 'browse-url-of-buffer)
          (local-set-key "<" 'my-sgml-electric-less-than)
          )

        (add-hook 'sgml-mode-hook 'my-psgml-hook)

                                        ; PSGML - enable face settings
        (setq-default sgml-set-face t)

                                        ; Auto-activate parsing the DTD when a document is loaded.
                                        ; If this isn't enabled, syntax coloring won't take affect until
                                        ; you manually invoke "DTD->Parse DTD"
        (setq-default sgml-auto-activate-dtd t)
        )
    (error
     (or recent-emacs (progn (require 'sgml-mode)
                             (setq html-tag-alist
                                   (append
                                    '( ("code"))
                                    html-tag-alist)
                                   )))
     )
    )
  )
