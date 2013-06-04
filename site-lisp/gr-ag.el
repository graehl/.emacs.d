;;; gr-ag.el --- ag frontend derived from ack-and-a-half
;;
;; Copyright (C) 2013 Jonathan Graehl <graehl@gmail.com>
;; Jacob Helwig <jacob@technosorcery.net>
;; Alexey Lebedeff <binarin@binarin.ru>
;; Andrew Stine <stine.drew@gmail.com>
;; Derek Chen-Becker <derek@precog.com>
;; Gleb Peregud <gleber.p@gmail.com>
;; Kim van Wyk <vanwykk@gmail.com>
;; Ronaldo M. Ferraz <ronaldoferraz@gmail.com>
;; Ryan Thompson <rct@thompsonclan.org>
;;
;; Author: Jonathan Graehl <graehl@gmail.com>
;; Homepage: http://graehl.org/
;; Version: 20130603
;; X-Original-Version: 20130603
;; URL: https://github.com/graehl/gr-ag
;;
;; This file is NOT part of GNU Emacs.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is furnished to do
;; so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; gr-ag.el provides a simple compilation mode for the silver searcher (https://github.com/ggreer/the-silver-searcher/)
;; , which is a find-grep replacement which is faster than ack)  (http://petdance.com/ack/).
;;
;; Add the following to your .emacs:
;;
;;     (add-to-list 'load-path "/path/to/gr-ag")
;;     (require 'gr-ag)
;;     (defalias 'ack 'gr-ag)
;;     (defalias 'ack-same 'gr-ag-same)
;;     (defalias 'ack-find-file 'gr-ag-find-file)
;;     (defalias 'ack-find-file-same 'gr-ag-find-file-same)
;;
;; Run `ack' to search for all files and `ack-same' to search for
;; files of the same type as the current buffer.
;;
;; `next-error' and `previous-error' can be used to jump to the
;; matches.
;;
;; `ack-find-file' and `ack-find-same-file' use ack to list the files
;; in the current project.  It's a convenient, though slow, way of
;; finding files.
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'compile)
(require 'grep)
(require 'thingatpt)

(add-to-list 'debug-ignored-errors
             "^Moved \\(back before fir\\|past la\\)st match$")
(add-to-list 'debug-ignored-errors "^File .* not found$")

(define-compilation-mode gr-ag-mode "Ack"
  "Ack results compilation mode."
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'compilation-disable-input) t)
  (let ((smbl  'compilation-ack-nogroup)
        (pttrn '("^\\([^:\n]+?\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)))
    (set (make-local-variable 'compilation-error-regexp-alist) (list smbl))
    (set (make-local-variable 'compilation-error-regexp-alist-alist) (list (cons smbl pttrn))))
  (set (make-local-variable 'compilation-process-setup-function) 'gr-ag-mode-setup)
  (set (make-local-variable 'compilation-error-face) grep-hit-face))

(defgroup gr-ag nil "Yet another front end for ack."
  :group 'tools
  :group 'matching)

(defcustom gr-ag-executable (or (executable-find "ack")
                                         (executable-find "ack-grep"))
  "*The location of the ack executable."
  :group 'gr-ag
  :type 'file)

(defcustom gr-ag-buffer-name "*gr-ag*"
  "*The name of the gr-ag buffer."
  :group 'gr-ag
  :type 'string)

(defun ack-buffer-name (mode) gr-ag-buffer-name)

(defcustom gr-ag-arguments nil
  "*Extra arguments to pass to ack."
  :group 'gr-ag
  :type '(repeat (string)))

(defcustom gr-ag-mode-type-alist nil
  "*File type(s) to search per major mode.  (gr-ag-same)
This overrides values in `gr-ag-mode-type-default-alist'.
The car in each list element is a major mode, and the rest
is a list of strings passed to the --type flag of ack when running
`gr-ag-same'."
  :group 'gr-ag
  :type '(repeat (cons (symbol :tag "Major mode")
                       (repeat (string :tag "ack --type")))))

(defcustom gr-ag-mode-extension-alist nil
  "*File extensions to search per major mode.  (gr-ag-same)
This overrides values in `gr-ag-mode-extension-default-alist'.
The car in each list element is a major mode, and the rest
is a list of file extensions to be searched in addition to
the type defined in `gr-ag-mode-type-alist' when
running `gr-ag-same'."
  :group 'gr-ag
  :type '(repeat (cons (symbol :tag "Major mode")
                       (repeat :tag "File extensions" (string)))))

(defcustom gr-ag-ignore-case 'smart
  "*Whether or not to ignore case when searching.
The special value 'smart enables the ack option \"smart-case\"."
  :group 'gr-ag
  :type '(choice (const :tag "Case sensitive" nil)
                 (const :tag "Smart case" 'smart)
                 (const :tag "Case insensitive" t)))

(defcustom gr-ag-regexp-search t
  "*Default to regular expression searching.
Giving a prefix argument to `gr-ag' toggles this option."
  :group 'gr-ag
  :type '(choice (const :tag "Literal searching" nil)
                 (const :tag "Regular expression searching" t)))

(defcustom gr-ag-use-environment t
  "*Use .ackrc and ACK_OPTIONS when searching."
  :group 'gr-ag
  :type '(choice (const :tag "Ignore environment" nil)
                 (const :tag "Use environment" t)))

(defcustom gr-ag-root-directory-functions '(gr-ag-guess-project-root)
  "*List of functions used to find the base directory to ack from.
These functions are called until one returns a directory.  If successful,
`gr-ag' is run from that directory instead of from `default-directory'.
The directory is verified by the user depending on `gr-ag-prompt-for-directory'."
  :group 'gr-ag
  :type '(repeat function))

(defcustom gr-ag-project-root-file-patterns
  '(".project\\'"
    ".xcodeproj\\'"
    ".sln\\'"
    "\\`Project.ede\\'"
    "\\`.git\\'"
    "\\`.bzr\\'"
    "\\`_darcs\\'"
    "\\`.hg\\'")
  "*List of file patterns for the project root (used by `gr-ag-guess-project-root').
Each element is a regular expression.  If a file matching any element is
found in a directory, then that directory is assumed to be the project
root by `gr-ag-guess-project-root'."
  :group 'gr-ag
  :type '(repeat (string :tag "Regular expression")))

(defcustom gr-ag-prompt-for-directory 'unless-guessed
  "*Prompt for directory in which to run ack.
If this is 'unless-guessed, then the value determined by
`gr-ag-root-directory-functions' is used without
confirmation.  If it is nil, then the directory is never
confirmed.  If t, then always prompt for the directory to use."
  :group 'gr-ag
  :type '(choice (const :tag "Don't prompt" nil)
                 (const :tag "Don't prompt when guessed" unless-guessed)
                 (const :tag "Always prompt" t)))

;;; Default setting lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst gr-ag-mode-type-default-alist
  '((actionscript-mode "actionscript")
    (LaTeX-mode "tex")
    (TeX-mode "tex")
    (asm-mode "asm")
    (batch-file-mode "batch")
    (c++-mode "cpp")
    (c-mode "cc")
    (cfmx-mode "cfmx")
    (cperl-mode "perl")
    (csharp-mode "csharp")
    (css-mode "css")
    (emacs-lisp-mode "elisp")
    (erlang-mode "erlang")
    (espresso-mode "java")
    (fortran-mode "fortran")
    (haskell-mode "haskell")
    (hexl-mode "binary")
    (html-mode "html")
    (java-mode "java")
    (javascript-mode "js")
    (jde-mode "java")
    (js2-mode "js")
    (jsp-mode "jsp")
    (latex-mode "tex")
    (lisp-mode "lisp")
    (lua-mode "lua")
    (makefile-mode "make")
    (mason-mode "mason")
    (nxml-mode "xml")
    (objc-mode "objc" "objcpp")
    (ocaml-mode "ocaml")
    (parrot-mode "parrot")
    (perl-mode "perl")
    (php-mode "php")
    (plone-mode "plone")
    (python-mode "python")
    (ruby-mode "ruby")
    (scala-mode "scala")
    (scheme-mode "scheme")
    (shell-script-mode "shell")
    (skipped-mode "skipped")
    (smalltalk-mode "smalltalk")
    (sql-mode "sql")
    (tcl-mode "tcl")
    (tex-mode "tex")
    (tt-mode "tt")
    (vb-mode "vb")
    (vim-mode "vim")
    (xml-mode "xml")
    (yaml-mode "yaml"))
  "Default values for `gr-ag-mode-type-alist'.")

(defconst gr-ag-mode-extension-default-alist
  '((d-mode "d"))
  "Default values for `gr-ag-mode-extension-alist'.")

(defun gr-ag-create-type (extensions)
  (list "--type-set"
        (concat "gr-ag-custom-type=" (mapconcat 'identity extensions ","))
        "--type" "gr-ag-custom-type"))

(defun gr-ag-type-for-major-mode (mode)
  "Return the --type and --type-set arguments to use with ack for major mode MODE."
  (let ((types (cdr (or (assoc mode gr-ag-mode-type-alist)
                        (assoc mode gr-ag-mode-type-default-alist))))
        (ext (cdr (or (assoc mode gr-ag-mode-extension-alist)
                      (assoc mode gr-ag-mode-extension-default-alist))))
        result)
    (dolist (type types)
      (push type result)
      (push "--type" result))
    (if ext
        (if types
            `("--type-add" ,(concat (car types)
                                    "=" (mapconcat 'identity ext ","))
              . ,result)
          (gr-ag-create-type ext))
      result)))

;;; Project root ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gr-ag-guess-project-root ()
  "Guess the project root directory.
This is intended to be used in `gr-ag-root-directory-functions'."
  (catch 'root
    (let ((dir (expand-file-name (if buffer-file-name
                                     (file-name-directory buffer-file-name)
                                   default-directory)))
          (pattern (mapconcat 'identity gr-ag-project-root-file-patterns "\\|")))
      (while (not (equal dir "/"))
        (when (directory-files dir nil pattern t)
          (throw 'root dir))
        (setq dir (file-name-directory (directory-file-name dir)))))))

;;; Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gr-ag-directory-history nil
  "Directories recently searched with `gr-ag'.")
(defvar gr-ag-literal-history nil
  "Strings recently searched for with `gr-ag'.")
(defvar gr-ag-regexp-history nil
  "Regular expressions recently searched for with `gr-ag'.")

(defun gr-ag-initial-contents-for-read ()
  (when (gr-ag-use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun gr-ag-default-for-read ()
  (unless (gr-ag-use-region-p)
    (thing-at-point 'symbol)))

(defun gr-ag-use-region-p ()
  (or (and (fboundp 'use-region-p) (use-region-p))
      (and transient-mark-mode mark-active
           (> (region-end) (region-beginning)))))

(defsubst gr-ag-read (regexp)
  (let* ((default (gr-ag-default-for-read))
         (type (if regexp "pattern" "literal search"))
         (history-var )
         (prompt  (if default
                      (format "ack %s (default %s): " type default)
                    (format "ack %s: " type))))
    (read-string prompt
                 (gr-ag-initial-contents-for-read)
                 (if regexp 'ack-regexp-history 'ack-literal-history)
                 default)))

(defun gr-ag-read-dir ()
  (let ((dir (run-hook-with-args-until-success 'gr-ag-root-directory-functions)))
    (if gr-ag-prompt-for-directory
        (if (and dir (eq gr-ag-prompt-for-directory 'unless-guessed))
            dir
          (read-directory-name "Directory: " dir dir t))
      (or dir
          (and buffer-file-name (file-name-directory buffer-file-name))
          default-directory))))

(defsubst gr-ag-xor (a b)
  (if a (not b) b))

(defun gr-ag-interactive ()
  "Return the (interactive) arguments for `gr-ag' and `gr-ag-same'."
  (let ((regexp (gr-ag-xor current-prefix-arg gr-ag-regexp-search)))
    (list (gr-ag-read regexp)
          regexp
          (gr-ag-read-dir))))

(defun gr-ag-type ()
  (or (gr-ag-type-for-major-mode major-mode)
      (when buffer-file-name
        (gr-ag-create-type (list (file-name-extension buffer-file-name))))))

(defun gr-ag-option (name enabled)
  (format "--%s%s" (if enabled "" "no") name))

(defun gr-ag-arguments-from-options (regexp)
  (let ((arguments (list "--nocolor" "--nogroup" "--column"
                         (gr-ag-option "smart-case" (eq gr-ag-ignore-case 'smart))
                         (gr-ag-option "env" gr-ag-use-environment)
                         )))
    (unless gr-ag-ignore-case
      (push "-i" arguments))
    (unless regexp
      (push "--literal" arguments))
    arguments))

(defun gr-ag-string-replace (from to string &optional re)
  "Replace all occurrences of FROM with TO in STRING.
All arguments are strings.  When optional fourth argument (RE) is
non-nil, treat FROM as a regular expression."
  (let ((pos 0)
        (res "")
        (from (if re from (regexp-quote from))))
    (while (< pos (length string))
      (if (setq beg (string-match from string pos))
          (progn
            (setq res (concat res
                              (substring string pos (match-beginning 0))
                              to))
            (setq pos (match-end 0)))
        (progn
          (setq res (concat res (substring string pos (length string))))
          (setq pos (length string)))))
    res))

(defun gr-ag-run (directory regexp pattern &rest arguments)
  "Run ack in DIRECTORY with ARGUMENTS."
  (let ((default-directory (if directory
                               (file-name-as-directory (expand-file-name directory))
                             default-directory)))
    (setq arguments (append gr-ag-arguments
                            (gr-ag-arguments-from-options regexp)
                            arguments
                            (list "--")
                            (list (shell-quote-argument pattern))
                            ))
    (make-local-variable 'compilation-buffer-name-function)
    (let (compilation-buffer-name-function)
      (setq compilation-buffer-name-function 'ack-buffer-name)
      (compilation-start (mapconcat 'identity (nconc (list gr-ag-executable) arguments) " ")
                         'gr-ag-mode))))

(defun gr-ag-read-file (prompt choices)
  (if ido-mode
      (ido-completing-read prompt choices nil t)
    (require 'iswitchb)
    (with-no-warnings
      (let ((iswitchb-make-buflist-hook
             (lambda () (setq iswitchb-temp-buflist choices))))
        (iswitchb-read-buffer prompt nil t)))))

(defun gr-ag-list-files (directory &rest arguments)
  (with-temp-buffer
    (let ((default-directory directory))
      (when (eq 0 (apply 'call-process gr-ag-executable nil t nil "-f" "--print0"
                         arguments))
        (goto-char (point-min))
        (let ((beg (point-min))
              files)
          (while (re-search-forward "\0" nil t)
            (push (buffer-substring beg (match-beginning 0)) files)
            (setq beg (match-end 0)))
          files)))))

(defun gr-ag-version-string ()
  "Return the ack version string."
  (with-temp-buffer
    (call-process gr-ag-executable nil t nil "--version")
    (goto-char (point-min))
    (re-search-forward " +")
    (buffer-substring (point) (point-at-eol))))

(defun gr-ag-mode-setup ()
  "Setup compilation variables and buffer for `gr-ag'.
Set up `compilation-exit-message-function'."
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
         (if (eq status 'exit)
             (cond ((and (zerop code) (buffer-modified-p))
                    '("finished (matches found)\n" . "matched"))
                   ((not (buffer-modified-p))
                    '("finished with no matches found\n" . "no match"))
                   (t
                    (cons msg code)))
           (cons msg code)))))

;;; Public interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun gr-ag (pattern &optional regexp directory)
  "Run ack.
PATTERN is interpreted as a regular expression, iff REGEXP is
non-nil.  If called interactively, the value of REGEXP is
determined by `gr-ag-regexp-search'.  A prefix arg
toggles the behavior.  DIRECTORY is the root directory.  If
called interactively, it is determined by
`gr-ag-project-root-file-patterns'.  The user is only
prompted, if `gr-ag-prompt-for-directory' is set."
  (interactive (gr-ag-interactive))
  (gr-ag-run directory regexp pattern))

;;;###autoload
(defun gr-ag-same (pattern &optional regexp directory)
  "Run ack with --type matching the current `major-mode'.
The types of files searched are determined by
`gr-ag-mode-type-alist' and
`gr-ag-mode-extension-alist'.  If no type is configured,
the buffer's file extension is used for the search.  PATTERN is
interpreted as a regular expression, iff REGEXP is non-nil.  If
called interactively, the value of REGEXP is determined by
`gr-ag-regexp-search'.  A prefix arg toggles that value.
DIRECTORY is the directory in which to start searching.  If
called interactively, it is determined by
`gr-ag-project-root-file-patterns`.  The user is only
prompted, if `gr-ag-prompt-for-directory' is set.`"
  (interactive (gr-ag-interactive))
  (let ((type (gr-ag-type)))
    (if type
        (apply 'gr-ag-run directory regexp pattern type)
      (gr-ag pattern regexp directory))))

;;;###autoload
(defun gr-ag-find-file (&optional directory)
  "Prompt to find a file found by ack in DIRECTORY."
  (interactive (list (gr-ag-read-dir)))
  (find-file (expand-file-name
              (gr-ag-read-file
               "Find file: "
               (gr-ag-list-files directory))
              directory)))

;;;###autoload
(defun gr-ag-find-file-same (&optional directory)
  "Prompt to find a file found by ack in DIRECTORY."
  (interactive (list (gr-ag-read-dir)))
  (find-file (expand-file-name
              (gr-ag-read-file
               "Find file: "
               (apply 'gr-ag-list-files directory (gr-ag-type)))
              directory)))

;;; End gr-ag.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'gr-ag)

;;; gr-ag.el ends here
