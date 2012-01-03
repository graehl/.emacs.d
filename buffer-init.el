(defcustom my-buffer-initialization-alist
  '(
    ("\\.[ih]\\(pp\\|xx\\)?$" . my-begin-header)
    ("\\.c\\(pp\\|xx\\)$" . my-begin-source)
    )
  "A list of pairs (PATTERN . FUNCTION) describing how to initialize an empty buffer whose
file name begins matches PATTERN."
  ':type 'alist
  )

(setq my-initials "LW")

(defun boost-copyright ()
  "Return the appropriate boost copyright for the current user and year"
  (concat "Copyright " (user-full-name) " " (number-to-string (nth 5 (decode-time)))
          ". Distributed under the Boost\n\
Software License, Version 1.0. (See accompanying\n\
file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)"))

(defun lw-copyright ()
  "Return the appropriate boost copyright for the current user and year"
  "")

(defun sdl-copyright ()
  "Return the appropriate copyright for the current user and year"
  (concat "Copyright SDL " (number-to-string (nth 5 (decode-time)))
          ". All rights reserved."))

(defcustom my-namespace-roots
  '(("boost". boost-copyright) ("fluid" . fluid-copyright) ("x" . lw-copyright))
  "An alist of root directory names and associated copyright
      functions from which to deduce C++ namespace names."
  ':type 'alist )

(defun my-filter-path-elts (pe) pe)

  ;; (if (equal (car pe) "racerx")
  ;;     (cons "LW" (remove "include" (cdr pe)))
  ;;   pe))

(provide 'buffer-init)
