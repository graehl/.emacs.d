;;; rename-sgml-tag.el --- Rename tag (including closing tag)
;;
;; Copyright (C) 2011 Magnar Sveen
;;
;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: html tags editing
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Rename the current tag (closest from point nesting-wise).
;;
;;     (require 'rename-sgml-tag)
;;     (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)
;;
;; This extension is dependent on the mark-multiple library.
;;     https://github.com/magnars/mark-multiple.el

;;; Code:

(require 'multiple-cursors)

;;;###autoload
(defalias 'rename-sgml-tag 'mc/mark-sgml-tag-pair)
(provide 'rename-sgml-tag)


;;; rename-sgml-tag.el ends here
