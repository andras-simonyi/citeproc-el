;; cpr-macro.el --- functions to render CSL macros -*- lexical-binding: t; -*-

;; Copyright (C) 2017 András Simonyi

;; Author: András Simonyi <andras.simonyi@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Functions to render the output of CSL macros.

;;; Code:

(require 'cpr-lib)
(require 'cpr-rt)
(require 'cpr-context)

;;; For macro evaluation
(defun cpr--macro (attrs context &rest body)
  "Render the content of a macro element with ATTRS and BODY."
  (setq body (cpr-lib-splice-into body 'splice))
  (let ((val (cpr-rt-typed-join attrs body context)))
    (if (eq 'empty-vars (cdr val))
	(cons nil 'text-only)
      val)))

(defun cpr-macro-output (macro context)
  "Return the output of MACRO.
MACRO is the macro's name as a string and the returned value is a
(RICH-TEXT-CONTENT . CONTENT-TYPE) cons cell."
  (let ((macro-fun (assoc-default macro (cpr-context-macros context))))
    (if macro-fun
	(funcall macro-fun context)
      (error "There is no macro called `%s' in style" macro))))

(defun cpr-macro-output-as-text (macro context)
  "Return the output of MACRO as plain text.
MACRO is the macro's name as a string."
  (cpr-rt-to-plain (cpr-rt-render-affixes
		   (car (cpr-macro-output macro context)))))

(provide 'cpr-macro)

;;; cpr-macro.el ends here
