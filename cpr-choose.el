;; cpr-choose.el --- conditionally rendered CSL elements -*- lexical-binding: t; -*-

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

;; CSL supports conditional rendering via the cs:choose rendering element, which
;; can contain cs:if, cs:else-if and cs:else elements as children. This file
;; contains functions corresponding to all these elements with some auxiliary
;; functions.

;; In order to support conditional rendering by cs:choose, the functions
;; corresponding to cs:if, cs:else-if and cs:else (`cpr--if', `cpr--else-if',
;; `cpr--else') return the generalized boolean value of their condition in
;; addition to their rendered content in the form of a (BOOLEAN . CONTENT) pair.

;;; Code:

(require 'cpr-lib)

(require 'cpr-context)

(defun cpr-choose-eval-conditions (attrs context)
  "Eval (possibly complex) boolean conditions in ATTRS."
  (-let* ((conditions (cpr-choose--elementary-conditions attrs))
	  (match (intern (or (alist-get 'match attrs) "all")))
	  (values (--mapcat (cpr-choose--eval-elementary-condition (car it)
								   (intern (cdr it))
								   context)
			    conditions)))
    (pcase match
      ('all (--all? it values))
      ('any (--any? it values))
      ('none (--none? it values)))))

(defun cpr-choose--elementary-conditions (attrs)
  "Expand complex conditions in ATTRS into elementary ones.
Return a list of elementary (CONDITION-TYPE . PARAM) pairs."
  (cl-mapcan (lambda (x)
	       (--map (cons (car x) it)
		      (s-split " " (cdr x))))
	     attrs))

(defun cpr-choose--eval-elementary-condition (type param context)
  "Evaluate an elementary choose condition of TYPE with PARAM.
TYPE is one of the symbols 'variable, 'type, 'locator,
'is-numeric, 'is-uncertain-date, 'match, 'position,
'disambiguate. Return a list containing the result of evaluation,
which is a generalized boolean, or nil if TYPE is 'match."
  (if (eq type 'match) nil
    (list
     (pcase type
       ('variable (cpr-var-value param context))
       ('type (string= param (cpr-var-value 'type context)))
       ('locator (string= param (cpr-var-value 'label context)))
       ('is-numeric (let ((val (cpr-var-value param context)))
		      (cpr-lib-numeric-p val)))
       ;; We return t iff the first date is uncertain. A more complicated alternative
       ;; would be to to test the second date of date ranges as well.
       ('is-uncertain-date (let ((dates (cpr-var-value param context)))
			     (if dates (cpr-date-circa (car dates)) nil)))
       ('position (and (eq (cpr-context-mode context) 'cite)
		       (or (and (eq param 'near-note) (cpr-var-value 'near-note context))
			   (let ((pos (cpr-var-value 'position context)))
			     (or (eq param pos)
				 (and (eq param 'subsequent)
				      (or (eq pos 'ibid) (eq pos 'ibid-with-locator)))
				 (and (eq param 'ibid)
				      (eq pos 'ibid-with-locator)))))))
       ('disambiguate (cpr-var-value 'disambiguate context))))))

(defmacro cpr--choose (_attrs _context &rest body)
  "Return the content of the first element in BODY with t boolean value.
Return the empty (nil . 'text-only) content if there is no such
element."
  (setq body (cpr-lib-splice-into body 'splice))
  `(let ((first-true
	  (--first (car it) (list ,@body))))
     (if first-true
	 (cdr first-true)
       (cons nil (quote text-only)))))

(defmacro cpr--if (attrs context &rest body)
  "If conditions in ATTRS eval to t return t with rendered BODY.
Return nil otherwise."
  `(if (cpr-choose-eval-conditions ,attrs ,context)
       (cons t (cpr-lib-add-splice-tag (list ,@body) 'splice))
     nil))

(defalias 'cpr--else-if 'cpr--if)

(defun cpr--else (_attrs _context &rest body)
  "Always return t boolean plus rendered BODY"
  (setq body (cpr-lib-splice-into body 'splice))
  (cons t (cpr-lib-add-splice-tag body 'splice)))

(provide 'cpr-choose)

;;; cpr-choose.el ends here
