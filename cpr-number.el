;;; cpr-number.el --- render CSL number elements -*- lexical-binding: t; -*-

;; Copyright "(C) 2017 András Simonyi

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

;; Functions to render CSL number elements.

;;; Code:

(require 'subr-x)
(require 'rst)

(require 'cpr-context)
(require 'cpr-lib)
(require 'cpr-s)
(require 'cpr-rt)

(defun cpr-number-extract (val)
  "Return the parse of string VAL holding numeric content."
  (cdr (s-match "\\`\\([[:alpha:]]?[[:digit:]]+[[:alpha:]]*\\)\\(?:\\(?: *\\([,&-–—―]\\|--\\) *\\)\\([[:alpha:]]?[[:digit:]]+[[:alpha:]]*\\)\\)?\\'" val)))

(defun cpr--number (attrs context &rest _body)
  "Render a cs:number element with the given ATTRS in CONTEXT."
  (-let* (((&alist 'variable var
		   'form form)
	   attrs)
	  (variable (intern var))
	  (value (cpr-var-value variable context))
	  (form (cpr-lib-intern form)))
    (if (not value)
	(cons nil 'empty-var)
      (let ((result-attrs
	     (cons `(rendered-var . ,variable) attrs))
	    (result-cont
	     (cpr-number-var-value value variable form context)))
	(cons (cpr-rt-format-single result-attrs result-cont context)
	      'present-var)))))

(defconst cpr--format-numsep-alist
  '(("&" . " & ")
    ("," . ", ")
    ("-" . "-")
    ("--" . "-")
    ("—" . "-")
    ("―" . "-"))
  "Alist specifying the proper formatting of number-pair separators.")

(defun cpr-number-var-value (value variable form context)
  "Return the numeric VALUE of VARIABLE formatted in FORM.
VARIABLE is a symbol."
  (cond  ((not value) nil)
	 ((numberp value) (number-to-string value))
	 (t (--if-let (cpr-number-extract value)
		(let ((formatted-first (cpr-number--format (car it) form variable context)))
		  (if (> (length it) 1)
		      (s-concat formatted-first
				(assoc-default (cadr it) cpr--format-numsep-alist)
				(cpr-number--format (cl-caddr it) form variable context))
		    formatted-first))
	      value))))

(defun cpr-number--format (s form term context)
  "Render the number in string S for TERM in format FORM."
  (if (s-matches-p "[[:alpha:]]" s) s
    (pcase form
      ('roman (downcase (rst-arabic-to-roman (string-to-number s))))
      ('ordinal (cpr-number--format-as-ordinal s term context))
      ('long-ordinal (cpr-number--format-as-long-ordinal s term context))
      (_ s))))

(defun cpr-number--format-as-ordinal (s term context)
  "Format numeric string S as ordinal agreeing with TERM."
  (let* ((terms (cpr-context-terms context))
	 (padded (if (= 1 (length s))
		     (s-concat "0" s)
		   s))
	 (gender (cpr-term-get-gender term context))
	 (matches
	  (--filter (and (string= (s-left 8 (cpr-term-name it)) "ordinal-")
			 (cpr-number--ordinal-matches-p padded gender it))
		    terms))
	 (suffix
	  (cpr-term-text
	   (if (not matches)
	       (when-let (ordinal-matches (--filter (string= (cpr-term-name it) "ordinal")
						    terms))
		 (if-let (match (--first (eq (cpr-term-gender-form it) gender)
					 ordinal-matches))
		     match
		   (car ordinal-matches)))
	     (let ((first-term (car matches)))
	       (if-let (second-term (cadr matches))
		   (if (= (elt (cpr-term-name first-term) 8) ?0) second-term first-term)
		 first-term))))))
    (s-concat s suffix)))

(defconst cpr-number--ordinal-match-alist
  '((last-two-digits . 2)
    (last-digit . 1)
    (whole-number . nil))
  "Alist mapping ordinal match attributes to the required match lengths.")

(defun cpr-number--ordinal-matches-p (s gender ord-term)
  "Whether S term with GENDER matches ordinal-term ORD-TERM."
  (and (eq gender (cpr-term-gender-form ord-term))
       (let ((match (cpr-term-match ord-term))
	     (term-num (s-right 2 (cpr-term-name ord-term))))
	 (when (not match)
	   (setq match
		 (if (= (elt (cpr-term-name ord-term) 8) ?0) 'last-digit 'last-two-digits)))
	 (let ((l (assoc-default match cpr-number--ordinal-match-alist)))
	   (string= (cpr-s-tail s l)
		    (cpr-s-tail term-num l))))))

(defun cpr-number--format-as-long-ordinal (s term context)
  "Format numeric string S as a long ordinal agreeing with TERM."
  (let ((num-val (string-to-number s)))
    (if (> num-val 10)
	(cpr-number--format-as-ordinal s term context)
      (when (= 1 (length s)) (setq s (s-concat "0" s)))
      (let* ((name (s-concat "long-ordinal-" s))
	     (gender (cpr-term-get-gender term context))
	     (match (--first (and (string= (cpr-term-name it) name)
				     (eq (cpr-term-gender-form it) gender))
				(cpr-context-terms context))))
	(if match
	    (cpr-term-text match)
	  (cpr-term-get-text name context))))))

(provide 'cpr-number)

;;; cpr-number ends here
