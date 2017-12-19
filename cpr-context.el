;;; cpr-context.el --- rendering context for CSL elements -*- lexical-binding: t; -*-

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

;; Provides the `cpr-context' CL-structure type to represent rendering contexts
;; for CSL elements, and functions for accessing context-dependendt information
;; (variable and term values etc.). Also contains some functions that perform
;; context-dependent formatting (e.g., quoting).

;;; Code:

(require 'let-alist)
(require 'subr-x)

(require 'cpr-lib)
(require 'cpr-rt)
(require 'cpr-term)
(require 'cpr-prange)
(require 'cpr-style)

(cl-defstruct (cpr-context (:constructor cpr-context--create))
  "A struct representing the context for rendering CSL elements."
  vars macros terms date-text date-numeric opts locale-opts
  mode render-mode render-year-suffix)

(defun cpr-context-create (var-alist style mode render-mode)
  "Create a cpr-context struct from var-values VAR-ALIST and csl style STYLE.
MODE is either 'bib or 'cite,
RENDER-MODE is 'display or 'sort."
  (cpr-context--create :vars var-alist
		       :macros (cpr-style-macros style)
		       :terms (cpr-style-terms style)
		       :date-text (cpr-style-date-text style)
		       :date-numeric (cpr-style-date-numeric style)
		       :opts (cpr-style-global-opts style mode)
		       :locale-opts (cpr-style-locale-opts style)
		       :mode mode
		       :render-mode render-mode
		       :render-year-suffix (not (cpr-style-uses-ys-var style))))

(defconst cpr--short-long-var-alist '((title . title-short)
				      (container-title . container-title-short))
  "Alist mapping the long form of variables names to their short form.")

(defun cpr-var-value (var context &optional form)
  "Return the value of csl variable VAR in CONTEXT.
VAR is a symbol, GLOBALS is a `cpr-context' struct, and the
optional FORM can be nil, 'short or 'long."
  (if (eq form 'short)
      (if-let ((short-var (alist-get var cpr--short-long-var-alist))
	       (short-var-val (alist-get short-var (cpr-context-vars context))))
	  short-var-val
	(alist-get var (cpr-context-vars context)))
    (let ((var-val (alist-get var (cpr-context-vars context))))
      (if (and var-val (or (and (eq var 'locator)
				(string= (cpr-var-value 'label context) "page"))
			   (eq var 'page)))
	  (let ((prange-format (cpr-lib-intern (alist-get 'page-range-format
							  (cpr-context-opts context))))
		(sep (or (cpr-term-text-from-terms "page-range-delimiter"
						   (cpr-context-terms context))
			 "–")))
	    (cpr-prange-render var-val prange-format sep))
	var-val))))

(defun cpr-locator-label (context)
  "Return the current locator label variable from CONTEXT."
  (cpr-var-value 'label context))

(defun cpr-rt-quote (rt context)
  "Return the quoted version of rich-text RT using CONTEXT."
  (let ((oq (cpr-term-get-text "open-quote" context))
	(cq (cpr-term-get-text "close-quote" context))
	(oiq (cpr-term-get-text "open-inner-quote" context))
	(ciq (cpr-term-get-text "close-inner-quote" context)))
    `(,oq ,@(cpr-rt-replace-all `((,oq . ,oiq) (,cq . ,ciq)
				  (,oiq . ,oq) (,ciq . ,cq))
				rt)
	  ,cq)))

(defun cpr-rt-join-formatted (attrs rts context)
  "Join and format according to ATTRS the rich-texts in RTS."
  (let-alist attrs
    (let ((result (delq nil rts)))
      (when .text-case (setq result (cpr--textcased result (intern .text-case))))
      (when (string= .strip-periods "true") (setq result (cpr-rt-strip-periods result)))
      (when (string= .quotes "true") (setq result (cpr-rt-quote result context)))
      (push (cpr-rt-select-attrs attrs cpr-rt-ext-format-attrs) result)
      (if (and .delimiter
	       (> (length result) 2))
	  result
	(cpr-rt-simplify-shallow result)))))

(defun cpr-rt-format-single (attrs rt context)
  "Format according to ATTRS rich-text RT using CONTEXT."
  (if (or (not rt) (and (char-or-string-p rt) (string= rt "")))
      nil
    (cpr-rt-join-formatted attrs (list rt) context)))

(defun cpr-rt-typed-join (attrs typed-rts context)
  "Join and format according to ATTRS contents in list TYPED-RTS.
TYPED RTS is a list of (RICH-TEXT . TYPE) pairs"
  (-let* ((types (--map (cdr it) typed-rts))
	  (type (cond ((--all? (eq it 'text-only) types)
		       'text-only)
		      ((--any? (eq it 'present-var) types)
		       'present-var)
		      (t 'empty-vars))))
    (cons (cpr-rt-join-formatted attrs
				 (--map (car it) typed-rts)
				 context)
	  type)))

(defun cpr-term-get-text (term context)
  "Return the first text associated with TERM in CONTEXT."
  (cpr-term-text-from-terms term (cpr-context-terms context)))

(defun cpr-term-inflected-text (term form number context)
  "Return the text associated with TERM having FORM and NUMBER."
  (let ((matches
	 (--select (string= term (cpr-term-name it))
		   (cpr-context-terms context))))
    (cond ((not matches) nil)
	  ((= (length matches) 1)
	   (cpr-term-text (car matches)))
	  (t (cpr-term--inflected-text-1 matches form number)))))

(defconst cpr--term-form-fallback-alist
  '((verb-short . verb)
    (symbol . short)
    (verb . long)
    (short . long))
  "Alist containing the fallback form for each term form.")

(defun cpr-term--inflected-text-1 (matches form number)
  (let ((match (--first (and (eq form (cpr-term-form it))
			     (or (not (cpr-term-number it))
				 (eq number (cpr-term-number it))))
			matches)))
    (if match
	(cpr-term-text match)
      (cpr-term--inflected-text-1 matches
				  (alist-get form cpr--term-form-fallback-alist)
				  number))))

(defun cpr-term-get-gender (term context)
  "Return the gender of TERM or nil if none is given."
  (if-let (match
	   (--first (and (string= (cpr-term-name it) term)
			 (cpr-term-gender it)
			 (eq (cpr-term-form it) 'long))
		    (cpr-context-terms context)))
      (cpr-term-gender match)
    nil))

(provide 'cpr-context)

;;; cpr-context.el ends here
