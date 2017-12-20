;;; cpr-sort.el --- cite and bibliography sorting  -*- lexical-binding: t; -*-

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

;; Functions to sort cites and bibliography items.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'dash)

(require 'cpr-lib)
(require 'cpr-s)
(require 'cpr-rt)
(require 'cpr-macro)
(require 'cpr-proc)
(require 'cpr-name)

(defun cpr--sort (_attrs _context &rest body)
  "Placeholder function corresponding to the cs:sort element of CSL."
  body)

(defun cpr-sort--name-var-key (var context)
  "Return the sort-key for name-var VAR using CONTEXT.
VAR is a CSL name-var name as a symbol. The returned value is a
string containing a semicolon-separated list of all full names in
sort order."
  (cpr-rt-to-plain (cpr-rt-render-affixes
		    (cpr-name--render-var var '((form . "long") (name-as-sort-order . "all")
						(et-al-min . nil) (et-al-use-first . "0")
						(delimiter . "; "))
					  nil nil nil nil nil context))))

(defun cpr-sort--date-as-key (d _context)
  "Render D cpr-date struct as a sort key."
  (if d (let ((year (cpr-date-year d))
	      (month (or (cpr-date-month d) 0))
	      (day (or (cpr-date-day d) 0)))
	  ;; We add 5000 as an offset to deal with sorting BC years properly
	  (concat (number-to-string (+ 5000 year))
		  (cpr-s-fill-left-to-len (number-to-string month) 2 ?0)
		  (cpr-s-fill-left-to-len (number-to-string day) 2 ?0)))
    ""))

(defun cpr-sort--date-var-key (var context)
  "Return the sort-key for name-var VAR using CONTEXT.
VAR is a symbol."
  (-let* (((d1 d2) (cpr-var-value var context))
	  (rendered-first (cpr-sort--date-as-key d1 context)))
    (if d2
	(s-concat rendered-first "–" (cpr-sort--date-as-key d2 context))
      rendered-first)))

(defun cpr--key (attrs context &rest _body)
  "Return a sort key corresponding to ATTRS and CONTEXT."
  (-let (((&alist 'macro macro
		  'variable var)
	  attrs)
	 (global-attrs (--filter (memq (car it)
				       '(names-min names-use-first names-use-last))
				 attrs)))
    (if var (let ((var-sym (intern var)))
	      (cond ((memq var-sym cpr--number-vars)
		     ;; OPTIMIZE: This is way too complicated to simply get a filled numeric value..
		     (cpr-s-fill-left-to-len
		      (cpr-number-var-value
		       (cpr-var-value var-sym context) var-sym 'numeric context)
		      5))
		    ((memq var-sym cpr--date-vars)
		     (cpr-sort--date-var-key var-sym context))
		    ((memq var-sym cpr--name-vars)
		     (cpr-sort--name-var-key var-sym context))
		    (t (cpr-rt-to-plain (cpr-var-value var-sym context)))))
      (let ((new-context (cpr-context--create
			  :vars (cpr-context-vars context) :macros (cpr-context-macros context)
			  :terms (cpr-context-terms context)
			  :date-text (cpr-context-date-text context)
			  :date-numeric (cpr-context-date-numeric context)
			  :opts (nconc global-attrs (cpr-context-opts context))
			  :mode (cpr-context-mode context) :render-mode 'sort
			  :render-year-suffix nil)))
	(cpr-macro-output-as-text macro new-context)))))

(defun cpr-sort--compare-keys (k1 k2 &optional desc)
  "Return 1, 0 or -1 depending on the sort-order of keys K1 and K2.
If optional DESC is non-nil then reverse the comparison for
descending sort."
  (cond ((string-collate-equalp k1 k2) 0)
	((s-blank? k1) -1)
	((s-blank? k2) 1)
	(t (* (if (string-collate-lessp k1 k2) 1 -1)
	      (if desc -1 1)))))

(defun cpr-sort--compare-keylists (k1 k2 sort-orders)
  "Whether keylist K1 precedes keylist K2 in the sort order.
SORT-ORDERS is a list of sort orders to use (see the bib- and
cite-sort-orders slots of `cpr-style' for details)."
  (cpr-lib-lex-compare k1 k2 #'cpr-sort--compare-keys sort-orders))

(defun cpr-sort--render-keys (style var-alist mode)
  "Render the sort keys of an item with STYLE and VAR-ALIST.
MODE is either `cite' or `bib'."
  (let ((context (cpr-context-create var-alist style mode 'sort))
	(sort (cl-ecase mode
		(cite (cpr-style-cite-sort style))
		(bib (cpr-style-bib-sort style)))))
    (if sort (funcall sort context) nil)))

(defun cpr-itd-update-sortkey (itd style)
  "Update the sort key of itemdata ITD for STYLE."
  (setf (cpr-itemdata-sort-key itd)
	(cpr-sort--render-keys style (cpr-itemdata-varvals itd) 'bib)))

(defun cpr-proc-update-sortkeys (proc)
  "Update all sort keys of the itemdata in PROC."
  (let ((style (cpr-proc-style proc))
	(itds (cpr-proc-itemdata proc)))
    (maphash (lambda (_id itd)
	       (cpr-itd-update-sortkey itd style))
	     itds)))

(defun cpr-proc-get-itd-list (proc)
  "Return the ordered itemdata list of PROC.
Ordering is according to citation number."
  (sort (hash-table-values (cpr-proc-itemdata proc))
	(lambda (x y)
	  (< (string-to-number (cpr-itd-getvar x 'citation-number))
	     (string-to-number (cpr-itd-getvar y 'citation-number))))))

(defun cpr-proc-sort-itds (proc)
  "Sort the itemdata in PROC."
  (when (cpr-style-bib-sort (cpr-proc-style proc))
    (let* ((itds (cpr-proc-get-itd-list proc))
	   (sort-orders (cpr-style-bib-sort-orders (cpr-proc-style proc)))
	   (sorted (sort itds
			 (lambda (x y)
			   (cpr-sort--compare-keylists (cpr-itemdata-sort-key x)
						       (cpr-itemdata-sort-key y)
						       sort-orders)))))
      (--each-indexed sorted
	(cpr-itd-setvar it 'citation-number
			(number-to-string (1+ it-index)))))))

(provide 'cpr-sort)

;;; cpr-sort.el ends here
