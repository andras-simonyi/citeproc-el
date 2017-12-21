;;; cpr-proc.el --- construct and manage citation processors -*- lexical-binding: t; -*-

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

;; Structure type and functions to construct citation processor objects, add or
;; clear stored bibliography items, and disambiguate and finalize the rendering
;; of the stored items.

;;; Code:

(require 'subr-x)
(require 'dash)
(require 'cl-lib)
(require 'queue)

(require 'cpr-date)
(require 'cpr-itemdata)
(require 'cpr-disamb)

(cl-defstruct (cpr-proc (:constructor cpr-proc--create))
  "Citation processor struct.
STYLE is a `cpr-style' struct,
GETTER is a bibliography item getter,
ITEMDATA is a hash table that maps itemids to `cpr-itemdata'
  structs,
CITATIONS is a queue containing citations,
NAMES is hash table that maps name alists to ids,
FINALIZED is non-nil iff the processor is finalized
  (bibliography items are properly sorted, citation positions are
  updated etc)."
  style getter itemdata citations names finalized)

(defun cpr-proc-clear (proc)
  "Remove all bibliographic and citation data from PROC."
  (clrhash (cpr-proc-itemdata proc))
  (clrhash (cpr-proc-names proc))
  (queue-clear (cpr-proc-citations proc))
  (setf (cpr-proc-finalized proc) t))

(defun cpr-proc--internalize-name (name proc)
  "Find or add name-alist NAME in/to the names stored in PROC.
Return an internalized version which contains the name-id, and is
sorted."
  (let* ((sorted (sort name (lambda (x y)
			      (string< (car x) (car y)))))
	 (names (cpr-proc-names proc))
	 (val (gethash sorted names)))
    (cons (cons 'name-id
		(or val (puthash sorted (hash-table-count names) names)))
	  sorted)))

(defconst cpr-proc--nonstd-csl-vars-alist '((shortTitle . title-short)
					    (journalAbbreviation . container-title-short))
  "Alist mapping non-standard citeproc.js vars to their standard CSL peers.")

(defun cpr-proc--internalize-item (proc item)
  "Return the internal form of a CSL json ITEM for PROC."
  (let* (label
	 page-first
	 (result
	  (--map (let* ((orig-var (car it))
			(var (if-let ((mapped (alist-get orig-var
							 cpr-proc--nonstd-csl-vars-alist)))
				 mapped
			       orig-var))
			(value (cpr-proc--parse-csl-var-val (cdr it) var proc)))
		   (pcase var
		     ('page (when-let ((page-first-match (s-match "[[:digit:]]+" value)))
			      (setq page-first (car page-first-match))))
		     ('label (setq label t)))
		   (cons var value))
		 item)))
    (when page-first (push (cons 'page-first page-first) result))
    (unless label (push (cons 'label "page") result))
    result))

(defun cpr-proc--put-item (proc item itemid)
  "Put parsed csl-json ITEM with ITEMID into PROC.
Return the added itemdata structure."
  (let* ((int-vars (cpr-proc--internalize-item proc item))
	 (itemdata (cpr-itemdata-create :varvals int-vars :rc-uptodate nil)))
    (cpr-proc-put-itd-put itemid itemdata proc)
    (cpr-itd-setvar itemdata 'citation-number
		    (number-to-string (hash-table-count
				       (cpr-proc-itemdata proc))))
    (setf (cpr-proc-finalized proc) nil)
    itemdata))

(defun cpr-proc-put-item-by-id (proc itemid)
  "Put item with ITEMID into the itemlist of PROC.
Return the itemdata struct that was added."
  (let ((item (cdar (funcall (cpr-proc-getter proc)
			     (list itemid)))))
    (cpr-proc--put-item proc
			(or item `((unprocessed-with-id . ,itemid)))
			itemid)))


(defun cpr-proc-put-items-by-id (proc itemids)
  "Add items with ITEMIDS into the itemlist of PROC."
  (let* ((received (funcall (cpr-proc-getter proc) itemids))
	 ;; OPTIMIZE: putting the received items into the original order could/should be
	 ;; made more efficient
	 (items (--map (cons it (assoc-default it received))
		       itemids)))
    (cl-loop for (itemid . item) in items do
	     (cpr-proc--put-item proc
				 (or item `((unprocessed-with-id . ,itemid)))
				 itemid))))

(defun cpr-proc-put-itd-put (id data proc)
  "Put the DATA of item with ID in processor PROC."
  (let ((itemdata (cpr-proc-itemdata proc)))
    (puthash id data itemdata)))

(defun cpr-proc-delete-occurrence-info (proc)
  "Remove all itemdata occurrence info from PROC."
  (maphash (lambda (_ itd)
	     (setf (cpr-itemdata-occurred-before itd) nil))
	   (cpr-proc-itemdata proc)))

(defun cpr-proc--parse-csl-var-val (rep var proc)
  "Parse the json representation REP of csl variable VAR.
VAR is a csl variable as symbol;
REP is its value in standard csl json representation as parsed by
  the Emacs `json' library;
PROC is the target cpr-processor of the internal representation.
Return the PROC-internal representation of REP."
  (cond ((memq var cpr--name-vars)
	 (--map
	  (let* ((filtered (-remove (lambda (x) (eq (car x) 'isInstitution)) it))
		 (w-smart-aposts (--map (cons (car it)
					      (let ((text-field (cdr it)))
						(if (stringp text-field)
						    (cpr-s-smart-apostrophes text-field)
						  text-field)))
					filtered)))
	    (cpr-proc--internalize-name w-smart-aposts proc))
	  rep))
	((memq var cpr--date-vars)
	 (cpr-date-parse rep))
	;;FIXME: We handle here the id... do we need it in the itemdata at all?
	((or (memq var cpr--number-vars) (eq 'id var))
	 (cpr-s-from-num-or-s rep))
	((stringp rep)
	 (let* ((w-aposts (cpr-s-smart-apostrophes rep))
		(rt (cpr-rt-from-str w-aposts)))
	   (if (s-contains-p "\"" rep)
	       (let* ((terms (cpr-style-terms (cpr-proc-style proc)))
		      (oq (cpr-term-text-from-terms "open-quote" terms))
		      (cq (cpr-term-text-from-terms "close-quote" terms)))
		 (cpr-rt-change-case rt (lambda (x) (cpr-s-smart-quotes x oq cq))))
	     rt)))
	(t rep)))

(defun cpr-proc-disamb (proc)
  "Disambiguate the items stored in PROC."
  (let* ((cite-opts (cpr-style-cite-opts (cpr-proc-style proc)))
	 (name (string= "true" (alist-get 'disambiguate-add-names cite-opts)))
	 (given (string= "true" (alist-get 'disambiguate-add-givenname cite-opts)))
	 (yearsuff (string= "true" (alist-get 'disambiguate-add-year-suffix cite-opts))))
    (cpr-disamb-itds (hash-table-values (cpr-proc-itemdata proc))
		     (cpr-proc-style proc)
		     name given yearsuff)))

(defun cpr-proc-finalize (proc)
  "Finalize processor PROC by sorting and disambiguating items."
  (unless (cpr-proc-finalized proc)
    (cpr-proc-update-sortkeys proc)
    (cpr-proc-sort-itds proc)
    (cpr-proc-update-positions proc)
    (cpr-proc-disamb proc)
    (cpr-proc-sort-cites proc)
    (cpr-proc-group-and-collapse-cites proc)
    (setf (cpr-proc-finalized proc) t)))

(defun cpr-proc-byte-compile (proc)
  "Byte-compile all lambdas in PROC."
  (let* ((style (cpr-proc-style proc))
	 (bib-sort (cpr-style-bib-sort style))
	 (cite-sort (cpr-style-cite-sort style)))
    (setf (cpr-style-macros style)
	  (--map (cons (car it) (byte-compile (cdr it)))
		 (cpr-style-macros style))
	  (cpr-style-cite-layout style)
	  (byte-compile (cpr-style-cite-layout style))
	  (cpr-style-bib-layout style)
	  (byte-compile (cpr-style-bib-layout style)))
    (when bib-sort (setf (cpr-style-bib-sort style) (byte-compile bib-sort)))
    (when cite-sort (setf (cpr-style-cite-sort style) (byte-compile cite-sort)))))

(provide 'cpr-proc)

;;; cpr-proc.el ends here
