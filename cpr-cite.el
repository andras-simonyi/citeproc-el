;;; cpr-cite.el --- cite and citation rendering -*- lexical-binding: t; -*-

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

;; Functionality to render citations and the cites they contain. (Terminology
;; from the CSL standard: "citations consist of one or more cites to individual
;; items".)

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'queue)

(require 'cpr-rt)
(require 'cpr-number)
(require 'cpr-itemdata)
(require 'cpr-style)
(require 'cpr-proc)

(cl-defstruct (cpr-citation (:constructor cpr-citation-create))
  "A struct representing a citation.
CITES is a list of cites,
NOTE-INDEX is the note index of the citation if it occurs in a
  note,
CAPITALIZE-FIRST is non-nil if the first word of the rendered
  citation should be capitalized,
SUPPRESS-AFFIXES is non-nil if the citation affixes should be
  suppressed.
GROUPED is used internally to indicate whether the cites were
  grouped by the csl processor."
  cites (note-index nil) (capitalize-first nil)
  (suppress-affixes nil) (grouped nil))

(defun cpr-cite--varlist (cite)
  "Return the varlist belonging to CITE."
  (let* ((itd (alist-get 'itd cite))
	 (item-vv (cpr-itemdata-varvals itd))
	 ;; OPTIMIZE: Should we do this filtering?
	 (cite-vv
	  (--filter (memq (car it)
			  '(label locator suppress-author suppress-date
				  position near-note first-reference-note-number))
		    cite)))
    (nconc cite-vv item-vv)))

(defun cpr-cite--render (cite style &optional no-link)
  "Render CITE in STYLE, together with its affixes.
If the prefix or suffix in CITE don't contain trailing and
leading spaces then they are added. If optional NO-LINK is
non-nil then don't link citations to the referred items."
  (-let* ((result nil)
	  ((&alist 'suffix suff
		   'prefix pref)
	   cite)
	  (rt-pref (cpr-rt-from-str pref))
	  (plain-pref (cpr-rt-to-plain rt-pref))
	  (rt-suff (cpr-rt-from-str suff))
	  (plain-suff (cpr-rt-to-plain rt-suff))
	  (rendered-varlist (cpr-render-varlist-in-rt (cpr-cite--varlist cite)
						       style 'cite 'display no-link)))
    (when (s-present-p plain-suff)
      (push (cpr-rt-from-str suff) result)
      (unless (= (aref plain-suff 0) ?\s)
	(push " " result)))
    (push rendered-varlist result)
    (when (s-present-p plain-pref)
      (unless (= (aref plain-pref (1- (length plain-pref))) ?\s)
	(push " " result))
      (push rt-pref result))
    (cpr-rt-join-formatted nil result nil)))

(defun cpr-cite-or-citegroup--render (c style no-links top-dl gr-dl ys-dl ac-dl)
  "Render cite or cite-group C with STYLE.
If NO-LINKS is non-nil then don't link cites to the cited items.
TOP-DL is the top-, GR-DL the group-, YS-DL the year-suffix- and
AC-DL the after-collapse-delimiter to use."
  (cond ((and (car c) (memq (car c) '(top group year-suffix-collapsed)))
	 (let ((delimiter (pcase (car c)
			    ('top top-dl)
			    ('group gr-dl)
			    ('year-suffix-collapsed ys-dl))))
	   (cons
	    nil				; empty attribute list
	    (nbutlast			; remove last delimiter
	     (--mapcat (list (cpr-cite-or-citegroup--render
			      it style no-links top-dl gr-dl ys-dl ac-dl)
			     (if (and (car it) (memq (car it) '(group year-suffix-collapsed)))
				 ac-dl
			       delimiter))
		       (cdr c))))))
	((eq (car c) 'range)
	 (list nil (cpr-cite--render (cl-second c) style no-links)
	       "–" (cpr-cite--render (cl-third c) style no-links)))
	(t (cpr-cite--render c style no-links))))

(defun cpr-citation--render (c proc &optional no-links)
  "Render citation C with CSL processor PROC.
If optional NO-LINKS is non-nil then don't link cites to the
bibliograpgy items they refer to."
  (let* ((style (cpr-proc-style proc))
	 (punct-in-quote
	  (string= (alist-get 'punctuation-in-quote (cpr-style-locale-opts style))
		   "true"))
	 (cites (cpr-citation-cites c))
	 (cite-attrs (cpr-style-cite-layout-attrs style))
	 (cite-layout-dl (alist-get 'delimiter cite-attrs)))
    ;; Remove delimiters from cite-attrs -- they are rendered 'manually' because of
    ;; the delimiter-after-collapse complications in rendering. Also remove affixes
    ;; if requested.
    (setq cite-attrs
	  (if (cpr-citation-suppress-affixes c)
	      (--remove (memq (car it) '(delimiter prefix suffix)) cite-attrs)
	    (--remove (eq (car it) 'delimiter) cite-attrs)))
    ;; Generate rendered cites
    (let ((rendered-cites
	   (cond
	    ((cpr-citation-grouped c)
	     (let ((gr-dl
		    (alist-get 'cite-group-delimiter (cpr-style-cite-opts style)))
		   (ys-dl
		    (alist-get 'year-suffix-delimiter (cpr-style-cite-opts style)))
		   (aft-coll-dl
		    (alist-get
		     'after-collapse-delimiter (cpr-style-cite-opts style))))
	       (cdr (cpr-cite-or-citegroup--render
		     (cons 'top cites)	; indicate top level input
		     style no-links cite-layout-dl gr-dl ys-dl aft-coll-dl))))
	    ((cdr cites)
	     (cdr (--mapcat
		   (list cite-layout-dl (cpr-cite--render it style no-links))
		   cites)))
	    (t
	     (list (cpr-cite--render (car cites) style no-links))))))
      ;; Calculate inner and outer citation attrs (affixes go inside)
      (let* ((non-affixes (--remove (memq (car it) '(prefix suffix delimiter)) cite-attrs))
	     (affixes (--filter (memq (car it) '(prefix suffix)) cite-attrs))
	     (outer-attrs (and affixes non-affixes))
	     (result
	      (cpr-rt-cull-spaces-puncts
	       (cpr-rt-finalize
		(cpr-rt-render-affixes
		 (cpr-rt-join-formatted (if outer-attrs affixes cite-attrs)
					rendered-cites nil)
		 t)
		punct-in-quote))))
	;; Add outer (non-affix attrs) if needed
	(when outer-attrs
	  (setq result (list outer-attrs result)))
	;; Capitalize first
	(if (cpr-citation-capitalize-first c)
	    (cpr-rt-change-case result #'cpr-s-capitalize-first)
	  result)))))

(defun cpr-cites--collapse-indexed (cites index-getter no-span-pred)
  "Collapse continuously indexed cites in CITES.
INDEX-GETTER is a function from cites to numeric indices,
NO-SPAN-PRED is a predicate that returns non-nil for cites that
cannot be part of a span. Return the collapsed cites list or nil
if no cites were collapsed."
  (let (group-len start-cite prev-index end-cite result)
    (dolist (cite cites)
      (let* ((cur-index (funcall index-getter cite))
	     (no-span-elt (funcall no-span-pred cite))
	     (subsequent (and prev-index (= (1+ prev-index) cur-index))))
	;; Process ending current group
	(when (and group-len (or no-span-elt (not subsequent)))
	  (setq result (nconc (cpr-cite-range--collapse
			       start-cite end-cite
			       group-len)
			      result)))
	(cond (no-span-elt		; Not only cite-no
	       (push cite result)
	       (setq group-len nil))
	      ((or (not group-len) (not subsequent)) ; New group starts
	       (setq group-len 1
		     start-cite cite
		     prev-index cur-index))
	      (t			; Group continues
	       (cl-incf group-len)
	       (setq end-cite cite
		     prev-index cur-index)))))
    ;; Process the last group
    (when group-len
      (setq result (nconc (cpr-cite-range--collapse
			   start-cite end-cite group-len)
			  result)))
    (if (not (= (length cites) (length result)))
	(nreverse result)
      nil)))

(defun cpr-cite-range--collapse (start-cite end-cite len)
  "Collapse cite span with START-CITE, END-CITE of LEN length.
START-CITE end END-CITE is the first and last rendered cites of
the span."
  (pcase len
    (1 (list start-cite))
    (2 (list end-cite start-cite))
    (_ (list (list 'range start-cite end-cite)))))

(defun cpr-citation--collapse-num-citeranges (citation)
  "Collapse numbered ranges in CITATION."
  (let* ((cites (cpr-citation-cites citation))
	 (cites-length (length cites)))
    (when (> cites-length 2)
      (when-let (collapsed
		 (cpr-cites--collapse-indexed
		  cites
		  (lambda (x)
		    (string-to-number
		     (alist-get 'citation-number (cpr-cite--varlist x))))
		  (lambda (x) (alist-get 'locator (cpr-cite--varlist x)))))
	(setf (cpr-citation-cites citation) collapsed
	      (cpr-citation-grouped citation) t)))))

(defun cpr-cites--collapse-suff-citeranges (cites)
  "Collapse continuously year-suffixed CITES."
  (or (cpr-cites--collapse-indexed
       cites
       (lambda (x)
	 (string-to-char (alist-get 'year-suffix (cpr-cite--varlist x) " ")))
       (lambda (_x) nil))
      cites))

(defun cpr-citation--render-formatted-citation (c proc format &optional no-links)
  "Render citation C with csl processor PROC in FORMAT.
If optional NO-LINKS is non-nil then don't link cites to the
referred items."
  (let ((fmt (cpr-formatter-for-format format)))
    (funcall (cpr-formatter-cite fmt)
	     (funcall (cpr-formatter-rt fmt)
		      (cpr-citation--render c proc no-links)))))

(defun cpr-citation--sort-cites (citation proc)
  "Sort cites in CITATION for processor PROC."
  (let ((cites (cpr-citation-cites citation)))
    (when (cdr cites)
      (let* ((style (cpr-proc-style proc))
	     (sort-orders (cpr-style-cite-sort-orders style)))
	(setf (cpr-citation-cites citation)
	      (sort
	       (--map (cons (cons 'key	; add keys to the cites as extra attr
				  (cpr-sort--render-keys style (cpr-cite--varlist it) 'cite))
			    it)
		      cites)
	       (lambda (x y) (cpr-sort--compare-keylists (cdar x) (cdar y) sort-orders))))))))

(defun cpr-proc-sort-cites (proc)
  "Sort cites in all citations of PROC."
  (when (cpr-style-cite-sort (cpr-proc-style proc))
    (dolist (citation (queue-head (cpr-proc-citations proc)))
      (cpr-citation--sort-cites citation proc))))

(defun cpr-proc-group-and-collapse-cites (proc)
  "Group and collapse cites in all citations of PROC."
  (let* ((cite-opts (cpr-style-cite-opts (cpr-proc-style proc)))
	 (group-delim (alist-get 'cite-group-delimiter cite-opts))
	 (collapse-type (alist-get 'collapse cite-opts))
	 (collapse-year-type
	  (when collapse-type
	    (let ((cy (member collapse-type
			      '("year" "year-suffix" "year-suffix-ranged"))))
	      (and cy (car cy))))))
    ;; Collapse (and group) according to collapse type
    (cond ((or group-delim collapse-year-type)
	   ;; Group and possibly collapse
	   (dolist (citation (queue-head (cpr-proc-citations proc)))
	     (cpr-citation--group-and-collapse-cites citation proc collapse-type)))
	  ;; Collapse numeric cites
	  ((string= collapse-type "citation-number")
	   (dolist (citation (queue-head (cpr-proc-citations proc)))
	     (cpr-citation--collapse-num-citeranges citation))))))

(defun cpr-citation--group-and-collapse-cites (c proc &optional collapse-type)
  "Divide items in citation C in place into groups for PROC.
Apart from movement necessary for grouping, the relative
positions of cites in C is kept. If optional COLLAPSE-TYPE is
given then collapse the groups accordingly."
  (let ((cites (cpr-citation-cites c)))
    (when (cdr cites)
      (let (groups)
	(dolist (cite cites)
	  (let ((g-ind
		 ;; Cites are in the same group iff the cdrs of the rendered cite's first
		 ;; name-var are equal. The cdr is taken because we ignore attributes, in
		 ;; particular the cited-item-no attribute which is added when the cite consists
		 ;; entirely of the rendered name var
		 (--find-index (equal (cdr (cpr-cite--first-namevar-cont cite proc))
				      (cdr (cpr-cite--first-namevar-cont (car it) proc)))
			       groups)))
	    (if g-ind
		(push cite (nth g-ind groups))
	      (push (list cite) groups))))
	(when (not (= (length groups) (length cites)))
	  (setf (cpr-citation-cites c)
		(nreverse
		 (--map (if (cdr it)
			    (cons 'group
				  (pcase collapse-type
				    ("year"
				     (cpr-citation-group--collapse-year (nreverse it)))
				    ("year-suffix"
				     (cpr-citation-group--collapse-ys (nreverse it) proc nil))
				    ("year-suffix-ranged"
				     (cpr-citation-group--collapse-ys (nreverse it) proc t))
				    (_ (nreverse it))))
			  (car it))
			groups))
		(cpr-citation-grouped c) t))))))

(defun cpr-citation-group--collapse-year (cites)
  "Collapse year in group CITES."
  (cons (car cites)
	(--map (cons '(suppress-author . t) it)
	       (cdr cites))))

(defun cpr-citation-group--collapse-ys (cites proc collapse-ranges)
  "Collapse year and suffix in group CITES using PROC.
If optional COLLAPSE-RANGES is non-nil then collapse year-suffix
ranges."
  (let ((first t) (groups (list (list (car cites))))
	prev-datevar-cont prev-locator)
    (dolist (cite cites)
      (let* ((varlist (cpr-cite--varlist cite))
	     (datevar-cont (cadr (cpr-cite--first-datevar-cont cite proc)))
	     (locator (alist-get 'locator varlist)))
	(cond (first
	       (setq first nil))
	      ((or prev-locator
		   locator
		   (not (alist-get 'year-suffix varlist))
		   (not (equal datevar-cont prev-datevar-cont)))
	       (push (list (cons '(suppress-author . t)
				 cite))
		     groups))
	      (t
	       (push (cons '(suppress-date . t)
			   (cons '(suppress-author . t)
				 cite))
		     (car groups))))
	(setq prev-datevar-cont datevar-cont
	      prev-locator locator))
      cites)
    (nreverse
     (--map (if (cdr it)
		(cons 'year-suffix-collapsed
		      (if (and collapse-ranges (> (length cites) 2))
			  (cpr-cites--collapse-suff-citeranges (nreverse it))
			(nreverse it)))
	      (car it))
	    groups))))

(defun cpr-citations--itd-referred-p (itd citations)
  "Whether ITD is referred to in CITATIONS."
  (let ((cites (--mapcat (cpr-citation-cites it) citations)))
    (--any-p (eq itd (alist-get 'itd it)) cites)))

(defun cpr-cite--update-nn-queue (q index nnd)
  "Remove too distant citations from near-notes queue Q.
INDEX is the actual note-index, NND is the near-note-distance."
  (while (and (queue-head q)
	      (<= nnd (- index
			 (cpr-citation-note-index (queue-first q)))))
    (queue-dequeue q)))

(defun cpr-cite--loc-equal-p (s1 s2)
  "Whether locator strings S1 and S2 refer to the same location."
  (if (and (cpr-lib-numeric-p s1) (cpr-lib-numeric-p s2))
      (equal (cpr-number-extract s1) (cpr-number-extract s2))
    (string= (s-trim s1) (s-trim s2))))

(defvar cpr-disambiguation-cite-pos 'last
  "Which cite position should be the basis of cite disambiguation.
Possible values are 'last, 'first and 'subsequent.")

(defun cpr-proc-update-positions (proc)
  "Update all position-related fields in PROC."
  (cpr-proc-delete-occurrence-info proc)
  (let* ((ctns (queue-head (cpr-proc-citations proc)))
	 (cite-opts (cpr-style-cite-opts (cpr-proc-style proc)))
	 (nnd (string-to-number
	       (or (alist-get 'near-note-distance cite-opts)
		   "5")))
	 (near-note-ctns (make-queue))
	 prev-itd prev-loc prev-label)
    (when (not (eq cpr-disambiguation-cite-pos 'last))
      (dolist (itd (hash-table-values
		    (cpr-proc-itemdata proc)))
	(setf (cpr-itemdata-disamb-pos itd) cpr-disambiguation-cite-pos)))
    (dolist (ctn ctns)
      (let* ((note-ind (cpr-citation-note-index ctn))
	     (cites (cpr-citation-cites ctn))
	     (single-cite (not (cdr cites))))
	(when note-ind (cpr-cite--update-nn-queue near-note-ctns note-ind nnd))
	(let (seen-itds)
	  (while cites
	    (let* ((cite (car cites))
		   (itd (alist-get 'itd cite))
		   (locator (alist-get 'locator cite))
		   (label (alist-get 'label cite))
		   (pos (if (cpr-itemdata-occurred-before itd)
			    (if (eq itd prev-itd)
				(if prev-loc
				    (if locator
					(if (and (cpr-cite--loc-equal-p prev-loc locator)
						 (string= prev-label label))
					    'ibid
					  'ibid-with-locator)
				      'subsequent)
				  (if locator 'ibid-with-locator 'ibid))
			      'subsequent)
			  'first)))
	      (when (and note-ind
			 (or (cpr-citations--itd-referred-p itd (queue-head near-note-ctns))
			     (memq itd seen-itds)))
		(setf (alist-get 'near-note (car cites)) t))
	      (setf (alist-get 'position (car cites)) pos
		    prev-itd itd
		    prev-loc locator
		    prev-label label)
	      (when (eq cpr-disambiguation-cite-pos 'last)
		(cpr--itd-update-disamb-pos itd pos))
	      (let ((prev-occurrence (cpr-itemdata-occurred-before itd)))
		(if prev-occurrence
		    (when (not (eq t prev-occurrence))
		      (setf (alist-get 'first-reference-note-number (car cites))
			    (number-to-string prev-occurrence)))
		  (setf (cpr-itemdata-occurred-before itd) (or note-ind t))))
	      (push itd seen-itds)
	      (pop cites))))
	(unless single-cite
	  (setq prev-itd nil prev-loc nil prev-label nil))
	(when note-ind (queue-append near-note-ctns ctn))))))

(defun cpr--itd-update-disamb-pos (itd pos)
  "Update the highest position of ITD with position POS."
  (let ((old (cpr-itemdata-disamb-pos itd)))
    (when (not (eq old 'subsequent))
      (let ((new (pcase pos
		   ('first 'first)
		   ((or 'ibid 'ibid-with-locator) 'ibid)
		   (_ 'subsequent))))
	(setf (cpr-itemdata-disamb-pos itd)
	      (cond ((memq old '(nil first)) new)
		    ((eq new 'subsequent) 'subsequent)
		    (t 'ibid)))))))

(defun cpr-cite--first-namevar-cont (cite proc)
  "Return the first raw name-var node of CITE rendered with PROC."
  (cpr-rt-find-first-node
   (cpr-itd-rt-cite (alist-get 'itd cite) (cpr-proc-style proc))
   (lambda (x)
     (and (consp x) (memq (alist-get 'rendered-var (car x))
			  cpr--name-vars)))))

(defun cpr-cite--first-datevar-cont (cite proc)
  "Return the first raw date-var node of CITE rendered with PROC."
  (cpr-rt-find-first-node
   (cpr-itd-rt-cite (alist-get 'itd cite) (cpr-proc-style proc))
   (lambda (x)
     (and (consp x) (memq (alist-get 'rendered-var (car x))
			  cpr--date-vars)))))
(provide 'cpr-cite)

;;; cpr-cite.el ends here
