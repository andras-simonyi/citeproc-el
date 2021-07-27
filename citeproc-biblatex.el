;;; citeproc-biblatex.el --- convert biblatex entries to CSL -*- lexical-binding: t; -*-

;; Copyright (C) 2021 András Simonyi

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

;; Convert biblatex bibliography entries to CSL.

;;; Code:

(require 'iso8601)
(require 'citeproc-bibtex)

(defconst citeproc-blt--to-csl-types-alist
  '((article . "article-journal")
    (book . "book")
    (periodical . "book")
    (booklet . "pamphlet")
    (bookinbook . "chapter")
    (misc . "article")
    (other . "article")
    (standard . "legislation")
    (collection . "book")
    (conference . "paper-conference")
    (dataset . "dataset")
    (electronic . "webpage")
    (inbook . "chapter")
    (incollection . "chapter")
    (inreference . "entry-encyclopedia")
    (inproceedings . "paper-conference")
    (manual . "book")
    (mastersthesis . "thesis")
    (mvbook . "book")
    (mvcollection . "book")
    (mvproceedings . "book")
    (mvreference . "book")
    (online . "webpage")
    (patent . "patent")
    (phdthesis . "thesis")
    (proceedings . "book")
    (reference . "book")
    (report . "report")
    (software . "software")
    (suppbook . "chapter")
    (suppcollection . "chapter")
    (techreport . "report")
    (thesis . "thesis")
    (unpublished . "manuscript")
    (www . "webpage")
    (artwork . "graphic")
    (audio . "song")
    (commentary . "book")
    (image . "figure")
    (jurisdiction . "legal_case")
    (legislation . "bill") ?
    (legal . "treaty")
    (letter . "personal_communication")
    (movie . "motion_picture")
    (music . "song")
    (performance . "speech")
    (review . "review")
    (standard . "legislation")
    (video . "motion_picture")
    (data . "dataset")
    (letters . "personal_communication")
    (newsarticle . "article-newspaper"))
  "Alist mapping biblatex item types to CSL item types.")

(defun citeproc-blt--to-csl-type (type entrysubtype)
  "Return the csltype corresponding to blt TYPE and ENTRYSUBTYPE."
  (pcase type
    ((or 'article 'periodical 'supperiodical)
     (pcase entrysubtype
       ("magazine" "article-magazine")
       ("newspaper" "article-newspaper")
       (_ "article-journal")))
    (_ (assoc-default type citeproc-blt--to-csl-types-alist))))

(defconst citeproc-blt--reftype-to-genre
  '(("mastersthesis" . "Master's thesis")
    ("phdthesis" . "PhD thesis")
    ("mathesis" . "Master's thesis")
    ("resreport" . "research report")
    ("techreport" . "technical report")
    ("patreqfr" . "French patent request")
    ("patenteu" . "European patent")
    ("patentus" . "U.S. patent"))
  "Alist mapping biblatex reftypes to CSL genres.")

(defconst citeproc-blt--article-types
  '(article periodical suppperiodical review)
  "Article-like biblatex types.")

(defconst citeproc-blt--chapter-types
 '(inbook incollection inproceedings inreference bookinbook)
  "Chapter-like biblatex types.")

(defconst citeproc-blt--collection-types
  '(book collection proceedings reference
    mvbook mvcollection mvproceedings mvreference
    bookinbook inbook incollection inproceedings
    inreference suppbook suppcollection)
  "Collection or collection part biblatex types.")

(defconst citeproc-blt--to-csl-names-alist
  '((author . author)
    (editor . editor)
    (bookauthor . container-author)
    (translator . translator))
  "Alist mapping biblatex name fields to the corresponding CSL ones.")

(defconst citeproc-blt--editortype-to-csl-name-alist
  '(("organizer" . organizer)
    ("director" . director)
    ("compiler" . compiler)
    ("editor" . editor)
    ("collaborator" . contributor))
  "Alist mapping biblatex editortypes to CSL fields.")

(defconst citeproc-blt--to-csl-dates-alist
  '((eventdate . event-date)
    (origdate . original-date)
    (urldate . accessed))
  "Alist mapping biblatex date fields to the corresponding CSL ones.")

(defconst citeproc-blt--publisher-fields
  '(school institution organization howpublished publisher)
  "Biblatex fields containing publisher-related information.")

(defconst citeproc-blt--etype-to-baseurl-alist
  '(("arxiv" . "https://arxiv.org/abs/")
    ("jstor" . "https://www.jstor.org/stable/")
    ("pubmed" ."https://www.ncbi.nlm.nih.gov/pubmed/")
    ("googlebooks" . "https://books.google.com?id="))
  "Alist mapping biblatex date fields to the corresponding CSL ones.")

(defconst citeproc-blt--to-csl-standard-alist
  '(;; locators
    (volume . volume)
    (part .  part)
    (edition . edition)
    (version . version)
    (volumes . number-of-volumes)
    (pagetotal . number-of-pages)
    (chapter-number . chapter)
    (pages . page)
    ;; titles
    (eventtitle . event-title)
    (origtitle . original-title)
    (series . collection-title)
    ;; publisher
    (origpublisher . original-publisher)
    ;; places
    (venue . event-place)
    (origlocation . original-publisher-place)
    (address . publisher-place)
    ;; doi etcetera
    (doi . DOI)
    (isbn . ISBN)
    (issn . ISSN)
    (pmid . PMID)
    (pmcid . PMCID)
    (library . call-number)
    ;; notes
    (abstract . abstract)
    (annotation . annote)
    ;; else
    (pubstate . status)
    (language . language)
    (version . version)
    (keywords . keyword)
    (label . citation-label))
  "Alist mapping biblatex standard fields to the corresponding CSL ones.
Only those fields are mapped that do not require further processing.")

(defun citeproc-blt--to-csl-date (d)
  "Return a CSL version of the biblatex date field given by D."
  (let* ((interval-strings (split-string d "/"))
	 (interval-date-parts
	  (mapcar (lambda (x)
		    (let* ((parsed (iso8601-parse-date x))
			   (year (decoded-time-year parsed))
			   (month (decoded-time-month parsed))
			   (day (decoded-time-day parsed))
			   date)
		      (when year
			(when day (push day date))
			(when month (push month date))
			(push year date)
			date)))
		  interval-strings)))
    (list (cons 'date-parts interval-date-parts))))

(defun citeproc-blt--get-standard (v b)
  "Return the CSL-normalized value of var V from item B.
V is a biblatex var name as a string, B is a biblatex entry as an
alist. Return nil if V is undefined in B."
  (-when-let (blt-val (alist-get v b))
    (citeproc-bt--to-csl blt-val)))

(defun citeproc-blt-entry-to-csl (b)
  "Return a CSL form of normalized parsed biblatex entry B.
The processing logic follows the analogous
function (itemToReference) in John MacFarlane's Pandoc, see
<https://github.com/jgm/pandoc/blob/master/src/Text/Pandoc/Citeproc/BibTeX.hs>
Many thanks to him.

Note: in the code, dotted var names refer to values of biblatex
variables in B."
  (let* ((b (mapcar (lambda (x) (cons (intern (downcase (car x))) (cdr x))) b))
	  (.type (intern (downcase (alist-get '=type= b))))
	  (.entrysubtype (alist-get 'entrysubtype b))
	  (type (citeproc-blt--to-csl-type .type .entrysubtype))
	  (is-article (memq .type citeproc-blt--article-types))
	  (is-periodical (eq .type 'periodical))
	  (is-chapter-like (memq .type citeproc-blt--chapter-types))
	  result)
    ;; set type and genre
    (push (cons 'type type) result)
    (when-let ((.reftype (alist-get 'type b))
	       (genre (assoc-default .reftype citeproc-blt--reftype-to-genre)))
      (push (cons 'genre genre) result))
    ;; names
    (when-let ((.editortype (alist-get 'editortype b))
	       (.editor (alist-get 'editor b))
	       (csl-var (assoc-default .editortype
				       citeproc-blt--editortype-to-csl-name-alist)))
      (push (cons csl-var (citeproc-bt--to-csl-names .editor))
	    result))
    (when-let ((.editoratype (alist-get 'editoratype b))
	       (.editora (alist-get 'editora b))
	       (csl-var (assoc-default .editoratype
				       citeproc-blt--editortype-to-csl-name-alist)))
      (push (cons csl-var (citeproc-bt--to-csl-names .editora))
	    result))
    ;; TODO: do this for editorb and editorc as well...
    ;; dates
    (-when-let (issued (-if-let (.issued (alist-get 'date b))
			   (citeproc-blt--to-csl-date .issued)
			 (-when-let (.year (alist-get 'year b))
			   (citeproc-bt--to-csl-date .year
						     (alist-get 'month b)))))
      (push (cons 'issued issued) result))
    ;; locators
    (-if-let (.number (alist-get 'number b))
	(cond ((memq .type citeproc-blt--collection-types) ; collection
	       (push `(collection-number . ,.number) result))
	      (is-article		; article
	       (push `(issue . ,(-if-let (.issue (alist-get 'issue b))
				    (concat .number ", " .issue)
				  .number))
		     result))
	      (t (push `(number . ,.number) result))))
    ;; titles
    (let* ((\.maintitle (alist-get 'maintitle b))
	   (title (cond (is-periodical (citeproc-blt--get-standard 'issuetitle b))
			((and \.maintitle (not is-chapter-like)) \.maintitle)
			(t (citeproc-blt--get-standard 'title b))))
	   (subtitle (citeproc-blt--get-standard
		      (cond (is-periodical 'issuesubtitle)
			    ((and \.maintitle (not is-chapter-like))
			     'mainsubtitle)
			    (t 'subtitle))
		      b))
	   (title-addon (citeproc-blt--get-standard
			 (if (and \.maintitle (not is-chapter-like))
			     'maintitleaddon
			   'titleaddon)
			 b))
	   (volume-title (when \.maintitle
			   (citeproc-blt--get-standard
			    (if is-chapter-like 'booktitle 'title) b)))

	   (volume-subtitle (when \.maintitle
			      (citeproc-blt--get-standard
			       (if is-chapter-like 'booksubtitle 'subtitle) b)))
	   (volume-title-addon (when \.maintitle
				 (citeproc-blt--get-standard
				  (if is-chapter-like 'booktitleaddon 'titleaddon) b)))
	   (container-title (or (and is-periodical (citeproc-blt--get-standard 'title b))
				(and is-chapter-like \.maintitle)
				(and is-chapter-like (citeproc-blt--get-standard
						      'booktitle b))
				(citeproc-blt--get-standard 'journaltitle b)))
	   (container-subtitle (or (and is-periodical (citeproc-blt--get-standard
						       'subtitle b))
				   (and is-chapter-like (citeproc-blt--get-standard
							 'mainsubtitle b))
				   (and is-chapter-like (citeproc-blt--get-standard
							 'booksubtitle b))
				   (citeproc-blt--get-standard 'journalsubtitle b)))
	   (container-title-addon (or (and is-periodical (citeproc-blt--get-standard
							  'titleaddon b))
				      (and is-chapter-like (citeproc-blt--get-standard
							    'maintitleaddon b))
				      (and is-chapter-like
					   (citeproc-blt--get-standard 'booktitleaddon b))))
	   (container-title-short (or (and is-periodical (not \.maintitle)
					   (citeproc-blt--get-standard 'titleaddon b))
				      (citeproc-blt--get-standard 'shortjournal b)))
	   (title-short (or (and (or (not \.maintitle) is-chapter-like)
				 (citeproc-blt--get-standard 'shorttitle b))
			    (and (or subtitle title-addon)
				 (not .maintitle)
				 title))))
      (when title
	(push (cons 'title
		    (concat title
			    (when subtitle (concat ": " subtitle))
			    (when title-addon (concat ". " title-addon))))
	      result))
      (when title-short
	(push (cons 'title-short title-short) result))
      (when volume-title
	(push (cons 'volume-title
		    (concat volume-title
			    (when volume-subtitle (concat ": " volume-subtitle))
			    (when volume-title-addon (concat ". " volume-title-addon))))
	      result))
      (when container-title
	(push (cons 'container-title
		    (concat container-title
			    (when container-subtitle (concat ": " container-subtitle))
			    (when container-title-addon (concat ". " container-title-addon))))
	      result))
      (when container-title-short
	(push (cons 'container-title-short container-title-short) result)))
    ;; publisher
    (-when-let (values (-non-nil (--map (citeproc-blt--get-standard it b)
					citeproc-blt--publisher-fields)))
      (push `(publisher . ,(mapconcat #'identity values "; ")) result))
    ;; places
    (let ((csl-place-var
	   (if (string= .type "patent") 'jurisdiction 'publisher-place)))
      (-when-let (.location (citeproc-blt--get-standard 'location b))
	(push (cons csl-place-var .location) result)))
    ;; url
    (-when-let (url (or (alist-get 'url b)
			(when-let ((.eprinttype (alist-get 'eprinttype b))
				   (.eprint (alist-get 'eprint b))
				   (base-url
				    (assoc-default .eprinttype
						   citeproc-blt--etype-to-baseurl-alist)))
			  (concat base-url .eprint))))
      (push (cons 'URL url) result))
    ;; notes
    (-when-let (note (let ((.note (citeproc-blt--get-standard 'note b))
			   (.addendum (citeproc-blt--get-standard 'addendum b)))
		       (cond ((and .note .addendum) (concat .note ". " .addendum))
			     (.note .note)
			     (.addendum .addendum)
			     (t nil))))
      (push (cons 'note note) result))
    ;; rest
    (let (rest)
      (pcase-dolist (`(,blt-key . ,blt-value)  b) 
	;; remaining standard vars
	(-when-let (csl-key
		    (alist-get blt-key citeproc-blt--to-csl-standard-alist))
	  (unless (alist-get csl-key result)
	    (push (cons csl-key (citeproc-bt--to-csl blt-value)) rest)))
	;; remaining name vars
 	(-when-let (csl-key
		    (alist-get blt-key citeproc-blt--to-csl-names-alist))
	  (unless (alist-get csl-key result)
	    (push (cons csl-key (citeproc-bt--to-csl-names blt-value)) rest)))
	;; remaining date vars
	(-when-let (csl-key
		    (alist-get blt-key citeproc-blt--to-csl-dates-alist))
	  (unless (alist-get csl-key result)
	    (push (cons csl-key (citeproc-blt--to-csl-date blt-value)) rest))))
      (append result rest))))

(provide 'citeproc-biblatex)

;;; citeproc-biblatex.el ends here
