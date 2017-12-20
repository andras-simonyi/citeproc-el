;;; cpr-bibtex.el --- convert BibTeX entries to CSL -*- lexical-binding: t; -*-

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

;; Convert BibTeX bibliography entries to CSL.

;;; Code:

(require 'dash)
(require 'subr-x)
(require 'bibtex)
(require 'cl-lib)

(require 'cpr-s)

(defconst cpr-bt--to-csl-types-alist
  '(("article" . "article-journal") ("book" . "book") ("proceedings" . "book")
    ("manual" . "book") ("periodical" . "book") ("booklet" . "pamphlet")
    ("inbook" . "chapter") ("incollection" . "chapter") ("inproceedings" . "paper-conference")
    ("conference" . "paper-conference") ("mastersthesis" . "thesis") ("phdthesis" . "thesis")
    ("techreport" . "report") ("patent" . "patent") ("electronic" . "webpage")
    ("misc" . "article") ("other" . "article") ("standard" . "legislation")
    ("unpublished" . "manuscript") ("online" . "article-journal"))
  "Alist mapping BibTeX item types to CSL item types.")

(defconst cpr-bt--to-csl-keys-alist
  '(("=key=" . citation-label) ("address" . publisher-place)
    ("booktitle" . container-title) ("journal" . container-title)
    ("chapter" . title) ("location" . event-place) ("series" . collection-title)
    ("keywords" . keyword) ("institution" . publisher) ("school" . publisher)
    ("pages" . page) ("organization" . publisher) ("url" . URL))
  "Alist mapping BibTeX keys to CSL keys with different names.")

(defconst cpr-bt--mon-to-num-alist
  '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("may" . 5) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9) ("oct" . 10) ("nov" . 11) ("dec" . 12))
  "Alist mapping LaTeX abbreviated month names to ordinals.")

(defconst cpr-bt--pref-to-ucs-alist
  '(("'" . "ACUTE") ("`" . "GRAVE") ("^" . "CIRCUMFLEX") ("~" . "TILDE")
    ("=" . "MACRON") ("." . "WITH DOT ABOVE") ("\"" . "DIAERESIS")
    ("''" . "DIAERESIS") ("H" . "DOUBLE ACUTE") ("r" . "WITH RING ABOVE")
    ("u" . "BREVE") ("c" . "CEDILLA") ("k" . "OGONEK"))
  "Alist mapping LaTeX prefixes to unicode name endings.")

(defconst cpr-bt--comm-letter-to-ucs-alist
  '((("`" . "A") . "À")
    (("'" . "A") . "Á")
    (("^" . "A") . "Â")
    (("~" . "A") . "Ã")
    (("\"" . "A") . "Ä")
    (("r" . "A") . "Å")
    (("c" . "C") . "Ç")
    (("`" . "E") . "È")
    (("'" . "E") . "É")
    (("^" . "E") . "Ê")
    (("\"" . "E") . "Ë")
    (("`" . "I") . "Ì")
    (("'" . "I") . "Í")
    (("^" . "I") . "Î")
    (("\"" . "I") . "Ï")
    (("~" . "N") . "Ñ")
    (("`" . "O") . "Ò")
    (("'" . "O") . "Ó")
    (("^" . "O") . "Ô")
    (("~" . "O") . "Õ")
    (("\"" . "O") . "Ö")
    (("`" . "U") . "Ù")
    (("'" . "U") . "Ú")
    (("^" . "U") . "Û")
    (("\"" . "U") . "Ü")
    (("'" . "Y") . "Ý")
    (("`" . "a") . "à")
    (("'" . "a") . "á")
    (("^" . "a") . "â")
    (("~" . "a") . "ã")
    (("\"" . "a") . "ä")
    (("r" . "a") . "å")
    (("c" . "c") . "ç")
    (("`" . "e") . "è")
    (("'" . "e") . "é")
    (("^" . "e") . "ê")
    (("\"" . "e") . "ë")
    (("`" . "i") . "ì")
    (("'" . "i") . "í")
    (("^" . "i") . "î")
    (("\"" . "i") . "ï")
    (("~" . "n") . "ñ")
    (("`" . "o") . "ò")
    (("'" . "o") . "ó")
    (("^" . "o") . "ô")
    (("~" . "o") . "õ")
    (("\"" . "o") . "ö")
    (("`" . "u") . "ù")
    (("'" . "u") . "ú")
    (("^" . "u") . "û")
    (("\"" . "u") . "ü")
    (("'" . "y") . "ý")
    (("\"" . "y") . "ÿ")
    (("H" . "o") . "ő")
    (("H" . "O") . "Ő")
    (("H" . "u") . "ű")
    (("H" . "U") . "Ű"))
  "Alist mapping LaTeX (SYMBOL-COMMAND . ASCII-CHAR) pairs to unicode characters.")

(defconst cpr-bt--to-ucs-alist
  '(("l" . "ł") ("L" . "Ł") ("o" . "ø") ("O" . "Ø") ("AA" . "Å") ("aa" . "å")
    ("AE" . "Æ") ("ae" ly-raw string "\"æ\""))
  "Alist mapping LaTeX commands to characters")

(defun cpr-bt--to-ucs (ltx char)
  "Return the unicode version of LaTeX command LTX applied to CHAR.
LTX is a one-char LaTeX accenting command (e.g. \"'\"), CHAR is
an ascii character. Return NIL if no corresponding unicode
character was found."
  (or (assoc-default (cons ltx char) cpr-bt--comm-letter-to-ucs-alist)
      ;; If the combination is not in cpr-bt--comm-letter-to-ucs-alist then, as a
      ;; last resort, we try to assemble the canonical unicode name of the requested
      ;; character and look it up in (usc-names). This process is *very slow*!
      (if-let ((case-name (if (s-lowercase-p char) "SMALL" "CAPITAL"))
	       (combining-name (assoc-default ltx cpr-bt--pref-to-ucs-alist))
	       (name (concat "LATIN " case-name " LETTER " (upcase char) " " combining-name))
	       (char-name (assoc-default name (ucs-names))))
	  (char-to-string char-name)
	nil)))

(defun cpr-bt--to-csl (s)
  "Convert a BibTeX field S to a CSL one."
  (--> s
       (cpr-bt--preprocess-for-decode it)
       (cpr-bt--decode it)
       (s-replace-all '(("{" . "") ("}" . "") ("\n" . " ")) it)
       (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " it)
       (s-chomp it)))

(defun cpr-bt--preprocess-for-decode (s)
  "Preprocess field S before decoding.
Remove flanking dumb quotes from string S and make some
replacements."
  (let ((wo-quotes (if (and (string= (substring s 0 1) "\"")
			    (string= (substring s -1) "\""))
		       (substring s 1 -1) s)))
    (s-replace "\\&" "&" wo-quotes)))

(defun cpr-bt--to-csl-names (n)
  "Return a CSL version of BibTeX names field N."
  (mapcar #'cpr--bt--to-csl-name (s-split "\\band\\b" n)))

(defun cpr-bt--parse-family (f)
  "Parse family name tokens F into a csl name-part alist."
  (let (family result particle)
    (if-let ((firsts (butlast f)))
	(progn
	  (while (and firsts (s-lowercase-p (car firsts)))
	    (push (pop firsts) particle))
	  (when particle
	    (push `(dropping-particle . ,(nreverse particle)) result))
	  (setq family (-concat firsts (last f))))
      (setq family f))
    (push `(family . ,family) result)
    result))

(defun cpr--bt--to-csl-name (name)
  "Return a CSL version of BibTeX name string NAME."
  (let* (result
	 family
	 (tokens (-remove #'s-blank-str-p
			  (cpr-s-slice-by-matches name "\\(,\\|[[:space:]]+\\)")))
	 (parts (-split-on "," tokens)))
    (pcase (length parts)
      ;; No commas in the name
      (1 (let ((name (car parts)))
	   (if-let ((1st-downcased-idx (-find-index #'s-lowercase-p name)))
	       (progn (setq family (-slice name 1st-downcased-idx))
		      (when (> 1st-downcased-idx 0)
			(push `(given . ,(-slice name 0 1st-downcased-idx)) result)))
	     (setq family (last name))
	     (when (> (length name) 1)
	       (push `(given . ,(-slice name 0 -1)) result)))))
      ;; A single comma separates family and last name
      (2 (setq family (car parts))
	 (push `(given . ,(cadr parts)) result))
      ;; More than one commas
      (_ (setq family (car parts))
	 (push `(suffix . ,(cadr parts)) result)
	 (push `(given . ,(cl-caddr parts)) result)))
    (setq result (nconc (cpr-bt--parse-family family) result))
    (--map (cons (car it) (s-join " " (cdr it)))
	   result)))

(defconst cpr-bt--decode-rx
  (rx (or (seq "\\" (group-n 1 (in "'" "`" "^" "~" "=" "." "\"")) (0+ space)
	       (group-n 2 letter))
	  (seq "\\" (group-n 1 (in "H" "r" "u" "c" "k")) (1+ space)
	       (group-n 2 letter))
	  (seq "\\" (group-n 1 (in "'" "`" "^" "~" "=" "." "\"" "H" "r" "u" "c" "k"))
	       (0+ space) "{" (group-n 2 letter) "}")
	  (seq "\\" (group-n 1 (or "l" "L" "o" "O" "AA" "aa" "ae" "AE")) word-boundary)
	  (seq "{" "\\" (group-n 1 (or "l" "L" "o" "O" "AA" "aa" "ae" "AE"))
	       (0+ space) "}")))
  "Regular expression matching BibTeX special character commands.")

(defun cpr-bt--decode (s)
  "Decode a BibTeX encoded string."
  (replace-regexp-in-string
   cpr-bt--decode-rx
   (lambda (x)
     (let ((command (match-string 1 x))
	   (letter (match-string 2 x)))
       (if letter
	   (or (cpr-bt--to-ucs command letter) (concat "\\" x))
	 (assoc-default command cpr-bt--to-ucs-alist))))
   s))

(defun cpr--bt--to-csl-date (year month)
  "Return a CSL version of the date given by YEAR and MONTH.
YEAR and MONTH are the values of the corresponding BibTeX fields,
MONTH might be nil."
  (let ((csl-year (string-to-number (car (s-match "[[:digit:]]+" year))))
	(csl-month (when month
		     (assoc-default (downcase month)
				    cpr-bt--mon-to-num-alist)))
	date)
    (when csl-year
      (when csl-month (push csl-month date))
      (push csl-year date))
    (list (cons 'date-parts (list date)))))

(defun cpr-bt-entry-to-csl (b)
  "Return a CSL form of normalized parsed BibTeX entry B."
  (let ((type (assoc-default (downcase (assoc-default "=type=" b))
			     cpr-bt--to-csl-types-alist))
	result year month)
    (cl-loop for (key . value) in b do
	     (let ((key (downcase key))
		   (value (cpr-bt--to-csl value)))
	       (if-let ((csl-key (assoc-default key cpr-bt--to-csl-keys-alist)))
		   ;; Vars mapped simply to a differently named CSL var
		   (push (cons csl-key value) result)
		 (pcase key
		   ((or "author" "editor") ; Name vars
		    (push (cons (intern key) (cpr-bt--to-csl-names value))
			  result))
		   ("=type=" (push (cons 'type type) result))
		   ("number" (push (cons (if (string= type "article-journal") 'issue
					   'number)
					 value)
				   result))
		   ;; Date vars that need further processing below
		   ("year" (setq year value))
		   ("month" (setq month value))
		   ;; Remaining keys are mapped without change
		   (_ (push (cons (intern key) value) result))))))
    (when year
      (push (cons 'issued (cpr--bt--to-csl-date year month))
	    result))
    result))

(provide 'cpr-bibtex)

;;; cpr-bibtex.el ends here
