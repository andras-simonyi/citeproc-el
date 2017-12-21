;;; citeproc.el --- A CSL 1.0.1 Citation Processor -*- lexical-binding: t; -*-

;; Copyright (C) 2017 András Simonyi

;; Author: András Simonyi <andras.simonyi@gmail.com>
;; Maintainer: András Simonyi <andras.simonyi@gmail.com>
;; URL: https://github.com/andras-simonyi/citeproc-el
;; Keywords: bib
;; Package-Requires: ((emacs "25") (dash "2.13.0") (s "1.12.0") (f "0.18.0") (queue "0.2"))
;; Version: 0.1

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

;; citeproc-el is a library for rendering citations and bibliographies in styles
;; described in the Citation Style Language (CSL). This file contains the public
;; API. See the accompanying README for full documentation.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'queue)

(require 'cpr-rt)
(require 'cpr-locale)
(require 'cpr-style)
(require 'cpr-choose)
(require 'cpr-generic-elements)
(require 'cpr-context)
(require 'cpr-itemdata)
(require 'cpr-proc)
(require 'cpr-cite)
(require 'cpr-sort)
(require 'cpr-formatters)
(require 'cpr-itemgetters)

(defun citeproc-create (style it-getter loc-getter &optional loc force-loc)
  "Return a CSL processor for a given STYLE, IT-GETTER and LOC-GETTER.
STYLE is either a path to a CSL style file or a CSL style as a
  string.
IT-GETTER is a function that takes a list of itemid strings as
  its sole argument and returns an alist in which the given
  itemids are the keys and the values are the parsed csl json
  descriptions of the corresponding bibliography items (keys are
  symbols, arrays and hashes should be represented as lists and
  alists, respecively).
LOC-GETTER is a function that takes a locale string (e.g.
  \"en_GB\") as an argument and returns a corresponding parsed
  CSL locale (in the same format as IT-GETTER).
Optional LOC is the locale to use if the style doesn't specify a
  default one. Defaults to \"en-US\".
If optional FORCE-LOC is non-nil then use locale LOC even if
  STYLE specifies a different one as default. Defaults to nil."
  (let ((style (citeproc-create-style style loc-getter loc force-loc))
	(names (make-hash-table :test 'equal))
	(itemdata (make-hash-table :test 'equal))
	(citations (make-queue)))
    (cpr-proc--create :style style :getter it-getter :names names
		      :itemdata itemdata :citations citations :finalized t)))

(defun citeproc-render-citations (proc format &optional no-links)
  "Render all citations in PROC in the given FORMAT.
Return a list of formatted citations. If optional NO-LINKS is
non-nil then don't link cites to the referred items."
  (when (not (cpr-proc-finalized proc))
    (cpr-proc-finalize proc))
  (--map (cpr-citation--render-formatted-citation it proc format no-links)
	 (queue-head (cpr-proc-citations proc))))

(defun citeproc-create-style (style locale-getter &optional locale force-locale)
  "Compile style in STYLE into a cpr-style struct.
STYLE is either a path to a CSL style file, or a style as a string.
LOCALE-GETTER is a getter function for locales, the optional
LOCALE is a locale to prefer. If FORCE-LOCALE is non-nil then use
  LOCALE even if the style's default locale is different."
  (-let* (((year-suffix . parsed-style) (cpr-style-parse style))
	  (default-locale (alist-get 'default-locale (cadr parsed-style)))
	  (preferred-locale (if force-locale locale (or default-locale
							locale
							"en-US")))
	  (act-parsed-locale (funcall locale-getter preferred-locale))
	  (act-locale (alist-get 'lang (cadr act-parsed-locale)))
	  (style (citeproc-create-style-from-locale
		  parsed-style
		  (not (not year-suffix)) act-locale)))
    (cpr-style--update-locale style act-parsed-locale)
    (cpr-style--set-opt-defaults style)
    style))

;; REVIEW: this should be rethought -- should we apply the specific wrappers as
;; well?
(defun citeproc-render-varlist (var-alist style mode format)
  "Render an item described by VAR-ALIST with STYLE.
MODE is one of the symbols `bib' or `cite',
FORMAT is a symbol representing a supported output format."
  (funcall (cpr-formatter-rt (cpr-formatter-for-format format))
	   (cpr-rt-cull-spaces-puncts
	    (cpr-rt-finalize
	     (cpr-render-varlist-in-rt var-alist style mode 'display t)))))

(defun citeproc-append-citations (citations proc)
  "Append CITATIONS to the list of citations in PROC.
CITATIONS is a list of `cpr-citation' structures."
  (let ((itemdata (cpr-proc-itemdata proc))
	ids)
    ;; Collect new itemids
    (dolist (citation citations)
      (dolist (cite (cpr-citation-cites citation))
	(push (alist-get 'id cite) ids)))
    (let* ((uniq-ids (delete-dups (nreverse ids))) ; reverse pushed ids
	   (new-ids (--remove (gethash it itemdata) uniq-ids)))
      ;; Add all new items in one pass
      (cpr-proc-put-items-by-id proc new-ids)
      ;; Add itemdata to the cite structs and add them to the cite queue.
      (dolist (citation citations)
	(setf (cpr-citation-cites citation)
	      (--map (cons (cons 'itd (gethash (alist-get 'id it) itemdata)) it)
		     (cpr-citation-cites citation)))
	(queue-append (cpr-proc-citations proc) citation))
      (setf (cpr-proc-finalized proc) nil))))

(defun citeproc-render-bib (proc format &optional no-link-targets)
  "Render a bibliography of items in PROC in FORMAT.
If optional NO-LINK-TARGETS is non-nil then don't generate
targets for citatation links.
  Returns a (FORMATTED-BIBLIOGRAPHY . FORMATTING-PARAMETERS) cons
cell, in which FORMATTING-PARAMETERS is an alist containing the
following formatting parameters keyed to the parameter names as
symbols:
  max-offset (integer): The width of the widest first field in the
bibliography, measured in characters.
  line-spacing (integer): Vertical line distance specified as a
multiple of standard line height.
  entry-spacing (integer): Vertical distance between
bibliographic entries, specified as a multiple of standard line
height.
  second-field-align (`flush' or `margin'): The position of
second-field alignment.
  hanging-indent (boolean): Whether the bibliography items should
be rendered with hanging-indents."
  (if (null (cpr-style-bib-layout (cpr-proc-style proc)))
      "[NO BIBLIOGRAPHY LAYOUT IN CSL STYLE]"
    (when (not (cpr-proc-finalized proc))
      (cpr-proc-finalize proc))
    (let* ((formatter (cpr-formatter-for-format format))
	   (rt-formatter (cpr-formatter-rt formatter))
	   (bib-formatter (cpr-formatter-bib formatter))
	   (bibitem-formatter (cpr-formatter-bib-item formatter))
	   (style (cpr-proc-style proc))
	   (bib-opts (cpr-style-bib-opts style))
	   (punct-in-quote (string= (alist-get 'punctuation-in-quote
					       (cpr-style-locale-opts style))
				    "true"))
	   (sorted (cpr-proc-get-itd-list proc))
	   (raw-bib (--map (cpr-rt-finalize
			    (cpr-render-varlist-in-rt
			     (cpr-itemdata-varvals it)
			     style 'bib 'display no-link-targets)
			    punct-in-quote)
			   sorted))
	   (substituted
	    (if-let (subs-auth-subst
		     (alist-get 'subsequent-author-substitute bib-opts))
		(cpr-rt-subsequent-author-substitute raw-bib subs-auth-subst)
	      raw-bib))
	   (max-offset (if (alist-get 'second-field-align bib-opts)
			   (cpr-rt-max-offset raw-bib)
			 0)))
      (let ((format-params (cons (cons 'max-offset max-offset)
				 (cpr-style-bib-opts-to-formatting-params bib-opts))))
	(cons (funcall bib-formatter
		       (--map (funcall bibitem-formatter
				       (funcall rt-formatter (cpr-rt-cull-spaces-puncts it))
				       format-params)
			      substituted)
		       format-params)
	      format-params)))))

(provide 'citeproc)

;;; citeproc.el ends here
