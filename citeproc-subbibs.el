;;; citeproc-subbibs.el --- support for subbibliographies -*- lexical-binding: t; -*-

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

;; Support for generating subbibliographies based on filtering items.

;;; Code:

(require 'dash)

(require 'citeproc-proc)
(require 'citeproc-itemdata)

(defun citeproc-sb-match-p (vv filter &optional use-blt-type)
  "Return whether var-vals alist VV matches FILTER.
If optional USE-BLT-TYPE is non-nil then use the value for key
`blt-type' to evaluate type-based filter parts."
  (let* ((type (alist-get (if use-blt-type 'blt-type 'type) vv))
	 (keyword (alist-get 'keyword vv))
	 (keywords (and keyword (split-string keyword "[ ,;]" t))))
    (--every-p
     (pcase it
       (`(type . ,key) (string= type key))
       (`(nottype . ,key) (not (string= type key)))
       (`(keyword . ,key) (member key keywords)) 
       (`(notkeyword . ,key) (not (member key keywords)))
       (`(,key . ,_) (error "Unsupported Citeproc filter keyword `%s'" key)))
     filter)))

(defun citeproc-sb-add-subbib-info (proc)
  "Add subbibliography information to the items in PROC."
  (let ((filters (citeproc-proc-bib-filters proc)))
    (maphash
     (lambda (_ itemdata)
       (let* ((varvals (citeproc-itemdata-varvals itemdata))
	      (subbib-nos
	       (-non-nil
		(--map-indexed
		 (when (citeproc-sb-match-p varvals it) it-index)
		 filters))))
	 (setf (citeproc-itemdata-subbib-nos itemdata) subbib-nos)))
     (citeproc-proc-itemdata proc))))

(provide 'citeproc-subbibs)

;;; citeproc-subbibs.el ends here
