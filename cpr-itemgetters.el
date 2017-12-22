;; cpr-itemgetters.el --- functions for constructing itemgetters -*- lexical-binding: t; -*-

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

;; Functions for constructing various types of bibliographic itemgetter functions. The
;; returned itemgetter functions can, in turn, be used to create `cpr-style' and
;; `cpr-proc' structures.

;;; Code:

(require 'dash)

(require 'json)
(require 'bibtex)
(require 'cpr-bibtex)

(defun cpr-hash-itemgetter-from-csl-json (file)
  "Return a hash-based getter for csl json bibliography FILE."
  (let* ((json-array-type 'list)
	 (json-key-type 'symbol)
	 (item-list (json-read-file file))
	 (hash (make-hash-table :test 'equal)))
    (--each item-list
      (puthash (alist-get 'id it) it hash))
    (lambda (itemids) (--map (cons it (gethash it hash))
			     itemids))))

(defun cpr-itemgetter-from-csl-json (file)
  "Return an item-getter for csl json bibliography FILE."
  (lambda (itemids)
    (let* ((json-array-type 'list)
	   (json-key-type 'symbol)
	   (item-list (json-read-file file))
	   result)
      (dolist (item item-list result)
	(let ((id (alist-get 'id item)))
	  (when (member id itemids)
	    (push (cons id item) result)))))))

(defun cpr-itemgetter-from-bibtex (file)
  "Return a getter for a BibTeX bibliography FILE."
  (lambda (itemids)
    (let (result)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(bibtex-set-dialect 'BibTeX t)
	(bibtex-map-entries
	 (lambda (key _beg _end)
	   (when (member key itemids)
	     (push (cons key (cpr-bt-entry-to-csl (bibtex-parse-entry)))
		   result)))))
      result)))

(provide 'cpr-itemgetters)

;;; cpr-itemgetters.el ends here
