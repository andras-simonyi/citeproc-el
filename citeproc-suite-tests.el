;; citeproc-suite-tests.el --- CSL test suite tests -*- lexical-binding: t; -*-

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

;; Functions to create ERT tests from tests in the official CSL test suite that
;; can be found at <https://github.com/citation-style-language/test-suite>.

;;; Code:

(require 'f)
(require 's)
(require 'json)
(require 'ert)
(require 'string-inflection)

(require 'citeproc)

(defvar citeproc-suite-tests-locale-dir "./test/locales")

(defun citeproc-suite-tests-parse-testfile (file)
  "Return a parsed form of CSL test FILE."
  (let (result
	(json-array-type 'list)
	(json-key-type 'symbol))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char 1)
      (setq result (list (cons 'YEAR-SUFF
			       (re-search-forward "variable=\"year-suffix\"" nil t))))
      (goto-char 1)
      (while (re-search-forward ">>=+ \\{1,2\\}\\([[:graph:]]+\\) =+>>" nil t)
	(let ((section (intern (buffer-substring (nth 2 (match-data))
						 (nth 3 (match-data)))))
	      (start (1+ (point)))
	      end)
	  (re-search-forward "<<=" nil t)
	  (setq end (- (car (match-data)) 1))
	  (push (cons section (pcase section
				('CSL (buffer-substring-no-properties start end))
				((or 'INPUT 'CITATIONS 'CITATION-ITEMS)
				 (goto-char (- start 1)) (json-read))
				(_ (buffer-substring start end))))
		result))))
    result))

(defun citeproc-suite-tests-create-getter (items)
  "Return a getter function for ITEMS.
ITEMS is the parsed representation of the `INPUT' section of a
CSL test."
  (let (result)
    (lambda (itemids)
      (dolist (item items result)
	(let ((id (citeproc-s-from-num-or-s (alist-get 'id item))))
	  (when (member id itemids)
	    (push (cons id item) result)))))))

(defun citeproc-suite-tests-proc-from-style (style parsed-input)
  "Create a processor from STYLE and PARSED-INPUT."
  (citeproc-create style
		   (citeproc-suite-tests-create-getter parsed-input)
		   (citeproc-locale-getter-from-dir citeproc-suite-tests-locale-dir)))

(defun citeproc-suite-tests-proc-from-testfile (file)
  "Create an (itemless) processor from a test FILE."
  (let ((style-string (alist-get 'CSL (citeproc-suite-tests-parse-testfile file)))
	(locale-getter (citeproc-locale-getter-from-dir citeproc-suite-tests-locale-dir)))
    (citeproc-create style-string nil locale-getter)))

(defun citeproc-suite-tests-parse-citation (ct-desc &optional cites-only)
  "Parse test citations description CT-DESC.
Return a list of citation structures. If CITES-ONLY is non-nil
then the input is list of cites."
  (citeproc-citation-create
   :cites (-map #'citeproc-suite-tests-normalize-cite
		(if cites-only ct-desc (alist-get 'citationItems (car ct-desc))))
   :note-index (and (not cites-only)
		    (alist-get 'noteIndex (alist-get 'properties (car ct-desc))))))

(defun citeproc-suite-tests-normalize-cite (cite)
  "Normalize a test CITE."
  (--map (let ((val (cdr it)))
	   (if (numberp val) (cons (car it) (number-to-string val)) it))
	 cite))

(defun citeproc-suite-tests-run-parsed (parsed)
  "Run the parsed CSL test PARSED.
Return the resulting output."
  (-let* (((&alist 'CSL style
		   'INPUT input
		   'MODE mode
		   'CITATION-ITEMS citation-items
		   'CITATIONS citations)
	   parsed)
	  (proc (citeproc-suite-tests-proc-from-style style input)))
    (--each input
      (citeproc-proc-put-item-by-id proc
				    (citeproc-s-from-num-or-s (alist-get 'id it))))
    (when (string= mode "citation")
      (cond
       (citation-items
	(citeproc-append-citations (--map (citeproc-suite-tests-parse-citation it t)
					  citation-items)
				   proc))
       (citations
	(citeproc-append-citations (mapcar #'citeproc-suite-tests-parse-citation citations)
				   proc))
       (t (citeproc-append-citations (list (citeproc-suite-tests-parse-citation input t))
				     proc))))
    (let ((output (if (string= mode "citation")
		      (citeproc-render-citations proc 'csl-test t)
		    (car (citeproc-render-bib proc 'csl-test t)))))
      (if (string= mode "citation") (s-join "\n" output) output))))

(defun citeproc-suite-tests-expected-from-parsed (parsed)
  "Return the expected output of parsed CSL test PARSED."
  (let ((expected (alist-get 'RESULT parsed)))
    (if (or (string= (s-left 5 expected) "..[0]")
	    (string= (s-left 5 expected) ">>[0]"))
	(s-join "\n" (--map (substring it 6)
			    (split-string expected "\n")))
      expected)))

(defun citeproc-suite-tests-create-test-from-file (file expected-fails)
  "Create an ERT test from a CSL test FILE."
  (let* ((parsed (citeproc-suite-tests-parse-testfile file))
	 (expected (citeproc-suite-tests-expected-from-parsed parsed))
	 (file-name (f-filename file))
	 (test-name (intern
		     (concat "citeproc-"
			     (string-inflection-kebab-case-function
			      (substring file-name 0 -4)))))
	 (expected-fail (memq test-name expected-fails)))
    (eval `(ert-deftest ,test-name ()
	     :expected-result ,(if expected-fail :failed :passed)
	     (let ((citeproc-disambiguation-cite-pos 'subsequent))
	       (should (string= ,expected (citeproc-suite-tests-run-parsed ',parsed))))))))

(defun citeproc-suite-tests-read-expected-fails (expected-fails-file)
  "Read the list of tests expected to fail from EXPECTED-FAILS-FILE."
  (let* ((list-as-str (with-temp-buffer
			(insert-file-contents expected-fails-file)
			(buffer-string)))
	 (split (split-string list-as-str "\n")))
    (--map (intern it) (butlast split))))

(defun citeproc-suite-tests-create-from-dir (dir &optional expected-fails-file)
  "Create all CSL suite tests from DIR.
Each file in DIR having the `txt' extension is read as a
human-readable CSL test, and a corresponding ERT test is created.
The created test's name will be constructed by prefixing the
test's filename (without the extension) with `citeproc-'. If the
optional EXPECTED-FAILS-FILE is non-nil then read that file as a
list of tests whose failure is expected. The file should contain
one test-name per line (together with the `citeproc-' prefix)."
  (let ((expected-fails
	 (if expected-fails-file
	     (citeproc-suite-tests-read-expected-fails expected-fails-file)
	   nil)))
    (dolist (test-file (f-glob (concat dir "/*.txt")))
      (citeproc-suite-tests-create-test-from-file test-file expected-fails))))

(provide 'citeproc-suite-tests)

;;; citeproc-suite-tests.el ends here
