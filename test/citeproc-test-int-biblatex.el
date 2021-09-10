;;; citeproc-test-int-biblatex.el --- biblatex tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'yaml)
(require 'string-inflection)
(require 'ht)
(require 'parsebib)

(require 'citeproc-biblatex)
(require 'citeproc-lib)

(defun citeproc-test-blt--parse (file)
  "Parse a biblatex->CSL test file.
Returns a (BLT . CSL) pair where BLT is the `parsebib'-parsed
form of the biblatex part and CSL is the `yaml'-parsed form of
the CSL part."
  (let (blt csl blt-start blt-end blt-string)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char 1)
      (re-search-forward "---" nil)
      (forward-line)
      (move-beginning-of-line nil)
      (setq blt-start (point))
      (re-search-forward "---" nil)
      (forward-line -1)
      (move-end-of-line nil)
      (setq blt-end (point)
	    blt-string (buffer-substring-no-properties blt-start blt-end))
      (with-temp-buffer
	(insert blt-string)
	(goto-char 1)
	(setq blt (ht->alist
		   (car (parsebib-parse-bib-buffer :expand-strings t :inheritance t)))))
      (forward-line 2)
      (move-beginning-of-line nil)
      (setq csl (yaml-parse-string
		 (buffer-substring-no-properties (point) (point-max))
		 :object-type 'alist
		 :sequence-type 'list))
      (cons blt csl))))

(defun citeproc-test-blt--sort-alist (alist)
  "Sort alist A according to key alphabetic order."
  (sort alist (lambda (x y) (string< (car x) (car y)))))

(defun citeproc-test-blt--sort-nparts (csl-entry)
  "Sort the name-parts in a CSL entry."
  (dolist (kv csl-entry)
    (when (memq (car kv) citeproc--name-vars) 
      (let* ((names (cdr kv))
	     (sorted-names (mapcar #'citeproc-test-blt--sort-alist names)))
	(setf (cdr kv) sorted-names))))
  csl-entry)

(defun citeproc-test-blt--simplify-dates (csl-entry)
  "Convert date entries to readable, simple fields"
  (dolist (kv csl-entry)
    (when (memq (car kv) citeproc--date-vars) 
      (let* ((interval-date-parts (cdadr kv))
	     (simplified-int-parts
	      (mapconcat
	       (lambda (int-part)
		 (mapconcat
		  (lambda (x)
		    (let ((str (number-to-string x)))
		      (if (= (length str) 1)
			  (concat "0" str)
			str)))
		  int-part "-"))
	       interval-date-parts "/")) )
	(setf (cdr kv) simplified-int-parts))))
  csl-entry)

(defun citeproc-test-blt--stringify (alist)
  "Convert all numerical values to strings."
  (dolist (kv alist)
    (when (numberp (cdr kv))
      (setf (cdr kv) (number-to-string (cdr kv)))))
  alist)

(defun citeproc-test-blt--normalize-csl-entries (entries)
  "Normalize a list of csl entries.
Performs the following steps:
- sorts the entries according to the id field,
- sorts the kv pairs according to key inside entries,
- converts numerical values to strings,
- sorts name-parts according to keys.
All sorts are alphanumerically increasing."
  (let* ((stringified (mapcar #'citeproc-test-blt--stringify entries))
	 (id-sorted (sort stringified
			  (lambda (x y)
			    (string< (alist-get 'id x)
				     (alist-get 'id y)))))
	 (internally-sorted (mapcar #'citeproc-test-blt--sort-alist id-sorted)))
    (mapcar #'citeproc-test-blt--sort-nparts internally-sorted)))

(defun citeproc-test-blt--output (blt)
  "Generate normalized output from biblatex parse BLT."
  (let* ((processed (mapcar (lambda (x)
			      (let ((csl-entry (citeproc-blt-entry-to-csl (cdr x) t)))
				(push (cons 'id (car x)) csl-entry)
				(citeproc-test-blt--simplify-dates 
				 csl-entry)))
			    blt)))
    (citeproc-test-blt--normalize-csl-entries processed)))

(defun citeproc-test-blt-create-from-file (file)
  "Create an ERT test from a blt-csl mapping file."
  (pcase-let* ((`(,blt . ,csl)
		(citeproc-test-blt--parse file))
	       (expected (citeproc-test-blt--normalize-csl-entries csl))
	       (file-name (f-filename file))
	       (test-name (intern
			   (concat "citeproc-int-blt-"
				   (string-inflection-kebab-case-function
				    (substring file-name 0 -4))))))
    (eval `(ert-deftest ,test-name ()
	     (should (equal
		      ',expected
		      (citeproc-test-blt--output ',blt)))))))

(defun citeproc-test-blt-create-from-dir (dir)
  "Create all blt-csl mapping tests from DIR."
  (dolist (test-file (f-glob (concat dir "/*.txt")))
    (citeproc-test-blt-create-from-file test-file)))

(citeproc-test-blt-create-from-dir "./test/biblatex_csl")

(provide 'citeproc-test-int-biblatex)

;;; citeproc-test-int-biblatex.el ends here
