;;; citeproc-test-int-subbibs.el --- subbib tests -*- lexical-binding: t; -*-

(require 'citeproc)

(require 'ert)

(defvar citeproc-test-int-root-dir  "./test/")

(ert-deftest citeproc-test-int-sb/add-subbib-info ()
  (let* ((ig (citeproc-itemgetter-from-csl-json
	      (concat citeproc-test-int-root-dir "etc/subbibs.json")))
	 (lg (citeproc-locale-getter-from-dir
	      (concat citeproc-test-int-root-dir "locales")))
	 (proc (citeproc-create
		(concat citeproc-test-int-root-dir "etc/chicago-author-date.csl") ig lg)))
    (citeproc-add-uncited '("doe2001" "smith1952" "doe1972") proc)
    (setf (citeproc-proc-bib-filters proc)
	  '(((type . "book"))
	    ((type . "article-journal"))
	    ((keyword . "primary"))
	    ((notkeyword . "primary") (nottype . "article-journal"))))
    (citeproc-proc-finalize proc)
    (citeproc-sb-add-subbib-info proc)
    (should (equal (citeproc-itemdata-subbib-nos
		    (gethash "doe2001" (citeproc-proc-itemdata proc)))
		   '(1 2)))
    (should (equal (citeproc-itemdata-subbib-nos
		    (gethash "smith1952" (citeproc-proc-itemdata proc)))
		   '(0 2)))
    (should (equal (citeproc-itemdata-subbib-nos
		    (gethash "doe1972" (citeproc-proc-itemdata proc)))
		   '(0 3)))))

(ert-deftest citeproc-test-int-sb/end2end ()
  (let* ((ig (citeproc-itemgetter-from-csl-json
	      (concat citeproc-test-int-root-dir "etc/subbibs.json")))
	 (lg (citeproc-locale-getter-from-dir
	      (concat citeproc-test-int-root-dir "locales")))
	 (proc (citeproc-create
		(concat citeproc-test-int-root-dir "etc/chicago-author-date.csl") ig lg)))
    ;; Basic subdivision according to type
    (citeproc-add-uncited '("doe2001" "smith1952" "doe1972") proc)
    (citeproc-add-subbib-filters
     '(((type . "book"))
       ((type . "article-journal")))
     proc)
    (should (equal (car (citeproc-render-bib proc 'plain))
		   '("Doe, Jane. 1972. The Second Book Title.\n\nSmith, Peter. 1952. The Book Title."
		     "Doe, John. 2001. “The Title.” The Mind Scientific 103 (1): 45–58.")))
    (citeproc-clear proc)
    ;; Subsequent author substitution
    (citeproc-add-uncited '("doe1972" "doe1972b" "doe1972c") proc)
    (citeproc-add-subbib-filters
     '(((type . "book"))
       ((keyword . "primary")))
     proc)
    (should (equal (car (citeproc-render-bib proc 'plain))
		   '("Doe, Jane. 1972a. The Second 1972 Jane Doe Book.\n\n———. 1972b. The Second Book Title.\n\n———. 1972c. The Third 1972 Jane Doe Book.\n\nSmith, Peter. 1952. The Book Title."
		     "Doe, Jane. 1972c. The Third 1972 Jane Doe Book.\n\nDoe, John. 2001. “The Title.” The Mind Scientific 103 (1): 45–58.\n\nSmith, Peter. 1952. The Book Title.")))))

(ert-deftest citeproc-test-int-sb/single-filter ()
  (let* ((ig (citeproc-itemgetter-from-csl-json
	      (concat citeproc-test-int-root-dir "etc/subbibs.json")))
	 (lg (citeproc-locale-getter-from-dir
	      (concat citeproc-test-int-root-dir "locales")))
	 (proc (citeproc-create
		(concat citeproc-test-int-root-dir "etc/chicago-author-date.csl") ig lg)))
    (citeproc-add-uncited '("doe1972" "doe1972b" "doe1972c") proc)
    ;; Single empty filter
    (citeproc-add-subbib-filters '(nil) proc)
    (should (equal (car (citeproc-render-bib proc 'plain))
		   '("Doe, Jane. 1972a. The Second 1972 Jane Doe Book.\n\n———. 1972b. The Second Book Title.\n\n———. 1972c. The Third 1972 Jane Doe Book.")))
    (citeproc-clear proc)
    ;; Single non-empty filter
    (citeproc-add-uncited '("doe1972" "doe1972b" "doe1972c" "doe2001") proc)
    (citeproc-add-subbib-filters
     '(((type . "article-journal")))
     proc)
    (should (equal (car (citeproc-render-bib proc 'plain))
		   '("Doe, John. 2001. “The Title.” The Mind Scientific 103 (1): 45–58.")))))

(provide 'citeproc-test-int-subbibs)

;;; citeproc-test-int-subbibs.el ends here
