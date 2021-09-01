;;; citeproc-test-int-subbibs.el --- subbib tests -*- lexical-binding: t; -*-

(require 'citeproc-subbibs)

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

(provide 'citeproc-test-int-subbibs)

;;; citeproc-test-int-subbibs.el ends here
