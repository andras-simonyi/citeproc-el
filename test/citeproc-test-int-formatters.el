;;; citeproc-test-int-formatters.el --- formatter tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'citeproc)
(require 'citeproc-formatters)

(ert-deftest citeproc-test-formatters-html ()
  (let ((f (citeproc-formatter-rt
	     (citeproc-formatter-for-format 'html))))
    (should (string= (funcall f '(((href . "http://orgmode.org")) "Org website"))
		     "<a href=\"http://orgmode.org\">Org website</a>"))))

(ert-deftest citeproc-test-formatters-org ()
  (let ((f (citeproc-formatter-rt
	    (citeproc-formatter-for-format 'org))))
    (should (string= (funcall f '(((href . "http://orgmode.org")) "Org website"))
		     "[[http://orgmode.org][Org website]]"))
    (should (string= (funcall f '(((href . "http://orgmode.org")) "http://orgmode.org"))
		     "http://orgmode.org"))
    (should (string= (funcall f '(((bib-item-no . "1")) "text"))
		     "<<citeproc_bib_item_1>>text"))))

(ert-deftest citeproc-test-formatters-plain ()
  (let ((f (citeproc-formatter-rt
	     (citeproc-formatter-for-format 'plain))))
    (should (string= (funcall f '(((href . "http://orgmode.org")) "Org website"))
		     "Org website"))))

(ert-deftest citeproc-test-formatters-latex ()
  (let ((f (citeproc-formatter-rt
	    (citeproc-formatter-for-format 'latex))))
    (should (string= (funcall f '(((href . "http://orgmode.org")) "Org website"))
		     "\\href{http://orgmode.org}{Org website}"))
    (should (string=
	     (funcall f
	      '(((href . "https://scholar.google.com/scholar?q=%22functional+data%22"))
		"https://scholar.google.com/scholar?q=%22functional+data%22"))
	     "\\url{https://scholar.google.com/scholar?q=\\%22functional+data\\%22}"))))

(ert-deftest citeproc-test-formatters-csl ()
  (let ((f (citeproc-formatter-rt
	     (citeproc-formatter-for-format 'csl-test))))
    (should (string= (funcall f '(((href . "http://orgmode.org")) "Org website"))
		     "Org website"))))

(provide 'citeproc-test-int-formatters)

;;; citeproc-test-int-formatters.el ends here
