;;; citeproc-test-csl-suite.el --- CSL tests from the official suite

(require 'citeproc-test-human)

(citeproc-test-human-create-from-dir
 "./test/suite/processor-tests/humans" "./test/expected_fails.lst" "suite")

(provide 'citeproc-test-csl-suite)

;;; citeproc-test-csl-suite.el ends here
