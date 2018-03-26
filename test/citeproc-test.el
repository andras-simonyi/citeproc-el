;;; citeproc-test.el --- Tests for citeproc

(require 'citeproc-suite-tests)

(citeproc-suite-tests-create-from-dir "./test/suite/processor-tests/humans" "./test/expected_fails.lst")

(provide 'citeproc-test)

;;; citeproc-test.el ends here
