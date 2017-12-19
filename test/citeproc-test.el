;;; citeproc-test.el --- Tests for citeproc

(require 'cpr-test-suite)

(cpr-test-suite-create-from-dir "/home/simka/projects/citeproc/tests" "/home/simka/projects/citeproc/tests/expected-fails")

(provide 'citeproc-test)

;;; citeproc-test.el ends here
