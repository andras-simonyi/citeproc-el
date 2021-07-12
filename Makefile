CASK ?= cask
CASK_EXEC ?= ${CASK} exec

all: update clean compile csl_suite

update:
	$(CASK) update

compile:
	$(CASK) build

test_csl_suite:
	${CASK_EXEC} ert-runner test/citeproc-test-csl-suite.el

test_internals:
	${CASK_EXEC} ert-runner test/citeproc-test-int*.el

test:
	${CASK_EXEC} ert-runner test/*.el

clean:
	rm -f *.elc

.PHONY: all clean test
