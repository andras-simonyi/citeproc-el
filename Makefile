CASK ?= cask
CASK_EXEC ?= ${CASK} exec

all: update clean compile test

update:
	$(CASK) update

compile:
	$(CASK) build

test:
	${CASK_EXEC} ert-runner test/citeproc-test.el

clean:
	rm -f *.elc

.PHONY: all clean test
