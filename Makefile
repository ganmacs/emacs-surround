EMACS ?= emacs

test:
	${EMACS} -Q -batch -L . -l test/unit-test.el -f ert-run-tests-batch-and-exit

.PHONY: test
