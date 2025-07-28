EMACS ?= emacs

.PHONY: test byte-compile
test:
	$(EMACS) -Q --batch -L lisp -l ert -l test/shaoline-core-test.el -f ert-run-tests-batch-and-exit

byte-compile:
	$(EMACS) -Q --batch -L lisp -f batch-byte-compile lisp/*.el
