EMACS ?= emacs

.PHONY: test test-core test-cache test-effects test-strategy byte-compile clean autoloads lint checkdoc compile-strict qa format-tabs format-docstrings format-comments fix-footers fix-local-vars autofix


# Run all tests
test: clean test-core test-cache test-effects test-strategy

# Individual test suites
test-core:
	$(EMACS) -Q --batch -L lisp -l ert -l test/shaoline-core-test.el -f ert-run-tests-batch-and-exit

test-cache:
	$(EMACS) -Q --batch -L lisp -l ert -l test/shaoline-cached-segment-test.el -f ert-run-tests-batch-and-exit

test-effects:
	$(EMACS) -Q --batch -L lisp -l ert -l test/shaoline-effects-test.el -f ert-run-tests-batch-and-exit

test-strategy:
	$(EMACS) -Q --batch -L lisp -l ert -l test/shaoline-strategy-test.el -f ert-run-tests-batch-and-exit

# Compilation and packaging
byte-compile:
	$(EMACS) -Q --batch -L lisp -f batch-byte-compile lisp/*.el

autoloads:
	$(EMACS) -Q --batch -L lisp --eval "(progn (require 'package) (loaddefs-generate \"lisp\" \"shaoline-autoloads.el\"))"

clean:
	rm -f lisp/*.elc shaoline-autoloads.el lisp/*autoloads.el

install: byte-compile autoloads test

# Coverage and debugging
test-debug:
	$(EMACS) -Q --batch -L lisp --eval "(setq debug-on-error t)" -l ert -l test/shaoline-core-test.el -f ert-run-tests-batch-and-exit

test-individual:
	@echo "Run individual test with: make test-individual TEST=test-name"
	$(EMACS) -Q --batch -L lisp -l ert -l test/shaoline-core-test.el --eval "(ert '$(TEST))"

lint:
	$(EMACS) -Q --batch -l scripts/lint.el -f shaoline-package-lint-batch

checkdoc:
	$(EMACS) -Q --batch -l scripts/checkdoc-elisp.el -f shaoline-checkdoc-elisp-lint

compile-strict:
	$(EMACS) -Q --batch -l scripts/compile-strict.el -f shaoline-byte-compile-strict

format-tabs:
	$(EMACS) -Q --batch -l scripts/format-tabs.el -f shaoline-format-tabs

format-docstrings:
	$(EMACS) -Q --batch -l scripts/fix-docstrings.el -f shaoline-fix-docstrings

format-comments:
	$(EMACS) -Q --batch -l scripts/fill-comments.el -f shaoline-fill-comments

fix-footers:
	$(EMACS) -Q --batch -l scripts/fix-footers.el -f shaoline-fix-footers

fix-local-vars:
	$(EMACS) -Q --batch -l scripts/fix-local-vars.el -f shaoline-check-local-vars

# One-shot automatic formatting helpers
autofix: format-docstrings format-comments fix-footers

# Full QA sweep: clean build, tests, lint, checkdoc and warning-free byte-compile
qa: clean byte-compile test lint checkdoc compile-strict
