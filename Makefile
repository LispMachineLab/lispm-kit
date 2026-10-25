EMACS ?= emacs

FILES = lispm-kit.el lispm-kit-commands.el

.PHONY: test compile

test:
	$(EMACS) -Q --batch -L . -l test/lispm-kit-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(FILES); \
	status=$$?; rm -f *.elc; exit $$status
