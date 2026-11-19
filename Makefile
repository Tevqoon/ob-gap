EMACS ?= emacs

.PHONY: all compile test clean

all: compile test

compile:
	$(EMACS) -Q --batch -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile ob-gap.el

test:
	$(EMACS) -Q --batch -L . -L test \
	  -l ob-gap-tests -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
