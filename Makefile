STACK       = stack

.PHONY: all test clean ghcid distclean

all: test

test:   clean
	$(STACK) test