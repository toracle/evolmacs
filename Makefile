.PHONY: all test clean compile lint package install coverage

ELDEV = eldev
EMACS ?= emacs

all: compile

test:
	$(ELDEV) test

compile:
	$(ELDEV) compile

clean:
	$(ELDEV) clean all
	rm -rf coverage coverage-final.json

lint:
	$(ELDEV) lint

package:
	$(ELDEV) package

install:
	$(ELDEV) install

# Coverage report - placeholder for future implementation
# We'll add proper undercover.el integration as a follow-up task
coverage:
	@echo "NOTE: Code coverage with undercover.el will be implemented in a separate PR."
	@echo "Running tests for now."
	$(ELDEV) test
	@echo "TODO: Implement full undercover.el integration with proper dependencies."