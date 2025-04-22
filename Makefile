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

# Run tests with code coverage using undercover.el
coverage:
	@echo "Running tests with coverage reporting..."
	@echo "Removing compiled files first..."
	@rm -f evolmacs*.elc
	
	@$(EMACS) -Q --batch -L . -L tests \
		-L .eldev/29.1/packages/dash-2.20.0 \
		-L .eldev/29.1/packages/shut-up-0.3.3 \
		-L .eldev/29.1/packages/undercover-0.8.1 \
		-l tests/run-coverage.el

	@if [ -f "coverage-final.json" ]; then \
		echo "Coverage report generated in coverage-final.json"; \
	else \
		echo "No coverage report was generated. Check the setup."; \
	fi

# Show a summary of the coverage report
coverage-summary:
	@$(EMACS) -Q --batch -L . -L tests \
		-L .eldev/29.1/packages/dash-2.20.0 \
		-L .eldev/29.1/packages/shut-up-0.3.3 \
		-L .eldev/29.1/packages/undercover-0.8.1 \
		-l tests/coverage-summary.el

# Show a CI-friendly summary of the coverage report
coverage-summary-ci:
	@$(EMACS) -Q --batch -L . -L tests \
		-L .eldev/29.1/packages/dash-2.20.0 \
		-L .eldev/29.1/packages/shut-up-0.3.3 \
		-L .eldev/29.1/packages/undercover-0.8.1 \
		-l tests/coverage-summary-ci.el