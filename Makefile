.PHONY: all test clean compile lint package install

ELDEV = eldev

all: compile

test:
	$(ELDEV) test

compile:
	$(ELDEV) compile

clean:
	$(ELDEV) clean all

lint:
	$(ELDEV) lint

package:
	$(ELDEV) package

install:
	$(ELDEV) install