all: test

test:
	cask exec ecukes --win

.PHONY: test
