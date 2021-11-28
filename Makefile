# Frontend to dune.

CURRENT_DIR := $(shell pwd)
BIN_DIR := $(CURRENT_DIR)/_build/install/default/bin
BIN_LIST := micse micse.naive_prover micse.naive_refuter micse.naive_trxpath_refuter
QUIET := > /dev/null

.PHONY: default build install uninstall test clean

default: build

build:
	dune build
	mkdir -p $(CURRENT_DIR)/bin $(QUIET)
	cp -f $(foreach file,$(BIN_LIST),$(BIN_DIR)/$(file)) $(CURRENT_DIR)/bin $(QUIET)

test:
	dune build
	dune runtest -f $(QUIET)

install:
	dune install $(QUIET)

uninstall:
	dune uninstall $(QUIET)

clean:
	dune clean $(QUIET)
	rm -rf ./bin $(QUIET)
# Optionally, remove all files/folders ignored by git as defined
# in .gitignore (-X).
	git clean -dfXq $(QUIET)