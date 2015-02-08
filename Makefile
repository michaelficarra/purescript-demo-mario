default: build
all: build externs doc test

MODULE = Mario

build: lib/$(MODULE).js
build-tests: $(TESTSOUT)
externs: lib/$(MODULE).externs.purs
deps: node_modules bower_components
doc: docs/README.md

BOWER_DEPS = $(shell find bower_components/purescript-*/src -name '*.purs' -type f | sort)
SRC = $(shell find src -name '*.purs' -type f | sort)
TESTS = $(shell [ -d test ] && find test -name '*.purs' -type f | sort)
TESTSOUT = $(TESTS:test/%.purs=built-tests/%.js)

BOWER = node_modules/.bin/bower
ISTANBUL = node_modules/.bin/istanbul
MOCHA = node_modules/.bin/_mocha
MOCHA_OPTS = --inline-diffs --check-leaks --reporter dot
NPM = $(shell command -v npm || { echo "npm not found."; exit 1; })
PSC = $(shell command -v psc || { echo "PureScript compiler (psc) not found."; exit 1; })
PSCDOCS = $(shell command -v psc-docs || command -v docgen)

lib/$(MODULE).js: bower_components $(SRC)
	@mkdir -p '$(@D)'
	$(PSC) --verbose-errors \
	  --module Main --module $(MODULE) \
	  $(BOWER_DEPS) $(SRC) \
	  > lib/$(MODULE).js

.PHONY: default all build externs deps doc clean test build-tests

lib/$(MODULE).externs.purs: bower_components $(SRC)
	@mkdir -p '$(@D)'
	$(PSC) --verbose-errors \
	  --module Main --module $(MODULE) \
	  --externs lib/$(MODULE).externs.purs \
	  $(BOWER_DEPS) $(SRC) \
	  > /dev/null

docs/README.md: lib/$(MODULE).externs.purs
	@mkdir -p '$(@D)'
	$(PSCDOCS) lib/$(MODULE).externs.purs >'$@'

built-tests/%.js: test/%.purs bower_components
	@mkdir -p '$(@D)'
	$(PSC) --verbose-errors --module Tests \
	  $(BOWER_DEPS) '$<' \
	  >'$@'

node_modules:
	$(NPM) install

bower_components: node_modules
	$(BOWER) install

test: node_modules $(TESTSOUT) lib/$(MODULE).js
	@mkdir -p test
	$(ISTANBUL) cover --root lib $(MOCHA) -- $(MOCHA_OPTS) -- $(TESTSOUT)

clean:
	rm -rf lib built-tests coverage bower_components node_modules
