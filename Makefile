default: build doc
all: build doc test

MODULE = Mario

build: lib/$(MODULE).js
build-tests: $(TESTSOUT)
externs: lib/$(MODULE).externs.purs
deps: node_modules bower_components
doc: docs/README.md


BOWER_DEPS = $(shell find bower_components/purescript-*/src -name '*.purs' -type f | sort)
SRC = $(shell find src -name '*.purs' -type f | sort)
TESTS = $([ -d test ] && shell find test -name '*.purs' -type f | sort)
TESTSOUT = $(TESTS:test/%.purs=built-tests/%.js)

BOWER = node_modules/.bin/bower
ISTANBUL = node_modules/.bin/istanbul
MOCHA = node_modules/.bin/_mocha
MOCHA_OPTS = --inline-diffs --check-leaks -R dot

lib/$(MODULE).js: bower_components src/$(MODULE).purs
	@mkdir -p '$(@D)'
	psc --verbose-errors \
	  --module $(MODULE) \
	  --main $(MODULE) \
	  ${BOWER_DEPS} $(SRC) \
	  > lib/$(MODULE).js

.PHONY: default all build externs deps doc clean test build-tests

lib/$(MODULE).externs.purs: bower_components src/$(MODULE).purs
	@mkdir -p '$(@D)'
	psc --verbose-errors \
	  --module $(MODULE) \
	  --codegen $(MODULE) \
	  --externs lib/$(MODULE).externs.purs \
	  ${BOWER_DEPS} $(SRC) \
	  > /dev/null

docs/README.md: lib/$(MODULE).externs.purs
	@mkdir -p '$(@D)'
	docgen lib/$(MODULE).externs.purs > docs/README.md

built-tests/%.js: bower_components test/%.purs
	@mkdir -p '$(@D)'
	psc --verbose-errors -m Tests \
	  $(BOWER_DEPS) '$<' \
	  >'$@'

node_modules:
	npm install

bower_components: node_modules
	$(BOWER) install

test: node_modules $(TESTSOUT) lib/$(MODULE).js
	[ -d test ] && $(ISTANBUL) cover --root lib $(MOCHA) -- $(MOCHA_OPTS) -- built-tests

clean:
	rm -rf lib built-tests coverage bower_components node_modules
