default: build
all: build externs doc

MODULE = Mario

build: lib/$(MODULE).js
externs: lib/$(MODULE).externs.purs
deps: bower_components
doc: docs/README.md

BOWER_DEPS = $(shell find bower_components/purescript-*/src -name '*.purs' -type f | sort)
SRC = $(shell find src -name '*.purs' -type f | sort)

NPM = $(shell command -v npm || { echo "npm not found."; exit 1; })
PSC = $(shell command -v psc || { echo "PureScript compiler (psc) not found."; exit 1; })
PSCDOCS = $(shell command -v psc-docs || command -v docgen)

NPM_BIN = $(shell npm bin)
BOWER = $(NPM_BIN)/bower

$(BOWER):
	npm install bower

lib/$(MODULE).js: bower_components $(SRC)
	@mkdir -p '$(@D)'
	$(PSC) --verbose-errors \
	  --module Main --module $(MODULE) \
	  $(BOWER_DEPS) $(SRC) \
	  --output lib/$(MODULE).js

.PHONY: default all build externs deps doc clean

lib/$(MODULE).externs.purs: bower_components $(SRC)
	@mkdir -p '$(@D)'
	$(PSC) --verbose-errors \
	  --module Main --module $(MODULE) \
	  --codegen Main --codegen $(MODULE) \
	  --externs lib/$(MODULE).externs.purs \
	  $(BOWER_DEPS) $(SRC) \
	  > /dev/null

docs/README.md: lib/$(MODULE).externs.purs
	@mkdir -p '$(@D)'
	$(PSCDOCS) lib/$(MODULE).externs.purs >'$@'

node_modules:
	$(NPM) install
	touch -cm node_modules

bower_components: $(BOWER)
	$(BOWER) install
	touch -cm bower_components

clean:
	rm -rf lib coverage bower_components node_modules .psci_modules
