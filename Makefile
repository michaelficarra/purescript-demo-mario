default: build
all: build doc

MODULE = Mario

build: dist/$(MODULE).js
deps: bower_components
doc: docs/README.md

SRC = $(shell find src -name '*.purs' -type f | sort)

NPM = $(shell command -v npm || { echo "npm not found."; exit 1; })
PSC = $(shell command -v psc || { echo "PureScript compiler (psc) not found."; exit 1; })
PSCDOCS = $(shell command -v psc-docs || { echo "Purescript doc generator (psc-docs) not found."; exit 1; })
PSCBUNDLE = $(shell command -v psc-bundle || { echo "Purescript bundler (psc-bundle) not found."; exit 1; })

NPM_BIN = $(shell npm bin)
BOWER = $(NPM_BIN)/bower

$(BOWER):
	npm install bower

dist/$(MODULE).js: bower_components $(SRC) $(FFI)
	mkdir -p "$(@D)"
	$(PSC) \
		'bower_components/purescript-*/src/**/*.purs' \
		$(SRC) \
		--verbose-errors
	$(PSCBUNDLE) \
		'output/**/*.js' \
		--output dist/$(MODULE).js \
		--module $(MODULE).Main \
		--main $(MODULE).Main

.PHONY: default all build deps doc clean

docs/README.md: bower_components $(SRC)
	mkdir -p '$(@D)'
	$(PSCDOCS) \
		--docgen $(MODULE) \
		'bower_components/purescript-*/src/**/*.purs'
		$(SRC) >'$@'

node_modules:
	$(NPM) install
	touch -cm node_modules

bower_components: $(BOWER)
	$(BOWER) install
	touch -cm bower_components

clean:
	rm -rf dist bower_components node_modules .psci_modules output
