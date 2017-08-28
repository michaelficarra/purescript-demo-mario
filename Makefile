default: build
all: build doc

MODULE = Mario

build: dist/$(MODULE).js
deps: bower_components
doc: docs/README.md

SRC = $(shell find src -name '*.purs' -type f | sort)

NPM = $(shell command -v npm || { echo "npm not found."; exit 1; })
PURS = $(shell command -v purs || { echo "PureScript compiler (purs) not found."; exit 1; })

NPM_BIN = $(shell npm bin)
BOWER = $(NPM_BIN)/bower

$(BOWER):
	npm install bower

dist/$(MODULE).js: bower_components $(SRC) $(FFI)
	mkdir -p "$(@D)"
	$(PURS) compile \
		'bower_components/purescript-*/src/**/*.purs' \
		$(SRC) \
		--verbose-errors
	$(PURS) bundle \
		'output/**/*.js' \
		--output dist/$(MODULE).js \
		--module $(MODULE).Main \
		--main $(MODULE).Main

.PHONY: default all build deps doc clean

docs/README.md: bower_components $(SRC)
	mkdir -p '$(@D)'
	$(PURS) docs \
		--docgen $(MODULE) \
		'bower_components/purescript-*/src/**/*.purs'
		$(SRC) >'$@'

node_modules:
	$(NPM) install
	touch -cm node_modules

bower_components: $(BOWER)
	"$(BOWER)" install
	touch -cm bower_components

clean:
	rm -rf dist bower_components node_modules .psci_modules output
