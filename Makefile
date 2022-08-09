SCRIPTS_DIR := scripts

.PHONY: build
build: install
	daml build
	cd $(SCRIPTS_DIR) && ./build.sh

.PHONY: install
install:
	./$(SCRIPTS_DIR)/get-dependencies.sh daml.yaml

.PHONY: clean
clean:
	daml clean
	./$(SCRIPTS_DIR)/remove-dependencies.sh daml.yaml
	cd scripts && ./clean.sh

.PHONY: test
test: build
	daml test
	cd $(SCRIPTS_DIR) && ./test.sh

.PHONY: headers-check
headers-check:
	./scripts/dade-copyright-headers.py check

.PHONY: headers-update
headers-update:
	./scripts/dade-copyright-headers.py update

DAML_SRC:=$(shell find src/main/daml -name '*.daml')

.PHONY: doc
doc: $(DAML_SRC)
	daml damlc docs --format html \
    --exclude-instances=HasField,HasImplementation,HasMethod,HasFromInterface,HasToInterface \
    --drop-orphan-instances \
    --output .docs $(DAML_SRC)

.PHONY: docjson
docjson: $(DAML_SRC)
	daml damlc docs --output=.docs/daml-finance.json --package-name=daml-finance --format Json \
    $(DAML_SRC)
