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

.PHONY: doc-json
doc-json: $(DAML_SRC)
	daml damlc docs \
		--output=docs/build/daml-finance.json \
		--package-name=daml-finance \
		--format Json \
    $(DAML_SRC)

SDK_VERSION:=$(shell yq e '.sdk-version' daml.yaml)

.PHONY: doc
doc: doc-json
	daml damlc docs \
		--output=docs/build/daml-finance-rst \
		--input-format=json \
		--format=Rst \
		--exclude-instances=HasField,HasImplementation,HasFromInterface,HasToInterface,HasInterfaceView,HasExercise,HasExerciseGuarded,HasFromAnyChoice,HasToAnyChoice \
		--drop-orphan-instances \
		--template=docs/code-documentation-templates/base-rst-template.rst \
		--index-template=docs/code-documentation-templates/base-rst-index-template.rst \
		--base-url=https://docs.daml.com/daml/daml-finance \
		--input-anchor=${HOME}/.daml/sdk/${SDK_VERSION}/damlc/resources/daml-base-anchors.json \
		docs/build/daml-finance.json
