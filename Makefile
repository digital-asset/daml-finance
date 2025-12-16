SCRIPTS_DIR := scripts

##########################
# Project (workspace)    #
##########################

.PHONY: install
install:
	./$(SCRIPTS_DIR)/get-dependencies.sh daml.yaml

.PHONY: build
build: install
	dpm build --all

.PHONY: test
test: build
	dpm test --all

.PHONY: clean
clean:
	-rm -rf .lib/
	dpm clean

#########################
# Packages (./packages) #
#########################

.PHONY: clean-packages
clean-packages:
	./$(SCRIPTS_DIR)/clean-packages.sh

.PHONY: build-packages
build-packages: clean-packages
	./$(SCRIPTS_DIR)/build-packages.sh

.PHONY: build-java-packages
build-java-packages: build-packages
	dpm codegen-java -o .dars/.java .dars/*

.PHONY: build-js-packages
build-js-packages: build-packages
	dpm codegen-js -o .dars/.js .dars/*

.PHONY: test-packages
test-packages: build-packages
	./$(SCRIPTS_DIR)/test-packages.sh

.PHONY: validate-packages
validate-packages: build-packages
	./$(SCRIPTS_DIR)/validate-packages.sh

.PHONY: update-packages
update-packages:
	git fetch
	packell versioning update
	make headers-update
# Remove: packell data-dependencies update

###############################
# Project Source and Packages #
###############################

.PHONY: build-all
build-all: build build-packages

.PHONY: test-all
test-all: build
	make ci-test

.PHONY: clean-all
clean-all: clean clean-packages clean-docs clean-cache

.PHONY: generate-docs
generate-docs: doc-code
	./docs/scripts/generate-docs.sh

.PHONY: validate-generated-docs
validate-generated-docs: doc-code
	./docs/scripts/validate-generated-docs.sh

.PHONY: sphinx-build-generated-docs
sphinx-build-generated-docs:
	sphinx-build -M html ./docs/generated ./docs/.preview -c ./docs/sphinx-config -E

.PHONY: sphinx-preview-generated-docs
sphinx-preview-generated-docs:
	python -m http.server -d ./docs/.preview/html

##################################
# CI                             #
#  - utilises nix                #
#  - avoids unnecessary rebuilds #
##################################

.PHONY: ci-build
ci-build:
	@nix-shell \
		--pure \
		--run 'make build; ./$(SCRIPTS_DIR)/build-packages.sh'

.PHONY: ci-build-java
ci-build-java:
	@nix-shell \
		--pure \
		--run 'dpm codegen-java -o .dars/.java .dars/*'

.PHONY: ci-build-js
ci-build-js:
	@nix-shell \
		--pure \
		--run 'dpm codegen-js -o .dars/.js .dars/*'

# Find all test projects that contain a daml.yaml in BOTH locations:
daml-test-projects := $(shell find package/test -maxdepth 6 -name daml.yaml -exec dirname {} \; 2>/dev/null)

.PHONY: ci-test
ci-test:
	@echo "Running dpm test on all Daml test packages..."
	@nix-shell --pure --run '\
		for proj in $(daml-test-projects); do \
			echo "Testing package: $$proj"; \
			(cd $$proj && dpm test) || exit $$?; \
		done; \
		echo ""; \
		echo "All Daml test packages ran successfully!"; \
	'

.PHONY: ci-validate
ci-validate:
	@nix-shell \
		--pure \
		--run './$(SCRIPTS_DIR)/validate-packages.sh'

.PHONY: ci-docs
ci-docs:
	@nix-shell \
		--pure \
		--run 'make doc-code'

.PHONY: ci-headers-check
ci-headers-check:
	@nix-shell \
		--pure \
		--run './scripts/dade-copyright-headers.py check'

.PHONY: ci-assembly
ci-assembly:
	@nix-shell \
		--pure \
		--run './docs/scripts/build-assembly.sh'

.PHONY: ci-versioning
ci-versioning:
	@nix-shell \
		--pure \
		--run 'export LANG=C.UTF-8; packell versioning validate'

.PHONY: ci-data-dependencies
ci-data-dependencies:
	@nix-shell \
		--pure \
		--run 'export LANG=C.UTF-8; packell data-dependencies validate'

.PHONY: ci-local
ci-local: clean-all ci-headers-check ci-versioning ci-data-dependencies ci-build ci-validate ci-build-java ci-build-js ci-test ci-docs

.PHONY: ci-validate-generated-docs-full
ci-validate-generated-docs-full:
	@nix-shell \
		--pure \
		--run 'make validate-generated-docs sphinx-build-generated-docs'

#########
# Cache #
#########

.PHONY: clean-cache
clean-cache:
	-rm -rf .cache

#####################
# Copyright headers #
#####################

.PHONY: headers-check
headers-check:
	./scripts/dade-copyright-headers.py check

.PHONY: headers-update
headers-update:
	./scripts/dade-copyright-headers.py update

############################
# Documentation Generation #
############################

DAML_SRC := $(shell \
	find package/main/daml \
		-path '*/.daml' -prune -o \
		-name '*.daml' -print)
SDK_VERSION := $(shell yq e '.sdk-version' daml.yaml)
DAML_ROOT := $(shell if [ -z ${DAML_HOME} ]; then echo ~/.daml; else echo ${DAML_HOME}; fi)

DPM_HOME := $(shell if [ -z $${DPM_HOME} ]; then echo $$HOME/.dpm; else echo $${DPM_HOME}; fi)

DOCS_BUILD_DIR := docs/build

.PHONY: doc-code-json
doc-code-json: build
	@mkdir -p $(DOCS_BUILD_DIR)
	dpm docs \
		--combine \
		--package-name=daml-finance \
		--format Json \
		--output=$(DOCS_BUILD_DIR)/daml-finance.json \
		$(DAML_SRC)

.PHONY: doc-code
doc-code: doc-code-json
	dpm docs \
		--output=$(DOCS_BUILD_DIR)/daml-finance-rst \
		--output-hoogle=$(DOCS_BUILD_DIR)/daml-finance-hoogle.txt \
		--input-format=json \
		--format=Rst \
		--exclude-instances=HasField,HasImplementation,HasFromInterface,HasToInterface,HasInterfaceView,HasExercise,HasExerciseGuarded,HasFromAnyChoice,HasToAnyChoice \
		--drop-orphan-instances \
		--template=docs/code-documentation-templates/base-rst-template.rst \
		--index-template=docs/code-documentation-templates/base-rst-index-template.rst \
		--hoogle-template=docs/code-documentation-templates/base-hoogle-template.txt \
		--base-url=https://docs.daml.com/daml-finance/reference/code-documentation/daml-finance-rst \
		--input-anchor=$(DAML_ROOT)/sdk/$(SDK_VERSION)/damlc/resources/daml-base-anchors.json \
		$(DOCS_BUILD_DIR)/daml-finance.json
	@echo "Daml Finance documentation generated successfully"

.PHONY: clean-docs
clean-docs:
	./$(SCRIPTS_DIR)/clean-docs.sh