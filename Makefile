SCRIPTS_DIR := scripts

##########################
# Project Source (./src) #
##########################

.PHONY: install
install:
	./$(SCRIPTS_DIR)/get-dependencies.sh daml.yaml

.PHONY: build
build: install
	daml build

.PHONY: test
test: build
	daml test

.PHONY: clean
clean:
	-rm -rf .lib/
	daml clean

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
	daml codegen java -o .dars/.java .dars/*

.PHONY: build-js-packages
build-js-packages: build-packages
	daml codegen js -o .dars/.js .dars/*

.PHONY: test-packages
test-packages: build-packages
	./$(SCRIPTS_DIR)/test-packages.sh

.PHONY: validate-packages
validate-packages: build-packages
	./$(SCRIPTS_DIR)/validate-packages.sh

.PHONY: update-data-dependencies-packages
update-data-dependencies-packages:
	packell data-dependencies update -f
	make headers-update

###############################
# Project Source and Packages #
###############################

.PHONY: build-all
build-all: build build-packages

.PHONY: test-all
test-all: test test-packages

.PHONY: clean-all
clean-all: clean clean-packages
	pipenv run make doc-clean

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
		--run 'daml codegen java -o .dars/.java .dars/*'

.PHONY: ci-build-js
ci-build-js:
	@nix-shell \
		--pure \
		--run 'daml codegen js -o .dars/.js .dars/*'

.PHONY: ci-test
ci-test:
	@nix-shell \
		--pure \
		--run 'daml test; ./$(SCRIPTS_DIR)/test-packages.sh'

.PHONY: ci-validate
ci-validate:
	@nix-shell \
		--pure \
		--run './$(SCRIPTS_DIR)/validate-packages.sh'

.PHONY: ci-docs
ci-docs: $(DAML_SDK_ROOT)
	@nix-shell \
		--pure \
		--run 'pipenv run make doc-html'

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

.PHONY: ci-data-dependencies
ci-data-dependencies:
	@nix-shell \
		--pure \
		--run 'export LANG=C.UTF-8; packell data-dependencies validate'

.PHONY: ci-local
ci-local: clean-all ci-headers-check ci-data-dependencies ci-build ci-build-java ci-build-js ci-test ci-validate ci-docs

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

DAML_SRC := $(shell find src/main/daml -name '*.daml')
SDK_VERSION := $(shell yq e '.sdk-version' daml.yaml)
DAML_ROOT := $(shell if [ -z ${DAML_HOME} ]; then echo ~/.daml; else echo ${DAML_HOME}; fi)

.PHONY: doc-code-json
doc-code-json: $(DAML_SRC)
	daml damlc docs \
		--output=docs/build/daml-finance.json \
		--package-name=daml-finance \
		--format Json \
    $(DAML_SRC)

.PHONY: doc-code
doc-code: doc-code-json
	daml damlc docs \
		--output=docs/build/daml-finance-rst \
		--input-format=json \
		--format=Rst \
		--exclude-instances=HasField,HasImplementation,HasFromInterface,HasToInterface,HasInterfaceView,HasExercise,HasExerciseGuarded,HasFromAnyChoice,HasToAnyChoice \
		--drop-orphan-instances \
		--template=docs/code-documentation-templates/base-rst-template.rst \
		--index-template=docs/code-documentation-templates/base-rst-index-template.rst \
		--base-url=https://docs.daml.com/daml/daml-finance \
		--input-anchor=$(DAML_ROOT)/sdk/$(SDK_VERSION)/damlc/resources/daml-base-anchors.json \
		docs/build/daml-finance.json

# Build doc theme
.PHONY: doc-theme
doc-theme:
	cd docs/sphinx && ./build-doc-theme.sh

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS  ?= -c "$(CONFDIR)" -W
SPHINXBUILD ?= sphinx-build
SOURCEDIR   = docs/source
BUILDDIR    = docs/build
CONFDIR     = docs/sphinx

.PHONY: doc-html
doc-html: doc-theme doc-code
	$(SPHINXBUILD) -M html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: doc-clean
doc-clean: Makefile
	$(SPHINXBUILD) -M clean "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
