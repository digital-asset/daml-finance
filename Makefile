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
	-rm -r .lib/
	daml clean

#########################
# Packages (./packages) #
#########################

.PHONY: build-packages
build-packages:
	./$(SCRIPTS_DIR)/build-packages.sh

.PHONY: test-packages
test-packages: build-packages
	./$(SCRIPTS_DIR)/test-packages.sh

.PHONY: validate-packages
validate-packages: build-packages
	./$(SCRIPTS_DIR)/validate-packages.sh

.PHONY: clean-packages
clean-packages:
	./$(SCRIPTS_DIR)/clean-packages.sh

###############################
# Project Source and Packages #
###############################

.PHONY: build-all
build-all: build build-packages

.PHONY: test-all
test-all: test test-packages

.PHONY: clean-all
clean-all: clean clean-packages

####################################
# CI (avoids unnecessary rebuilds) #
####################################
.PHONY: ci-build
ci-build: build build-packages

.PHONY: ci-test
ci-test:
	daml test
	./$(SCRIPTS_DIR)/test-packages.sh

.PHONY: ci-validate
ci-validate:
	./$(SCRIPTS_DIR)/validate-packages.sh

.PHONY: ci-local
ci-local: clean-all ci-build ci-test ci-validate

#########
# Cache #
#########

.PHONY: clean-cache
clean-cache:
	-rm -r .cache

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

DAML_SRC:=$(shell find src/main/daml -name '*.daml')

.PHONY: doc-code-json
doc-code-json: $(DAML_SRC)
	daml damlc docs \
		--output=docs/build/daml-finance.json \
		--package-name=daml-finance \
		--format Json \
    $(DAML_SRC)

SDK_VERSION:=$(shell yq e '.sdk-version' daml.yaml)

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
		--input-anchor=${HOME}/.daml/sdk/${SDK_VERSION}/damlc/resources/daml-base-anchors.json \
		docs/build/daml-finance.json

# Build doc theme
.PHONY: doc-theme
doc-theme:
	cd docs/sphinx && ./build-doc-theme.sh

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?= -c "$(CONFDIR)" -W
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = docs/source
BUILDDIR      = docs/build
CONFDIR       = docs/sphinx

.PHONY: doc-html
doc-html: doc-theme doc-code
	$(SPHINXBUILD) -M html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: doc-clean
doc-clean: Makefile
	$(SPHINXBUILD) -M clean "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
