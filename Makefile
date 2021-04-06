
DAML_SRC:=$(shell find daml/ContingentClaims -name '*.daml')

doc: $(DAML_SRC)
	daml damlc docs --output doc $(DAML_SRC)
