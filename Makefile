
DAML_SRC:=$(shell find daml/ContingentClaims -name '*.daml')

doc: $(DAML_SRC)
	daml damlc docs --format html \
    --exclude-instances=HasField \
    --drop-orphan-instances \
    --output doc $(DAML_SRC)
