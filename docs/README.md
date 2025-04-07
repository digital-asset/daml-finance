# Daml Finance Documentation

The Daml Finance documentation is hosted in the
[docs.daml.com](https://github.com/digital-asset/docs.daml.com) repository. However, the code
documentation and relevant code snippets are built in this repository and shipped to the rest of
the docs via an `assembly` artifact.

## Building the docs

1. Navigate to the root folder of the repository.

2. `make build-all` to build the Daml Finance `dar`s.

3. Write manually-written docs in `docs/source`.

4. `make generate-docs` to create auto-generated docs and combine them with manual docs in `docs/generated`.


