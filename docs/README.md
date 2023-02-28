# Daml Finance Documentation

The Daml Finance documentation is hosted on the
[docs.daml.com](https://github.com/digital-asset/docs.daml.com) repository. However, the code
documentation and relevant code snippets are built in this repository and shipped to the rest of
the docs via an `assembly` artifact.

## Building the docs

1. Navigate to the root folder of the repository.

2. `make build-all` to build the Daml Finance `dar`s.

3. `make ci-docs` to build the `rst` documentation of the code.

## Publishing to docs.daml.com

In order to update the assembly package used on the main documentation website at
[docs.daml.com](http://docs.daml.com), we need to execute the following steps:

1. Update the `version` in [daml.yaml](../daml.yaml). Merge this new version into main.
2. Create a branch beginning with `assembly` or `Assembly`. This must be an
   empty branch without any commits in it. Otherwise, the assembly tag will be incorrect.
   You can create a remote branch in the
   [GitHub GUI](https://github.com/digital-asset/daml-finance/branches).
3. Log into [CircleCi](https://app.circleci.com/) and navigate to the Daml-Finance
   [pipeline](https://app.circleci.com/pipelines/github/digital-asset/daml-finance).
4. Navigate to the branch that you created in step 2 with the CircleCi workflow `assembly`.
5. Once CircleCI has successfully built both `build` and `docs` steps, an approval step named
   `hold` will be enabled - select the step and press `Approve`.
6. The next step named `assembly` will start processing. This step:
   1. Runs the script `docs/scripts/build-assembly.sh` which:
      1. Takes the build output at `docs/build/daml-finance-rst` and the `src` directory and places
         it into a folder at `docs/.assembly` with the expected directory structure.
      2. Updates the directory paths.
   2. Creates a tarball file.
   3. Uploads this tarball to Artifactory (Note - if the target Daml SDK version already exists for
      Daml-Finance, this step will fail with a 403 http error code).
7. In the docs.daml.com [repo](https://github.com/digital-asset/docs.daml.com), follow the
   instructions in the [README](https://github.com/digital-asset/docs.daml.com/blob/main/README.md)
   to update the `daml-finance` version.

Note - the "Getting Started" code sample [here](docs/code-samples/getting-started) is part of the
`daml` assembly build. The code samples are copied to
[the `daml` repo](https://github.com/digital-asset/daml/tree/main/templates/quickstart-finance).