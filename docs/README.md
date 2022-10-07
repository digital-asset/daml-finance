# Daml Finance Documentation

Theme is copied from Daml Docs theme.

## Pre-requisites

### Python

1. Open a terminal window.

2. Install `pyenv` for Python version management with `brew install pyenv`.

3. Install `pipx` with `brew install pipenv`. This should install the `pipenv` binary for virtual
   environments management.

4. Navigate to the root folder of the repository.

5. Run `pipenv install` to create a virtual environment and download the required packages.

### Ruby

`Ruby` must be installed on your machine (`brew install ruby`).

### Node

`Node` and `yarn` must be installed on your machine.

### Sass

The `ruby` version of `sass` must be installed: `gem install sass`.

### Grunt

If grunt is not already installed: `npm install -g grunt-cli`.

If you had to install anything new above, but it still does not work, you may have to recreate the
virtual environment: run `pipenv install` again in the root folder of the repository.

## Building the docs

1. Navigate to the root folder of the repository.

2. `make build-all` to build the Daml Finance `dar`s.

3. `pipenv shell` to enter the virtual environment.

4. `make doc-html` to build the HTML documentation.

## Viewing the docs locally

1. Navigate to the root folder of the repository.

2. Start a local web server on `http://localhost:8000/` by running `./docs/scripts/preview.sh`. In case an
   old/cached html document is being shown, make sure to refresh your web browser.

Note - The table of contents (the menu on the left of the UI) will show more drop down links than what it'll show on `docs.daml.com`
(ie., it is displaying anchor links).
When making modifications to the table of contents, ensure to test these changes as part of `docs.daml.com` repo's build.

### If you are doing many changes to the docs

It is useful to do the two parts above in two separate terminal windows. That way, you only have to
re-run `make doc-html` in the first terminal every time you want to see your changes in the browser.
The second terminal window can keep running the web server, which will automatically serve the
updated document.

### If you have renamed an .rst file

Unfortunately, if you rename an .rst file it seems `make doc-html` does not pick this up. It will
still build the document structure using old file names / content. In order to solve this, first
run `make doc-clean` and then `make doc-html` again.

### If you change menu names / structure

The same goes for changes to the documentation menu in index.rst. If you change the
structure (e.g. introducing sub-sections) or rename a heading (e.g. Tutorial -> Tutorials)
`make doc-html` does not pick this up. First run `make doc-clean` and then `make doc-html` again.

## Publishing the docs on GitHub

The website is currently hosted on
[Github pages](https://digital-asset.github.io/daml-finance/).

In order to update it, you need to

- build the docs from the desired branch as outlined above
- copy the content of `docs/build/html` to a temporary folder `temp`
- switch to the `github-pages` branch
- delete the content of the `docs` folder, except for the `.nojekyll` file (e.g. go to the docs folder and run `find . ! -name .nojekyll -delete`)
- copy the content of `temp` into `docs` (notice the difference in folder structure: in the `main` branch the html files are located in `docs/build/html`, but in the `github-pages` branch the html files must be in `docs`)
- commit and push to remote (wait a minute or so before reloading the [Github pages](https://digital-asset.github.io/daml-finance/)

## Publishing to docs.daml.com

In order to publish to the main documentation website at [docs.daml.com](http://docs.daml.com), we need to execute the following steps :

1. Update the `version` in [daml.yaml](daml.yaml)
2. Create a branch beginning with `assembly` or `Assembly`
3. Log into [CircleCi](https://app.circleci.com/) and navigate to the Daml-Finance [pipeline](https://app.circleci.com/pipelines/github/digital-asset/daml-finance)
4. Navigate to the branch that you created in step 2 with the CircleCi workflow `assembly`
5. Once CircleCI has successfully built both `build` and `docs` steps, an approval step named `hold` will be enabled - select the step and press `Approve`
6. The next step named `assembly` will start processing. This step
   1. Runs the script `docs/scripts/build-assembly.sh` which :
      1. Takes the documentation source at `docs/source`, the build output at `docs/build/daml-finance-rst` and the `src` directory and places them into a folder at `docs/.assembly` with the expected directory structure
      2. Updates the directory paths
   2. Creates a tarball file
   3. Uploads this tarball to Artifactory (Note - if the target Daml SDK version already exists for Daml-Finance, this step will fail with a 403 http error code)
7. In the docs.daml.com [repo](https://github.com/digital-asset/docs.daml.com), follow the instructions [here](https://github.com/digital-asset/docs.daml.com/blob/main/README.md#making-changes-to-the-next-unreleased-version) to update the `daml-finance` version.

Note - the "Getting Started" code sample [here](docs/code-samples/getting-started) is part of the `daml` assembly build. The code samples in the `daml` repo are located [here](https://github.com/digital-asset/daml/tree/main/templates/quickstart-finance) in the `daml` repo. As such, the build process of the `docs.daml.com` updates the references contained in the various RST files of `Daml-Finance` that references these templates.
