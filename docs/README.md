# Daml Finance Documentation

Theme is copied from Daml Docs theme.

## Pre-requisites

### Python

1. Open a terminal window.

2. Install `pyenv` for Python version management with `brew install pyenv`.

3. Install `pipx` with `brew install pipenv`. This should install the `pipenv` binary for virtual environments management.

4. Navigate to the root folder of the repository.

5. Run `pipenv install` to create a virtual environment and download the required packages.

### Ruby

`Ruby` must be installed on your machine (`brew install ruby`).

### Node

`Node` and `yarn` must be installed on your machine.

## Building the docs

1. Navigate to the root folder of the repository.

2. `pipenv shell` to enter the virtual environment.

3. `make doc-html` to build the HTML documentation.

## Viewing the docs locally

1. Navigate to the root folder of the repository.

2. `./docs/preview.sh`

## If you are doing many changes to the docs

It is useful to do the two parts above in two separate terminal windows. That way, you only have to re-run `make doc-html` in the first terminal every time you want to see your changes in the browser. The second terminal window can keep running the web server, which will automatically serve the updated document.

## If you have renamed an .rst file

Unfortunately, if you rename an .rst file seems `make doc-html` does not pick this up.
It will still build the document structure using old file names / content.
In order to solve this, first run `make doc-clean` and then `make doc-html` again.

## If you change menu names / structure

The same goes for changes to the documentation menu in index.rst. If you change the
structure (e.g. introducing sub-sections) or rename a heading (e.g. Tutorial -> Tutorials)
`make doc-html` does not pick this up. First run `make doc-clean` and then `make doc-html` again.
