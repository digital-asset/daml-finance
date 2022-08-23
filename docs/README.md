# Daml Finance Documentation

Theme is copied from Daml Docs theme.

## Pre-requisites

1. Open a terminal window.

2. Install `pyenv` for Python version management with `brew install pyenv`.

3. Install `pipx` with `brew install pipenv`. This should install the `pipenv` binary for virtual environments management.

4. Navigate to the `docs` folder.

5. Run `pipenv install` to create a virtual environment and download the required packages.

## Building the docs

1. Navigate to the `docs` folder.

2. `pipenv shell` to enter the virtual environment.

3. `make html` to trigger the Sphinx build.

## Viewing the docs locally

1. `cd build`

2. `python -m http.server 8000 --bind 127.0.0.1`

## If you are doing many changes to the docs

It is useful to do the two parts above in two separate terminal windows. That way, you only have to re-run `make html` in the first terminal every time you want to see your changes in the browser. The second terminal window can keep running the web server, which will automatically serve the updated document.

## If you have renamed an .rst file

Unfortunately, if you rename an .rst file seems `make html` does not pick this up.
It will still build the document structure using old file names / content.
In order to solve this, first run `make clean` and then `make html` again.

## If you change menu names / structure

The same goes for changes to the documentation menu in index.rst. If you change the
structure (e.g. introducing sub-sections) or rename a heading (e.g. Tutorial -> Tutorials)
`make html` does not pick this up. First run `make clean` and then `make html` again.
