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
