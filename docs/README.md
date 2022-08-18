# Daml Finance Documentation

Theme is copied from Daml Docs theme.

## Pre-requisites

1. Install `pyenv` for Python version management with `brew install pyenv`.

2. Install `pipx` with `brew install pipenv`. This should install the `pipenv` binary for virtual environments management.

3. From this folder, `pipenv install` to create a virtual environment and download the packages.

## Building the docs

1. `pipenv shell` to enter the virtual environment.

2. `make html` to trigger the Sphinx build.

## Viewing the docs locally

1. `cd build/html`

2. `python -m http.server 8000 --bind 127.0.0.1`
