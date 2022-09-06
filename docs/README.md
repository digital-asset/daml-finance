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

The `ruby` version of `sass` must be installed: `sudo gem install sass`.

### Grunt

If grunt is not already installed: `sudo npm install -g grunt-cli`.

If you had to install anything new above, but it still does not work, you may have to recreate the
virtual environment: run `pipenv install` again in the root folder of the repository.

## Building the docs

1. Navigate to the root folder of the repository.

2. `make build` to build the Daml Finance `dar`s.

3. `pipenv shell` to enter the virtual environment.

4. `make doc-html` to build the HTML documentation.

## Viewing the docs locally

1. Navigate to the root folder of the repository.

2. Start a local webserver on `http://localhost:8000/` by running `./docs/preview.sh`. In case an
   old/cached html document is being shown, make sure to refresh your webbrowser.

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

## Publishing the docs

The final website is currently hosted on
[Github pages](https://digital-asset.github.io/daml-finance/).

In order to update it, you need to

- build the docs from the desired branch as outlined above
- copy the content of `docs/build/html` to a temporary folder `temp`
- switch to the `github-pages` branch
- delete the content of the `docs` folder, except for the `.nojekyll` file
- copy the content of `temp` into `docs`
- commit and push to remote
