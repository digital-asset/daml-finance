# Packell

A tool to aid the development of the Daml-Finance library.

## VS Code

### Setup

To develop this application using `VS Code`, install the following `VS Code` extensions:
- [Haskell Syntax Highlighting plugin](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell)
- [Haskell plugin](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)

The Haskell Language Server (HLS) is defined and installed via [nix](default.nix) (alongside the GHC
and cabal). By default, the HLS will look to `ghcup` to manage HLS installations. To have nix manage
it instead, update your perferences either by opening the settings pane and updating "Manage HLS" to
`PATH`, or by running "Preferences: Open User Settings (JSON)" and add/update the following line
`"haskell.manageHLS": "PATH"`.

### Development

To develop the Packell application, open `VS Code` at the root of the Packell project.

## Usage

As part of nix setup, the `Packell` application is build and installed as a derivative into the nix
store and added to the `PATH`. Running the following command will display all the available options :

```
packell -h
```

## Auto-completion

Auto-completion is available for bash, fish and zsh. To enable auto-completion for your shell of
choice, please the `optparse-applicative` documentation
[https://github.com/pcapriotti/optparse-applicative#bash-zsh-and-fish-completions](here).

An example for bash shell on Ubuntu :

```
packell --bash-completion-script packell >/etc/bash_completion.d/packell
```

An example for fish shell on MacOS :

```
packell --fish-completion-script packell > ~/.config/fish/completions/packell.fish
```

## Cross compilation

Cross compilation will depend on your host system. For example, with a host using Apple silicon
(ie., Apple M1), it's only possible to cross compile to Apple with an Intel CPU (ie., `x86_64`).

By default, upon entering this directory `nix` will build `Packell` (if necessary) with the default
instruction set architecture of the host system.

To cross compile, for example, assuming an Apple M1 host cross compiling to Apple with an Intel CPU
run :

```
  nix-shell --system x86_64-darwin
```

To cross compile in the opposite direction, change the system variable in the command above to
`aarch64-darwin` (untested).

Compilation of the Linux `x86_64` Packell executable is done via the `CI` by the job
`packell_linux_x86_64` and will be made available as an `artifact` upon successful completion of
the `CI`. To run this job,simply create a branch beginning with `packell-release`.
