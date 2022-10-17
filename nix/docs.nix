# Doc dependencies are taken from daml.docs.com (https://github.com/digital-asset/docs.daml.com/blob/main/shell.nix).

{ pkgs }:
let
  sphinx-copybutton = pkgs.python3Packages.buildPythonPackage rec {
      pname = "sphinx-copybutton";
      version = "0.2.12";

      src = pkgs.python3Packages.fetchPypi {
        inherit pname version;
        sha256 = "0p1yls8pplfg59wzmb96m3pjcyr3202an1rcr5wn2jwqhqvqi4ll";
      };
      doCheck = false;
      buildInputs = [pkgs.python3Packages.sphinx];
  };
  sphinx-exts = pkgs.python3Packages.sphinx.overridePythonAttrs (attrs: rec {
    propagatedBuildInputs = attrs.propagatedBuildInputs ++ [sphinx-copybutton];
  });
  texlive = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      bera
      capt-of
      collection-fontsrecommended
      collection-luatex
      datetime
      enumitem
      environ
      epigraph
      eqparbox
      eulervm
      fancyhdr
      fmtcount
      fncychap
      footmisc
      footnotebackref
      framed
      latexmk
      lipsum
      mathpartir
      mathpazo
      mnsymbol
      multirow
      needspace
      palatino
      scheme-small
      tabulary
      threeparttable
      tikzsymbols
      titlesec
      tocbibind
      todonotes
      transparent
      trimspaces
      varwidth
      wrapfig
      xargs
    ;
  };
  dependencies = [ sphinx-exts
    texlive
    pkgs.pipenv
    pkgs.python39
    pkgs.sass
    pkgs.yarn
    pkgs.nodePackages.grunt-cli ];
in
  dependencies
