# For the Linux version of Packell, the compiled executable is 'Dynamically Linked' meaning the
# interpreter and the paths (or 'rpaths') of the compiled executable is linked to
# the interpreter/libraries available on the build system (i.e., in its nix store). As the nix paths
# will differ between the build and the host system, running Packell on a host system will generate
# an error due to the differing paths of the interpreter/libraries. To fix this we use
# `autoPatchelfHook` to automatically set both the interpreter and the 'rpath' of the dependending
# libraries used by the executable (depending libraries are set in 'buildInputs').
#
# The Apple compiled Packell ships with everything necessary to run the executable.
#
# See - https://nixos.wiki/wiki/Packaging/Binaries
#     - https://unix.stackexchange.com/questions/522822/different-methods-to-run-a-non-nixos-executable-on-nixos

{ pkgs, stdenv, version }:
let
  platform =
    if stdenv.isDarwin
      then if stdenv.isAarch64 then "aarch64-apple" else "x86_64-apple"
      else "x86_64-linux";
  tarball = fetchTarball {
    url = "https://github.com/digital-asset/daml-finance/releases/download/packell/${version}/packell-${platform}.tar.gz";
  };
in
  stdenv.mkDerivation {
    name = "packell";
    version = "$version";
    src = tarball;
    nativeBuildInputs =
      (if stdenv.isLinux then [ pkgs.autoPatchelfHook ] else [ ])
      ++ (if stdenv.isDarwin then [ pkgs.darwin.cctools ] else [ ]);
    buildInputs = [ pkgs.gmp pkgs.libffi pkgs.libiconv ];
    baseInputs = [ pkgs.binutils ];
    installPhase = ''
      mkdir -p $out/bin
      cp packell $out/bin
    '';
    postFixup = if stdenv.isDarwin then ''
      # Packell release binaries are dynamically linked against absolute nix store
      # paths from the build machine. Rewrite those to libraries in our closure so
      # GC does not leave a broken executable in developer shells.
      bin="$out/bin/packell"
      ffiTarget=""
      for candidate in \
        ${pkgs.libffi}/lib/libffi.8.dylib \
        ${pkgs.libffi}/lib/libffi.dylib \
        ${pkgs.libffi}/lib/libffi.7.dylib; do
        if [ -e "$candidate" ]; then
          ffiTarget="$candidate"
          break
        fi
      done
      # Rewrite any store path references regardless of hash/version.
      ${pkgs.darwin.cctools}/bin/otool -L "$bin" | ${pkgs.gawk}/bin/awk 'NR > 1 { print $1 }' | while read -r dep; do
        case "$dep" in
          /nix/store/*/lib/libgmp*.dylib)
            ${pkgs.darwin.cctools}/bin/install_name_tool -change "$dep" ${pkgs.gmp}/lib/libgmp.10.dylib "$bin" || true
            ;;
          /nix/store/*/lib/libffi*.dylib)
            if [ -n "$ffiTarget" ]; then
              ${pkgs.darwin.cctools}/bin/install_name_tool -change "$dep" "$ffiTarget" "$bin" || true
            fi
            ;;
          /nix/store/*/lib/libiconv*.dylib)
            ${pkgs.darwin.cctools}/bin/install_name_tool -change "$dep" ${pkgs.libiconv}/lib/libiconv.dylib "$bin" || true
            ;;
        esac
      done
      # install_name_tool can invalidate signatures on prebuilt macOS binaries,
      # which may cause runtime termination (Killed: 9). Re-sign ad-hoc.
      /usr/bin/codesign --force --sign - "$bin" || true
    '' else "";
  }
