{ pkgs ? import <nixpkgs> {},
  hc ? "ghc844"
}:

pkgs.stdenv.mkDerivation rec {
  name = "slay";
  buildInputs = [
    pkgs.haskell.compiler.${hc}
    pkgs.zlib
    pkgs.cabal-install
    pkgs.pkgconfig
    pkgs.cairo
    pkgs.pango
    pkgs.gtk3
    pkgs.which # for CI
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
    export LANG=en_US.UTF-8
  '';
  LOCALE_ARCHIVE =
    if pkgs.stdenv.isLinux
    then "${pkgs.glibcLocales}/lib/locale/locale-archive"
    else "";
}
