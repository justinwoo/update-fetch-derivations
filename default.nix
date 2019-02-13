{ pkgs ? import <nixpkgs> {} }:

let
  prefetch-github = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "prefetch-github";
    rev = "ecc358529592f403d24a955c293922124c4354f7";
    sha256 = "1wcyzmbrs0rzva7jwnqa4vqr34z1sv1cigpyyiaajkf8bx29pamw";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "update-fetch-derivations";

  src = ./.;

  buildInputs = [
    pkgs.makeWrapper
  ];

  installPhase = ''
    mkdir -p $out/bin
    install -D -m555 -t $out/bin update-fetch-derivations

    wrapProgram $out/bin/update-fetch-derivations \
      --prefix PATH : ${pkgs.lib.makeBinPath [
        pkgs.coreutils
        pkgs.perl
        prefetch-github
      ]}
  '';
}
