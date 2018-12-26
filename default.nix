{ pkgs ? import <nixpkgs> {} }:

let
  prefetch-github = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "prefetch-github";
    rev = "3b7bf4ba8b8e2dba3df78d82e613818bbf0f244a";
    sha256 = "127c8qgjw64y29mjqi3ww7v78immzjvq0aw8rwhr3krd1vsdcl4n";
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
