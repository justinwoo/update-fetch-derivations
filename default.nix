{ pkgs ? import <nixpkgs> {} }:

let
  prefetch-github = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "prefetch-github";
    rev = "ecc358529592f403d24a955c293922124c4354f7";
    sha256 = "1wcyzmbrs0rzva7jwnqa4vqr34z1sv1cigpyyiaajkf8bx29pamw";
  }) { inherit pkgs; };

  binary = pkgs.rustPlatform.buildRustPackage rec {
    name = "update-fetch-derivations-rs";
    version = "0.1.0";
    src = ./.;
    cargoSha256 = "0dxmw28hiv21vmdvlbkz0knn665m24sq1101jyp8x6la9im3pg0z";
  };

in pkgs.runCommand "update-fetch-derivations" {
  name = "update-fetch-derivations";
  buildInputs = [
    pkgs.makeWrapper
  ];
} ''
    mkdir -p $out/bin
    install -D -m555 -t $out/bin ${binary}/bin/update-fetch-derivations

    wrapProgram $out/bin/update-fetch-derivations \
      --prefix PATH : ${pkgs.lib.makeBinPath [
        prefetch-github
      ]}
  ''
