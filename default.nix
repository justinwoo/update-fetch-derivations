{ pkgs ? import <nixpkgs> {} }:

let
  prefetch-github = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "prefetch-github";
    rev = "ecc358529592f403d24a955c293922124c4354f7";
    sha256 = "1wcyzmbrs0rzva7jwnqa4vqr34z1sv1cigpyyiaajkf8bx29pamw";
  }) { inherit pkgs; };

  prefetch-url = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "prefetch-url";
    rev = "71a74a6facfe8bd81ad0e571ef382b9abd887718";
    sha256 = "1qgdpy7qxlz81sf55lyswwjyzn99siy745k3w4y7l26w68zjrjki";
  }) { inherit pkgs; };

  binary = pkgs.rustPlatform.buildRustPackage rec {
    name = "update-fetch-derivations-rs";
    version = "0.2.0";
    src = ./.;
    cargoSha256 = "1p9svbrrxay3frgl22g1jnps4d8dflv684bcmiv9lh00hk77i52y";
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
        prefetch-url
        pkgs.nix-prefetch-git
      ]}
  ''
