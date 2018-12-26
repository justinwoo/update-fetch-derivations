{ pkgs ? import <nixpkgs> {} }:

let ufd = import ../default.nix { inherit pkgs; };
in pkgs.runCommand "update-fetch-derivations-test" {
  buildInputs = [ufd];
} ""
