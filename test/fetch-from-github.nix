{ pkgs ? import <nixpkgs> {} }:

let
  a = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-dhall-nix";
    rev = "testrev";
    sha256 = "testsha";
  }) {};

  b = pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "testrev";
    sha256 = "testsha";
  };
in {
  inherit a;
  inherit b;
}
