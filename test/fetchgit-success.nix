{ pkgs ? import <nixpkgs> {} }:

let
  a = import (pkgs.fetchgit {
    url = "https://github.com/justinwoo/purp";
    rev = "testrev";
    sha256 = "testsha";
  }) {};
in {
  inherit a;
}
