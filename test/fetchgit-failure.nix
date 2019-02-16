{ pkgs ? import <nixpkgs> {} }:

let
  a = import (pkgs.fetchgit {
    url = "https://github.com/justinwoo/thiscauses404";
    rev = "testrev";
    sha256 = "testsha";
  }) {};
in {
  inherit a;
}
