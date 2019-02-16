{ pkgs ? import <nixpkgs> {} }:

pkgs.fetchurl {
  url = "https://gist.githubusercontent.com/justinwoo/e1659d1690b5b418571be28be3d39a0f/raw/62bf5b4befc9196e9dd781176ccc48b0854ea81a/test-fetch-url";
  sha256 = "testsha";
}
