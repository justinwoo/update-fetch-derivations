{ pkgs ? import <nixpkgs> {} }:

rec {
  # we might eventually try to make a hacky workaround
  # but we won't bother with parameterized urls for now
  param = "gist";

  not-updated = pkgs.fetchurl {
    url = "https://${param}.githubusercontent.com/justinwoo/e1659d1690b5b418571be28be3d39a0f/raw/62bf5b4befc9196e9dd781176ccc48b0854ea81a/test-fetch-url";
    sha256 = "testsha";
  };
}
