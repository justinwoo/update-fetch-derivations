# Update-Fetch-Derivations

[![Build Status](https://travis-ci.com/justinwoo/update-fetch-derivations.svg?branch=master)](https://travis-ci.com/justinwoo/update-fetch-derivations)

Update some fetchFromGitHub usages in a nix source file.

## Usage

```
update-fetch-derivations
    Update GitHub fetch revisions and hashes for a given nix source file.

    Example:

      update-fetch-derivation fetch-from-github.nix
```

## Example:

From ./test:

```diff
> update-fetch-derivations fetch-from-github.nix
Updated 2 matches of fetchFromGitHub in fetch-from-github.nix

> gid
diff --git a/test/fetch-from-github.nix b/test/fetch-from-github.nix
index 9af00b8..c0b2caa 100644
--- a/test/fetch-from-github.nix
+++ b/test/fetch-from-github.nix
@@ -4,15 +4,15 @@ let
   a = import (pkgs.fetchFromGitHub {
     owner = "justinwoo";
     repo = "easy-dhall-nix";
-    rev = "testrev";
-    sha256 = "testsha";
+    rev = "9e3b8744db0a9d369675a4b12a955614b8100449";
+    sha256 = "00ww6fhv8lvihjfzjzpd4kgfqx8isk4nalmc79vh9mhfv7ya0m5p";
   }) {};

   b = pkgs.fetchFromGitHub {
     owner = "justinwoo";
     repo = "easy-purescript-nix";
-    rev = "testrev";
-    sha256 = "testsha";
+    rev = "993f63359b64db080061b274e4688e3b80c4f68e";
+    sha256 = "18b7fmmxkg38y1av9kfgcv2rikdlji51ya5b9p7sy3aml2hprmi5";
   };
 in {
   inherit a;
```
