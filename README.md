[![Build Status](https://travis-ci.com/justinwoo/update-fetch-derivations.svg?branch=master)](https://travis-ci.com/justinwoo/update-fetch-derivations)

# update-fetch-derivations

Update some fetchgit/fetchFromGitHub usages in a nix source file.

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
Fetching: justinwoo/easy-dhall-nix
Fetching: justinwoo/easy-purescript-nix
updated 2 derivations in fetch-from-github.nix

> gid
diff --git a/test/fetch-from-github.nix b/test/fetch-from-github.nix
index 9af00b8..5974894 100644
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
+    rev = "d9e92b89b53865e93a56d19b2f1e55dc0c344696";
+    sha256 = "1miphbm0m3i7mrjyyb8s97ka62pwgvql8vry0jqz8zdllfqm0rh2";
   };
 in {
   inherit a;
```
