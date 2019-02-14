/*!
Update some fetchFromGitHub usages in a nix source file.

# Usage

```text
update-fetch-derivations
    Update GitHub fetch revisions and hashes for a given nix source file.

    Example:

      update-fetch-derivation fetch-from-github.nix
```

# Example:

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
*/

#![warn(missing_docs, rust_2018_idioms, clippy::all)]

use regex::Captures;
use regex::Regex;
use regex::RegexBuilder;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;
use std::process::Command;

// main goal:
// fetchFromGithub

// stretch goals:
// fetchgit
// fetchurl

struct ReplaceResults {
    count: usize,
    acc: String,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let target_file_path = args.get(1).expect(EXPECT_FILE_PATH_ARG_MSG);

    let mut in_file = File::open(target_file_path)
        .unwrap_or_else(|_| panic!("invalid in_file path provided: {}", target_file_path));

    let mut contents: String = String::new();
    in_file
        .read_to_string(&mut contents)
        .expect("Could not extract contents of specified in_file.");
    drop(in_file);

    let ReplaceResults { count, acc } = update_fetch_from_github(contents);

    let mut out_file = File::create(target_file_path)
        .unwrap_or_else(|_| panic!("invalid out_file path provided: {}", target_file_path));
    out_file
        .write_fmt(format_args!("{}", acc))
        .unwrap_or_else(|_| panic!("Unable to write to out_file {}", target_file_path));
    println!("updated {} derivations in {}", count, target_file_path);
}

fn update_fetch_from_github(contents: String) -> ReplaceResults {
    let mut acc: String = String::new();

    let github_regex = RegexBuilder::new(r"(fetchFromGitHub \{)(.*?)(\})")
        .dot_matches_new_line(true)
        .build()
        .unwrap();

    // ABA pattern: N(lits) - 1 = N(caps)
    let lits = Vec::from_iter(github_regex.split(&contents));
    let caps = github_regex.captures_iter(&contents);

    // how this works:
    // put together all the literal and captured segments, in ABABA pattern.
    // the inner matched section of a capture segment should be updated
    // updating involves calling `prefetch-github`

    let owner_regex: Regex = Regex::new(r#"owner = "(.*?)";"#).unwrap();
    let repo_regex: Regex = Regex::new(r#"repo = "(.*?)";"#).unwrap();
    let rev_regex: Regex = Regex::new(r#"(rev = ")(.*?)(";)"#).unwrap();
    let sha256_regex: Regex = Regex::new(r#"(sha256 = ")(.*?)(";)"#).unwrap();

    for (i, cap) in caps.enumerate() {
        let inner = &cap[2];
        let owner = &owner_regex.captures(inner).unwrap()[1];
        let repo = &repo_regex.captures(inner).unwrap()[1];
        println!("Fetching: {}/{}", owner, repo);

        let prefetch_result = Command::new("prefetch-github")
            .arg("-owner")
            .arg(owner)
            .arg("-repo")
            .arg(repo)
            .output()
            .expect("Failed to launch prefetch-github")
            .stdout;
        let result: String = String::from_utf8(prefetch_result).unwrap();
        let rev = &rev_regex.captures(&result).unwrap()[2];
        let sha256 = &sha256_regex.captures(&result).unwrap()[2];

        let inner2 = &rev_regex.replace(inner, |caps: &Captures<'_>| {
            format!("{}{}{}", &caps[1], rev, &caps[3])
        });
        let inner3 = &sha256_regex.replace(inner2, |caps: &Captures<'_>| {
            format!("{}{}{}", &caps[1], sha256, &caps[3])
        });

        acc.push_str(lits[i]);
        acc.push_str(&cap[1]);
        acc.push_str(inner3);
        acc.push_str(&cap[3]);
    }

    if let Some(s) = lits.last() {
        acc.push_str(s);
    };

    ReplaceResults {
        count: lits.len() - 1,
        acc,
    }
}

const EXPECT_FILE_PATH_ARG_MSG: &str = r#"
Need an argument for what in_file to process.

Usage Examples:
    # update a single in_file
    update-fetch-derivation my-in_file.nix

    # Using fd
    fd -e nix -x update-fetch-derivation {}
"#;
