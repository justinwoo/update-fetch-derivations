/*!
Update some fetchgit/fetchFromGitHub usages in a nix source file.

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
    let mut count: usize = 0;

    update_fetch_from_github(&mut contents, &mut count);
    update_fetch_git(&mut contents, &mut count);
    update_fetch_url(&mut contents, &mut count);

    let mut out_file = File::create(target_file_path)
        .unwrap_or_else(|_| panic!("invalid out_file path provided: {}", target_file_path));
    out_file
        .write_fmt(format_args!("{}", contents))
        .unwrap_or_else(|_| panic!("Unable to write to out_file {}", target_file_path));
    println!("updated {} derivations in {}", count, target_file_path);
}

fn update_fetch_from_github(contents: &mut String, count: &mut usize) {
    let github_regex = RegexBuilder::new(r"(fetchFromGitHub \{)(.*?)(\})")
        .dot_matches_new_line(true)
        .build()
        .unwrap();

    let owner_regex: Regex = Regex::new(r#"owner = "(.*?)";"#).unwrap();
    let repo_regex: Regex = Regex::new(r#"repo = "(.*?)";"#).unwrap();
    let rev_regex: Regex = Regex::new(r#"(rev = ")(.*?)(";)"#).unwrap();
    let sha256_regex: Regex = Regex::new(r#"(sha256 = ")(.*?)(";)"#).unwrap();

    update_contents_by_inner(contents, count, github_regex, |inner: &mut String| {
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

        *inner = rev_regex
            .replace(inner, |caps: &Captures<'_>| {
                format!("{}{}{}", &caps[1], rev, &caps[3])
            })
            .to_owned()
            .to_string();
        *inner = sha256_regex
            .replace(inner, |caps: &Captures<'_>| {
                format!("{}{}{}", &caps[1], sha256, &caps[3])
            })
            .to_owned()
            .to_string();
    });
}

fn update_fetch_git(contents: &mut String, count: &mut usize) {
    let fetchgit_regex = RegexBuilder::new(r"(fetchgit \{)(.*?)(\})")
        .dot_matches_new_line(true)
        .build()
        .unwrap();

    let url_regex: Regex = Regex::new(r#"url = "(.*?)";"#).unwrap();
    let rev_regex: Regex = Regex::new(r#"(rev = ")(.*?)(";)"#).unwrap();
    let sha256_regex: Regex = Regex::new(r#"(sha256 = ")(.*?)(";)"#).unwrap();

    let rev_json_regex: Regex = Regex::new(r#"("rev": ")(.*?)(",)"#).unwrap();
    let sha256_json_regex: Regex = Regex::new(r#"("sha256": ")(.*?)(",)"#).unwrap();

    update_contents_by_inner(contents, count, fetchgit_regex, |inner: &mut String| {
        let url = &url_regex.captures(&inner).unwrap()[1];
        println!("Fetching: {}", url);

        let prefetch_attempt = Command::new("nix-prefetch-git")
            .arg(url)
            .arg("--quiet")
            .env("GIT_TERMINAL_PROMPT", "0")
            .output()
            .expect("Failed to launch nix-prefetch-git");

        if prefetch_attempt.status.success() {
            let result: String = String::from_utf8(prefetch_attempt.stdout).unwrap();
            let rev = &rev_json_regex.captures(&result).unwrap()[2];
            let sha256 = &sha256_json_regex.captures(&result).unwrap()[2];

            if sha256 == INVALID_PREFETCH_GIT_SHA {
                panic!(
                    "nix-prefetch-git could not find the repository at the url: {}",
                    url
                )
            }

            *inner = rev_regex
                .replace(&inner, |caps: &Captures<'_>| {
                    format!("{}{}{}", &caps[1], rev, &caps[3])
                })
                .to_owned()
                .to_string();

            *inner = sha256_regex
                .replace(&inner, |caps: &Captures<'_>| {
                    format!("{}{}{}", &caps[1], sha256, &caps[3])
                })
                .to_owned()
                .to_string();
        } else {
            println!(
                "warning: error from nix-prefetch-git: {:?}",
                String::from_utf8(prefetch_attempt.stderr)
            );
        }
    });
}

fn update_fetch_url(contents: &mut String, count: &mut usize) {
    let fetchurl_regex = RegexBuilder::new(r"(fetchurl \{)(.*?)(\})")
        .dot_matches_new_line(true)
        .build()
        .unwrap();

    let url_regex: Regex = Regex::new(r#"url = "(.*?)";"#).unwrap();
    let sha256_regex: Regex = Regex::new(r#"(sha256 = ")(.*?)(";)"#).unwrap();

    update_contents_by_inner(contents, count, fetchurl_regex, |inner: &mut String| {
        if let Some(matches) = url_regex.captures(&inner) {
            let url = &matches[1];
            println!("Fetching: {}", url);

            let prefetch_attempt = Command::new("prefetch-url")
                .arg(url)
                .output()
                .expect("Failed to launch prefetch-url");

            if prefetch_attempt.status.success() {
                let result: String = String::from_utf8(prefetch_attempt.stdout).unwrap();
                let sha256 = &sha256_regex.captures(&result).unwrap()[2];

                *inner = sha256_regex
                    .replace(&inner, |caps: &Captures<'_>| {
                        format!("{}{}{}", &caps[1], sha256, &caps[3])
                    })
                    .to_owned()
                    .to_string();
            } else {
                println!(
                    "warning: error from prefetch-url while fetching: {:?}",
                    String::from_utf8(prefetch_attempt.stderr)
                );
            }
        };
    });
}

// update contents of a file, mutating the contents and adding to the count of matches.
// the match regex argument used to break the literal matches and the actual captured groups to be processed.
// the contents of each capture are called "inner", which is used by the update_inner closure argument.
fn update_contents_by_inner<F>(
    contents: &mut String,
    count: &mut usize,
    match_regex: Regex,
    update_inner: F,
) where
    F: Fn(&mut String),
{
    let mut acc: String = String::new();

    // ABA pattern: N(lits) - 1 = N(caps)
    // how this works:
    // put together all the literal and captured segments, in ABABA pattern.
    // the inner matched section of a capture segment should be updated
    let lits = Vec::from_iter(match_regex.split(&contents));
    let caps = match_regex.captures_iter(&contents);

    for (i, cap) in caps.enumerate() {
        let mut inner = String::from(&cap[2]);
        update_inner(&mut inner);

        acc.push_str(lits[i]);
        acc.push_str(&cap[1]);
        acc.push_str(&inner);
        acc.push_str(&cap[3]);
    }

    if let Some(s) = lits.last() {
        acc.push_str(s);
    };

    *count += lits.len() - 1;
    *contents = acc;
}

const INVALID_PREFETCH_GIT_SHA: &str = "0sjjj9z1dhilhpc8pq4154czrb79z9cm044jvn75kxcjv6v5l2m5";

const EXPECT_FILE_PATH_ARG_MSG: &str = r#"
Need an argument for what in_file to process.

Usage Examples:
    # update a single in_file
    update-fetch-derivation my-in_file.nix

    # Using fd
    fd -e nix -x update-fetch-derivation {}
"#;
