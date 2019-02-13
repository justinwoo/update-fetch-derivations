extern crate regex;

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

fn main() {
    let args: Vec<String> = env::args().collect();
    let target_file_path = args.get(1).expect(EXPECT_FILE_PATH_ARG_MSG);

    let mut in_file = File::open(target_file_path)
        .expect(&format!("invalid in_file path provided: {}", target_file_path));

    let mut contents: String = String::new();
    in_file.read_to_string(&mut contents)
        .expect("Could not extract contents of specified in_file.");
    drop(in_file);

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
        let mut inner = &cap[2];
        let owner = &owner_regex.captures(inner).unwrap()[1];
        let repo = &repo_regex.captures(inner).unwrap()[1];
        println!("Fetching: {}/{}", owner, repo);

        let prefetch_result =
          Command::new("prefetch-github")
            .arg("-owner")
            .arg(owner)
            .arg("-repo")
            .arg(repo)
            .output()
            .expect("Failed to launch prefetch-github").stdout;
        let result: String = String::from_utf8(prefetch_result).unwrap();
        let rev = &rev_regex.captures(&result).unwrap()[2];
        let sha256 = &sha256_regex.captures(&result).unwrap()[2];

        let inner2 = &rev_regex.replace(inner, |caps: &Captures| {
            format!("{}{}{}", &caps[1], rev, &caps[3])
        });
        let inner3 = &sha256_regex.replace(inner2, |caps: &Captures| {
            format!("{}{}{}", &caps[1], sha256, &caps[3])
        });

        acc.push_str(lits[i]);
        acc.push_str(&cap[1]);
        acc.push_str(inner3);
        acc.push_str(&cap[3]);
    }
    let count = lits.len() - 1;
    acc.push_str(lits.last().unwrap());

    let mut out_file = File::create(target_file_path)
        .expect(&format!("invalid out_file path provided: {}", target_file_path));
    out_file.write_fmt(format_args!("{}", acc)).expect(&format!("Unable to write to out_file {}", target_file_path));
    println!("updated {} derivations in {}", count, target_file_path);
}

const EXPECT_FILE_PATH_ARG_MSG: &'static str = r#"
Need an argument for what in_file to process.

Usage Examples:
    # update a single in_file
    update-fetch-derivation my-in_file.nix

    # Using fd
    fd -e nix -x update-fetch-derivation {}
"#;
