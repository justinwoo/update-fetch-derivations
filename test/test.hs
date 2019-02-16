#!/usr/bin/env runghc

import qualified System.Exit as Exit
import qualified System.Process as Proc

data ExpectError = Oui | Non

-- expect things, and be rewarded with disappointment
runWithExpectation :: ExpectError -> String -> IO ()
runWithExpectation expectError cmd = do
  exit <- Proc.system cmd
  case (exit, expectError) of
    (Exit.ExitSuccess, Oui) -> fail $ "expected error from cmd: " <> cmd
    (Exit.ExitSuccess, Non) -> pure ()
    (Exit.ExitFailure _, Oui) -> pure ()
    (Exit.ExitFailure _, Non) -> fail $ "unexpected error from cmd: " <> cmd

main :: IO ()
main = do
  runWithExpectation Non "update-fetch-derivations fetch-from-github.nix"
  runWithExpectation Oui gitErrorOnDiff
  runWithExpectation Non resetFiles

  runWithExpectation Non "update-fetch-derivations fetchgit-success.nix"
  runWithExpectation Oui gitErrorOnDiff
  runWithExpectation Non resetFiles

  runWithExpectation Oui "update-fetch-derivations fetchgit-failure.nix"
  runWithExpectation Non gitErrorOnDiff
  runWithExpectation Non resetFiles

  putStrLn "tests passed."

  where
    gitErrorOnDiff = "git diff --exit-code --quiet"
    resetFiles = "git checkout *.nix"
