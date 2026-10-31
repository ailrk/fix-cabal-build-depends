# fix-cabal-build-depends

Runs `cabal build`, reads the compiler errors, adds any missing packages to your
`.cabal` file, and repeats until nothing is missing. It supports pacakge with
multiple components.

## Install

> cabal install exe:fix-cabal-build-depends

## Use

Run it in the folder with your `.cabal` file:

> fix-cabal-build-depends

A backup is saved as `<name>.cabal.bak` first. Use `--dry-run` to only see what
would be added.

## Notes

- Packages are taken from GHC's own error message, so they must already be installed.
- No version bounds are added.
- Modules GHC gives no package for are listed, not guessed.
