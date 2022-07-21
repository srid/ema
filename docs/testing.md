# Testing of Custom Type Errors

Ema uses static verification to catch most common errors related to anyclass deriving of `GenericRoute`, instead displaying to the user more legible and friendly error messages. Due to current GHC limitations, we are left with having to _manually_ test that custom type errors are, in fact, displaying appropriately for various cases of improper deriving. That said, the current testing methodology has been designed to ease the transition over to future automated testing as GHC limitations are eventually resolved.

## The Problem

- Automated testing is ultimately being held up on the now-implemented `Unsatisfiable` proposal landing in a release GHC version.
  * Due to more formalized semantics, this will allow for custom type errors that can be delayed until runtime via `-fdefer-type-errors`
  * Relevant PR documenting the fact that `TypeError`s do not actually defer even with `-fdefer-type-errors` can be found [here](https://github.com/CRogers/should-not-typecheck/pull/15#issuecomment-502295932)

## Current Manual Workaround

Current release candidates of Ema should load a REPL for the `test-type-errors` test suite (ex. `nix develop -c cabal -- repl test-type-errors`), and for each test case:

1. Uncomment the test case code.
    * Use something like block delete ideally.
    * Just the first 3 characters of each line; whitespace in error message quasiquote is significant!
2. Reload the source file (`:r`).
3. Verify that the expected error message detailed in the error message quasiquote at the tail of each test case spec matches _at least one_ of the error blocks GHC is throwing.
    * 'Error block' in the context of this document refers to a segment in the overall compiler error message starting with a bulletin dot (`•`), and ending at the next line (exclusive) to contain a bulletin dot, ignoring any leading whitespace.

## Future Rollout Plan for Automated Testing

Once `Unsatisfiable` lands in a release GHC:
1. Submit a patch to [should-not-typecheck](https://github.com/CRogers/should-not-typecheck), adding a mechanism for custom type error testing via `Unsatisfiable`.
2. Convert over and automate test cases under `test/type-errors` via this new mechanism:
    * Create a unified spec per module that evaluates expressions that will force the type error for each test case, and compares them to the expected output:
      * Ex. for routes: `routeUniverse @RouteSpec_x undefined`
      * May need to make an exception to ignore errors related to evaluating `undefined`
      * Note that quasiquoted `[r||]` strings will all start with an empty line due to how error messages have been formatted to start on a new line for readability purposes, so preprocess it before comparing.
      * Suggestion: use a compiler plugin that discovers all data types that start with the test spec identifier for the module; i.e. in `Deriving.hs`, all generated test routes will start with `RouteSpec_`. Otherwise, modify the test spec generator TH to name these after a unique identifer and add said identifiers for all test cases, then manually reference them from the main module spec. 
    * Merge all module specs with the main test suite spec.
2. Deprecate/remove `test-type-errors` test suite.
    * Also consider renaming `test/type-errors` to be a proper sub-module of the main test suite.
