# Testing of GenericRoute Deriving

Ema uses static verification to catch most common errors related to anyclass deriving of `GenericRoute`, instead displaying to the user more legible and friendly error messages. Due to GHC limitations currently, we are left with having to _manually_ test that custom type errors are, in fact, displaying appropriately for various cases of improper deriving. 

## The Problem

- Automated testing is ultimately being held up on the now-implemented `Unsatisfiable` proposal landing in a release GHC version.
  * Due to more formalized semantics, this will allow for custom type errors that can be delayed until runtime via `-fdefer-type-errors`
  * Relevant PR documenting the fact that `TypeError`s do not actually defer even with `-fdefer-type-errors` can be found [here](https://github.com/CRogers/should-not-typecheck/pull/15#issuecomment-502295932)

## Rollout Plan for Automated Testing

Once `Unsatisfiable` lands in a release GHC:
1. Submit a patch to [should-not-typecheck](https://github.com/CRogers/should-not-typecheck), adding a mechanism for custom type error testing via `Unsatisfiable`.
2. Covert over and automate test cases in `test/manual/Deriving.hs` under this new mechanism, to reside in `test/Deriving.hs`.
2. Deprecate `test/manual/Deriving.hs`.

## Current Manual Workaround

Current release candidates of Ema should manually load `test/manual/Deriving.hs` via `bin/repl`, and for each test case:

1. Uncomment the test case code.
2. Reload the source file.
3. Verify that the expected error message detailed in the test case header block match the error displayed in the REPL.
