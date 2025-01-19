default:
    @just --list

# Run the examples (after compiling the library)
run:
    ghcid \
        --restart ./www \
        -c "cabal repl ema-examples" \
        -T Ema.Example.Ex04_Multi.main  \
        --setup ":set args run --port=8080"

# Run Cabal repl
repl:
    cabal repl ema

# Test type-errors
test-type-errors:
    ghcid -c "cabal repl test:test-type-errors"