#!/bin/sh

# Script that's useful when trying to find bugs (indicated by test
# failures) using `git bisect`.

cabal clean
(cabal configure && cabal build) || exit 125  # this skips broken builds
make -C tests clean run-tests
