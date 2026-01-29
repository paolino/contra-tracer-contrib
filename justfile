# shellcheck shell=bash

set unstable := true

# List available recipes
default:
    @just --list

# Format all source files
format:
    #!/usr/bin/env bash
    set -euo pipefail
    for i in {1..3}; do
        fourmolu -i src test
    done
    cabal-fmt -i *.cabal
    nixfmt *.nix nix/*.nix

# Run hlint
hlint:
    #!/usr/bin/env bash
    hlint src test

# Build all components
build:
    #!/usr/bin/env bash
    cabal build all -O0 --enable-tests

# Run unit tests with optional match pattern
unit match="":
    #!/usr/bin/env bash
    if [[ '{{ match }}' == "" ]]; then
        cabal test unit-tests -O0 --test-show-details=direct
    else
        cabal test unit-tests -O0 \
            --test-show-details=direct \
            --test-option=--match \
            --test-option="{{ match }}"
    fi

# Check Hackage compatibility
hackage-check:
    #!/usr/bin/env bash
    cabal check

# Full CI pipeline
CI:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just unit
    just hackage-check
    fourmolu -m check src test
    hlint src test

# Serve documentation locally
serve-docs:
    #!/usr/bin/env bash
    mkdocs serve

# Build documentation
build-docs:
    #!/usr/bin/env bash
    mkdocs build
