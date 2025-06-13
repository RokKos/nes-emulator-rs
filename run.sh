#!/bin/bash

set -e

cargo fmt --check

cargo clippy-strict

RUST_BACKTRACE=full cargo watch -x 'run --features strict'
