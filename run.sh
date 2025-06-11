#!/bin/bash

set -e

cargo fmt --check

cargo clippy-strict

RUST_BACKTRACE=1 cargo watch -x 'run --features strict'
