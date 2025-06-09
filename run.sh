#!/bin/bash

set -e

cargo fmt --check

cargo clippy-strict

cargo watch -x 'run --features strict'
