#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

iris format --check
iris build
iris test
iris run >/dev/null
