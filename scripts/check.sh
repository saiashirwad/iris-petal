#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

iris format --check
iris build
iris test
iris run -p examples >/dev/null
