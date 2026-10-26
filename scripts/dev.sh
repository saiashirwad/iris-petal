#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

iris watch &
watcher=$!
trap 'kill "$watcher" 2>/dev/null || true' EXIT INT TERM

npx vite
