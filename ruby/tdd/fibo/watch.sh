#!/bin/bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT_DIR"

if ! command -v entr >/dev/null 2>&1; then
  echo "entr is required. Install it with: brew install entr" >&2
  exit 1
fi

echo "Watching lib/ and spec/. Save a Ruby file to rerun RSpec."

find lib spec -type f -name '*.rb' | sort | entr -c "$ROOT_DIR/test.sh"
