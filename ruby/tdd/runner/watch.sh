#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="${1:-$(pwd)}"
cd "$PROJECT_DIR"

if ! command -v entr >/dev/null 2>&1; then
  echo "entr is required. Install it with: brew install entr" >&2
  exit 1
fi

echo "Watching lib/ and spec/ in $PROJECT_DIR"

find lib spec -type f -name '*.rb' | sort | entr -c "$SCRIPT_DIR/test.sh"
