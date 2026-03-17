#!/bin/bash

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="${TDD_PROJECT_DIR:-$(pwd)}"
cd "$PROJECT_DIR"

BUNDLER_VERSION="2.5.16"
HAMMERSPOON_CLI="/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs"
APP_NAME="${TDD_APP_NAME:-Ruby TDD}"
APP_BUNDLE_ID="${TDD_APP_BUNDLE_ID:-com.apple.Terminal}"

resolve_bundle() {
  local candidate

  if [ -n "${BUNDLE_CMD:-}" ] && [ -x "${BUNDLE_CMD}" ]; then
    echo "${BUNDLE_CMD}"
    return 0
  fi

  if bundle _"${BUNDLER_VERSION}"_ version >/dev/null 2>&1; then
    echo "bundle"
    return 0
  fi

  for candidate in \
    "$HOME"/.rubies/*/bin/bundle \
    "$HOME"/.rbenv/shims/bundle \
    "$HOME"/.asdf/shims/bundle
  do
    if [ -x "$candidate" ] && "$candidate" _"${BUNDLER_VERSION}"_ version >/dev/null 2>&1; then
      echo "$candidate"
      return 0
    fi
  done

  return 1
}

b64_encode() {
  printf '%s' "$1" | /usr/bin/base64 | tr -d '\n'
}

collapse_whitespace() {
  printf '%s' "$1" | tr '\n' ' ' | tr '\t' ' ' | sed -E 's/[[:space:]]+/ /g; s/^ //; s/ $//'
}

truncate_text() {
  local text
  text="$(collapse_whitespace "$1")"
  if [ "${#text}" -gt "$2" ]; then
    printf '%s…' "${text:0:$(( $2 - 1 ))}"
  else
    printf '%s' "$text"
  fi
}

extract_pass_summary() {
  local output_file="$1"
  local summary

  summary="$(grep -Eo '[0-9]+ examples?, [0-9]+ failures?(, [0-9]+ pending)?' "$output_file" | tail -n 1)"
  if [ -z "$summary" ]; then
    summary="Tests passed"
  fi

  printf '%s' "$summary"
}

extract_fail_summary() {
  local output_file="$1"
  local example failure expected got summary

  example="$(awk '/^  [0-9]+\)/{sub(/^  [0-9]+\) /,""); print; exit}' "$output_file")"
  failure="$(awk '/Failure\/Error:/{sub(/.*Failure\/Error: /,""); print; exit}' "$output_file")"
  expected="$(awk '/expected:/{gsub(/^[[:space:]]+/,""); print; exit}' "$output_file")"
  got="$(awk '/got:/{gsub(/^[[:space:]]+/,""); print; exit}' "$output_file")"

  if [ -n "$example" ] && [ -n "$expected" ] && [ -n "$got" ]; then
    summary="${example}: ${expected}; ${got}"
  elif [ -n "$example" ] && [ -n "$failure" ]; then
    summary="${example}: ${failure}"
  elif [ -n "$failure" ]; then
    summary="$failure"
  else
    summary="$(grep -Eo '[0-9]+ examples?, [0-9]+ failures?(, [0-9]+ pending)?' "$output_file" | tail -n 1)"
  fi

  if [ -z "$summary" ]; then
    summary="Tests failed"
  fi

  truncate_text "$summary" 180
}

notify() {
  local title="$1"
  local message="$2"
  local status="$3"
  local safe_title="${title//\"/\\\"}"
  local safe_message="${message//\"/\\\"}"

  if [ "${USE_HAMMERSPOON:-1}" = "1" ] && [ -x "${HAMMERSPOON_CLI}" ]; then
    "${HAMMERSPOON_CLI}" -A "${SCRIPT_DIR}/hammerspoon_tdd.lua" \
      "$status" \
      "$(b64_encode "$APP_NAME")" \
      "$(b64_encode "$title")" \
      "$(b64_encode "$message")" \
      "$(b64_encode "$APP_BUNDLE_ID")" >/dev/null 2>&1 || true
    return
  fi

  if command -v osascript >/dev/null 2>&1; then
    if osascript -e "display notification \"$safe_message\" with title \"$safe_title\" subtitle \"$APP_NAME\"" >/dev/null 2>&1; then
      return
    fi
  fi

  if [ "${USE_TERMINAL_NOTIFIER:-0}" = "1" ] && command -v terminal-notifier >/dev/null 2>&1; then
    terminal-notifier -title "$title" -message "$message" -group "ruby-tdd" >/dev/null 2>&1 || true
  fi
}

if ! BUNDLE_EXE="$(resolve_bundle)"; then
  echo "Could not find Bundler ${BUNDLER_VERSION}. Set BUNDLE_CMD or install the required Bundler version." >&2
  exit 1
fi

output_file="$(mktemp /tmp/ruby-tdd-rspec.XXXXXX)"
"$BUNDLE_EXE" _"${BUNDLER_VERSION}"_ exec rspec 2>&1 | tee "$output_file"
status=$?

if [ "$status" -eq 0 ]; then
  notify "성공" "$(extract_pass_summary "$output_file")" "pass"
else
  notify "실패" "$(extract_fail_summary "$output_file")" "fail"
fi

rm -f "$output_file"

exit "$status"
