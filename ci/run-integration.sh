#!/usr/bin/env bash

set -euo pipefail

repository_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
artifact_directory=${SEKI_INTEGRATION_ARTIFACT_DIR:-"$repository_root/artifacts/integration"}
expected_manifest="$repository_root/ci/expected-integration-manifest.txt"
manifest="$artifact_directory/manifest.txt"
raw_log="$artifact_directory/integration.raw.log"
bounded_log="$artifact_directory/integration.log"

mkdir -p -- "$artifact_directory"
: >"$manifest"
: >"$raw_log"

finalize_log() {
  tail -c 1048576 -- "$raw_log" >"$bounded_log" 2>/dev/null || true
  rm -f -- "$raw_log"
}
trap finalize_log EXIT

cd -- "$repository_root"

set +e
cabal build seki-integration 2>&1 | tee -a "$raw_log"
build_status=${PIPESTATUS[0]}
set -e
if ((build_status != 0)); then
  printf 'integration build failed with status %d\n' "$build_status" | tee -a "$raw_log" >&2
  exit "$build_status"
fi

integration_executable=$(cabal list-bin seki-integration 2>>"$raw_log")

set +e
SEKI_RUN_INTEGRATION=1 \
  SEKI_INTEGRATION_MANIFEST="$manifest" \
  timeout 120s "$integration_executable" 2>&1 | tee -a "$raw_log"
suite_status=${PIPESTATUS[0]}
set -e

manifest_status=0
if ! cmp -s -- "$expected_manifest" "$manifest"; then
  manifest_status=1
  {
    printf '%s\n' 'integration manifest was incomplete or unexpected'
    diff -u -- "$expected_manifest" "$manifest" || true
  } | tee -a "$raw_log" >&2
fi

if ((suite_status != 0)); then
  printf 'integration suite failed with status %d\n' "$suite_status" | tee -a "$raw_log" >&2
  exit "$suite_status"
fi

if ((manifest_status != 0)); then
  exit "$manifest_status"
fi

printf '%s\n' 'integration suite and complete manifest verified' | tee -a "$raw_log"
