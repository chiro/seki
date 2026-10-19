#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
scratch=$(mktemp -d)
trap 'rm -rf -- "$scratch"' EXIT

cabal build lib:seki-wire "$@"
compiler_version=$(cabal exec "$@" -- ghc --numeric-version)
package_db="dist-newstyle/packagedb/ghc-$compiler_version"
wire_package=$(cabal exec "$@" -- ghc-pkg --package-db="$package_db" field z-seki-z-seki-wire id --simple-output)

manage_requests=(
  "proposeWindowDimensions w 100 100"
  "fullscreenWindow w o"
  "exitWindowFullscreen w"
  "informWindowFullscreen w"
  "informWindowNotFullscreen w"
)

{
  cat <<'HASKELL'
module Valid where
import Seki.Run.Phase (ManageM, RenderM, exitWindowFullscreen, fullscreenWindow, hideWindow, informWindowFullscreen, informWindowNotFullscreen, proposeWindowDimensions)
import Seki.Protocol.RiverWindowManagementV1 (RiverOutputV1, RiverWindowV1)
import Seki.Wire.Connection.ObjectTable (Object)
render :: Object RiverWindowV1 -> RenderM ()
render = hideWindow
HASKELL
  for index in "${!manage_requests[@]}"; do
    echo "manage$index :: Object RiverWindowV1 -> Object RiverOutputV1 -> ManageM ()"
    echo "manage$index w o = const (${manage_requests[$index]}) o"
  done
} >"$scratch/Valid.hs"

invalid_sources=(RenderInManage)
sed 's/render :: Object RiverWindowV1 -> RenderM ()/render :: Object RiverWindowV1 -> ManageM ()/' \
  "$scratch/Valid.hs" >"$scratch/RenderInManage.hs"
for index in "${!manage_requests[@]}"; do
  invalid_sources+=("ManageInRender$index")
  sed "s/^manage$index :: \(.*\) -> ManageM ()/manage$index :: \1 -> RenderM ()/" \
    "$scratch/Valid.hs" >"$scratch/ManageInRender$index.hs"
done

compile() {
  cabal exec "$@" -- ghc -dynamic -fno-code -package-db "$package_db" -package-id "$wire_package" "$source_file"
}

source_file="$scratch/Valid.hs"
compile "$@"
for invalid in "${invalid_sources[@]}"; do
  source_file="$scratch/$invalid.hs"
  if compile "$@" >"$scratch/$invalid.log" 2>&1; then
    echo "check-phase-types: $invalid unexpectedly compiled" >&2
    exit 1
  fi
  if ! grep -qE 'ManagePhase|RenderPhase' "$scratch/$invalid.log"; then
    cat "$scratch/$invalid.log" >&2
    echo "check-phase-types: $invalid failed for a reason other than phase separation" >&2
    exit 1
  fi
  echo "check-phase-types: $invalid rejected by the phase types"
done
