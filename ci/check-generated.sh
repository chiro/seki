#!/usr/bin/env bash
# Regenerates every committed protocol binding with seki-scanner and fails,
# naming the file, when a committed binding differs from the scanner's output
# or when a file under src-generated/ or tools-generated/ has no generation rule.
set -euo pipefail

cd "$(dirname "$0")/.."

wayland='Wayland 1.25.0, commit 3e673a438b0a9749e3bdf5cac4befac86333024c'
river045='river v0.4.5, commit f6d961773711dc29d3eb5b60bd14744a6a33894f'
river050='river v0.5.0, commit cc4a291b2b9375fe3d5fe5a64b6f510b463f888b'
protocols149='wayland-protocols 1.49, commit ee78491a237eaff9389a0ccf8680521d074407d3'
wlr='wayland-protocols-wlr 0.3.12'
fixture=tools-generated/layer-surface-fixture/Seki/Fixture/Protocol

wl=protocol/wayland/wayland.xml
wm=protocol/river/river-window-management-v1.xml
xkb=protocol/river/river-xkb-bindings-v1.xml
layer=protocol/river/river-layer-shell-v1.xml
input=protocol/river/river-input-management-v1.xml
xdg=protocol/wayland-protocols/xdg-shell.xml
toplevel=protocol/wayland-protocols/ext-foreign-toplevel-list-v1.xml
source=protocol/wayland-protocols/ext-image-capture-source-v1.xml
copy=protocol/wayland-protocols/ext-image-copy-capture-v1.xml
wlrlayer=protocol/river/upstream/wlr-layer-shell-unstable-v1.xml
pointer=protocol/wlr-protocols/wlr-virtual-pointer-unstable-v1.xml
libinput=protocol/river/river-libinput-config-v1.xml
xkbconfig=protocol/river/river-xkb-config-v1.xml

cabal build -v0 exe:seki-scanner
scanner=$(cabal list-bin -v0 exe:seki-scanner)
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT

failures=0
expected=()

generate() {
  local module=$1 pin=$2 output=$3 primary=$4
  shift 4
  expected+=("$output")
  if ! "$scanner" generate "$module" "$primary" "$pin" "$scratch/$output" "$primary" "$@"; then
    echo "check-generated: seki-scanner failed to generate $output" >&2
    failures=$((failures + 1))
  elif ! diff -u --label "$output (committed)" --label "$output (regenerated)" "$output" "$scratch/$output"; then
    echo "check-generated: $output differs from seki-scanner output" >&2
    failures=$((failures + 1))
  fi
}

generate Seki.Protocol.Wayland "$wayland" src-generated/Seki/Protocol/Wayland.hs "$wl"
generate Seki.Protocol.RiverWindowManagementV1 "$river045" src-generated/Seki/Protocol/RiverWindowManagementV1.hs "$wm" "$wl" "$xkb"
generate Seki.Protocol.RiverXkbBindingsV1 "$river045" src-generated/Seki/Protocol/RiverXkbBindingsV1.hs "$xkb" "$wl" "$wm"
generate Seki.Protocol.RiverLayerShellV1 "$river045" src-generated/Seki/Protocol/RiverLayerShellV1.hs "$layer" "$wl" "$wm"
generate Seki.Protocol.RiverInputManagementV1 "$river050" src-generated/Seki/Protocol/RiverInputManagementV1.hs "$input" "$wl"
generate Seki.Protocol.RiverLibinputConfigV1 "$river050" src-generated/Seki/Protocol/RiverLibinputConfigV1.hs "$libinput" "$wl" "$input"
generate Seki.Protocol.RiverXkbConfigV1 "$river050" src-generated/Seki/Protocol/RiverXkbConfigV1.hs "$xkbconfig" "$wl" "$input"
generate Seki.Fixture.Protocol.WlrLayerShellUnstableV1 "$river045" "$fixture/WlrLayerShellUnstableV1.hs" "$wlrlayer" "$wl" "$xdg"
generate Seki.Fixture.Protocol.WlrVirtualPointerUnstableV1 "$wlr" "$fixture/WlrVirtualPointerUnstableV1.hs" "$pointer" "$wl"
generate Seki.Fixture.Protocol.XdgShell "$protocols149" "$fixture/XdgShell.hs" "$xdg" "$wl" "$toplevel"
generate Seki.Fixture.Protocol.ExtImageCaptureSourceV1 "$protocols149" "$fixture/ExtImageCaptureSourceV1.hs" "$source" "$wl" "$toplevel"
generate Seki.Fixture.Protocol.ExtImageCopyCaptureV1 "$protocols149" "$fixture/ExtImageCopyCaptureV1.hs" "$copy" "$wl" "$toplevel" "$source"

while read -r file; do
  if ! printf '%s\n' "${expected[@]}" | grep -q -x -F "$file"; then
    echo "check-generated: $file has no generation rule in ci/check-generated.sh" >&2
    failures=$((failures + 1))
  fi
done < <(find src-generated tools-generated -type f | sort)

if [ "$failures" -ne 0 ]; then
  exit 1
fi
echo "check-generated: ${#expected[@]} bindings match seki-scanner output"
