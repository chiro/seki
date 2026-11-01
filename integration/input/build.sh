#!/usr/bin/env bash
set -euo pipefail
# Caller owns the private output directory. The resulting DSO is injected only
# into the child river, never into seki, Waybar, or the user's session.
output=${1:?private output directory required}
mkdir -p "$output"
cc -shared -fPIC -Wall -Wextra -Werror "$(dirname "$0")/backend.c" -o "$output/backend.so" \
  $(pkg-config --cflags --libs wlroots-0.20 wayland-server xkbcommon) -ldl
cc -Wall -Wextra -Werror "$(dirname "$0")/keymap.c" -o "$output/keymap" $(pkg-config --cflags --libs xkbcommon)
"$output/keymap" > "$output/two-layouts.xkb"
printf '%s\n' 'not an XKB keymap' > "$output/invalid.xkb"
cc -shared -fPIC -Wall -Wextra -Werror "$(dirname "$0")/hold.c" -o "$output/hold.so"
