# Vendored Protocol Sources

The protocol definitions in this directory are inputs to seki's binding
generator. They are vendored to make generation reproducible and are kept
unaltered, including their embedded copyright and license notices.

Verify the files from the repository root with:

```console
sha256sum --check protocol/SHA256SUMS
```

## Wayland core protocol

- File: `wayland/wayland.xml`
- Upstream: <https://gitlab.freedesktop.org/wayland/wayland>
- Tag: `1.25.0`
- Commit: `3e673a438b0a9749e3bdf5cac4befac86333024c`
- Source: <https://gitlab.freedesktop.org/wayland/wayland/-/blob/1.25.0/protocol/wayland.xml>
- License: MIT-style; full notice is embedded in the XML

## river protocols

- Files:
  - `river/river-window-management-v1.xml`
  - `river/river-xkb-bindings-v1.xml`
- Upstream: <https://codeberg.org/river/river>
- Tag: `v0.4.5`
- Commit: `f6d961773711dc29d3eb5b60bd14744a6a33894f`
- Source: <https://codeberg.org/river/river/src/tag/v0.4.5/protocol>
- License: MIT (`SPDX-License-Identifier: MIT`); full notices are embedded in
  each XML file

The GitHub repository at <https://github.com/riverwm/river> is an upstream
mirror and can be used when Codeberg is unavailable.
