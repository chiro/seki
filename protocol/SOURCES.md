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
  - `river/river-layer-shell-v1.xml`
  - `river/river-window-management-v1.xml`
  - `river/river-xkb-bindings-v1.xml`
  - `river/upstream/wlr-layer-shell-unstable-v1.xml`, river's own pinned copy
    of the wlroots protocol. Only the layer-surface integration fixture uses
    it; seki itself does not bind it.
- Upstream: <https://codeberg.org/river/river>
- Tag: `v0.4.5`
- Commit: `f6d961773711dc29d3eb5b60bd14744a6a33894f`
- Source: <https://codeberg.org/river/river/src/tag/v0.4.5/protocol>
- License: MIT (`SPDX-License-Identifier: MIT`); full notices are embedded in
  each XML file

The layer-shell files are byte-identical in river v0.5.0, which the release
integration environment runs.

The GitHub repository at <https://github.com/riverwm/river> is an upstream
mirror and can be used when Codeberg is unavailable.

## river input protocols

- Files: `river/river-input-management-v1.xml`,
  `river/river-libinput-config-v1.xml`, `river/river-xkb-config-v1.xml`
- Upstream: <https://codeberg.org/river/river>
- Tag: `v0.5.0`
- Commit: `cc4a291b2b9375fe3d5fe5a64b6f510b463f888b`
- Source: <https://codeberg.org/river/river/src/tag/v0.5.0/protocol>
- License: MIT; full notices are preserved unchanged in each XML.

These three files differ from v0.4.5. Input management and libinput add
version-2 `done` events. XKB adds version-2 `done` and version-3 scrolllock
requests/events. All v1 request/event opcodes, argument signatures and enum
values are preserved. The new bindings permit v1 negotiation; they do not
raise the runtime minimum above river 0.4.5. The existing river bindings above
retain their v0.4.5 source pin. Arrays remain opaque in generated bindings.

## wayland-protocols

- Files:
  - `wayland-protocols/xdg-shell.xml`
  - `wayland-protocols/ext-image-capture-source-v1.xml`
  - `wayland-protocols/ext-image-copy-capture-v1.xml`
  - `wayland-protocols/ext-foreign-toplevel-list-v1.xml`
- Upstream: <https://gitlab.freedesktop.org/wayland/wayland-protocols>
- Tag: `1.49`
- Commit: `ee78491a237eaff9389a0ccf8680521d074407d3`
- Source: <https://gitlab.freedesktop.org/wayland/wayland-protocols/-/blob/1.49/stable/xdg-shell/xdg-shell.xml>
- License: MIT-style; full notice is embedded in the XML
- Use: the native integration fixture binds xdg-shell and the two image capture
  protocols. The scanner also uses xdg-shell to resolve layer-shell's `xdg_popup`.
  ext-foreign-toplevel-list is a reference for capture-source validation only.
- Staging sources at the same tag and commit:
  - <https://gitlab.freedesktop.org/wayland/wayland-protocols/-/blob/1.49/staging/ext-image-capture-source/ext-image-capture-source-v1.xml>
  - <https://gitlab.freedesktop.org/wayland/wayland-protocols/-/blob/1.49/staging/ext-image-copy-capture/ext-image-copy-capture-v1.xml>
  - <https://gitlab.freedesktop.org/wayland/wayland-protocols/-/blob/1.49/staging/ext-foreign-toplevel-list/ext-foreign-toplevel-list-v1.xml>

## wlr-protocols

- File: `wlr-protocols/wlr-virtual-pointer-unstable-v1.xml`
- Upstream: <https://gitlab.freedesktop.org/wlroots/wlr-protocols>
- Commit: `bf4fc79abc359eea5a0edec0ac6d4a2b2955f82a`; the file is unchanged
  since `c11408942e2fb54d41dadb84cdf844331076ae11`
- Source: <https://gitlab.freedesktop.org/wlroots/wlr-protocols/-/blob/bf4fc79abc359eea5a0edec0ac6d4a2b2955f82a/unstable/wlr-virtual-pointer-unstable-v1.xml>
- License: MIT-style; full notice is embedded in the XML
- Use: the integration probe clicks windows through a virtual pointer. seki
  itself does not bind it.
