# Real-river Integration Tests

Status: **release gate for M0 and later work** / Last updated: 2026-10-17

The `seki-integration` Cabal component exercises seki against a real river
compositor and real Wayland, Xwayland, and input-method clients. It runs river
with a headless backend in a private fixture, so it does not take over the
developer's desktop.

The ordinary `seki-test` suite remains the primary source of precise codec,
transport, protocol, and policy failures. The real-river suite covers the
boundaries that mocks cannot: generated protocol compatibility, compositor
event ordering, client configuration, focus, Xwayland, and IME commits.

## Running the tests

The recommended full check uses the pinned Nix environment:

```console
nix develop --no-write-lock-file .#ci --command ci/run-integration.sh
```

This command:

1. builds `seki-integration`;
2. explicitly enables the real-compositor scenarios;
3. applies a 120-second suite timeout;
4. requires the exact completion manifest; and
5. writes `manifest.txt` and a log limited to the last 1 MiB beneath
   `artifacts/integration/`.

Set `SEKI_INTEGRATION_ARTIFACT_DIR` to place those artifacts elsewhere.

For development with system-installed dependencies, run the component
directly:

```console
SEKI_RUN_INTEGRATION=1 cabal test seki-integration
```

An enabled run treats every missing prerequisite as a failure, not a skip.
Without `SEKI_RUN_INTEGRATION=1`, the component prints a disabled message and
exits successfully so that ordinary `cabal test all` remains usable on systems
without river.

To capture a manifest during a direct run:

```console
SEKI_RUN_INTEGRATION=1 \
SEKI_INTEGRATION_MANIFEST=/tmp/seki-integration-manifest.txt \
cabal test seki-integration
```

The Haskell-only pinned environment used by CI's unit job is available
separately:

```console
nix develop --no-write-lock-file .#unit --command cabal test all
```

## Scenario coverage

| Manifest entry | What it verifies | Additional programs |
| --- | --- | --- |
| `native-wayland-full` | River globals, output and seat discovery, bindings, foot launch, Full dimensions, terminal extent, keyboard focus/input, and close lifecycle | `river`, `foot`, `wtype` |
| `two-output-full` | Two distinct headless outputs and deterministic selection of the complete output with the lowest river object ID | `wlr-randr` |
| `xwayland` | Xwayland availability, Xterm management and app ID, X input focus, and close lifecycle | `xterm`, `xdotool` |
| `fcitx5-mozc` | Private D-Bus and fcitx5 startup, required Wayland IME protocols, Mozc selection, and an exact `日本語` commit into native foot | `dbus-daemon`, `dbus-test-tool`, `gdbus`, `fcitx5`, `fcitx5-remote`, Mozc |

All scenarios also use the core river, foot, and synthetic-input fixture as
needed. The CI shell in `flake.nix` provides the complete dependency set,
including fonts and runtime data.

## Isolation and cleanup

Each scenario creates a fresh mode-0700 runtime directory and a separate
configuration directory. The fixture removes the host's
`WAYLAND_DISPLAY`, `DISPLAY`, `DBUS_SESSION_BUS_ADDRESS`, and input-method
variables before starting children.

```text
seki-integration
├── private XDG_RUNTIME_DIR and XDG_CONFIG_HOME
├── private D-Bus session and fcitx5 state (IME scenario)
├── river with WLR_BACKENDS=headless
│   └── private Xwayland server (X11 scenario)
├── seki connected to river's explicit private socket
├── wtype connected to the same socket
└── foot or xterm
```

River's init command invokes a helper mode of the test executable. The helper
reports the actual `WAYLAND_DISPLAY` and optional `DISPLAY` over a private
local socket, avoiding assumptions about display names and avoiding log
parsing.

The harness passes explicit environments to child processes and opens seki
with `openRiverSessionAt`; it does not change its own process-wide display
variables. Startup and assertions use readiness messages or bounded state
polling rather than fixed sleeps.

Cleanup owns only the fixture's known process groups. It terminates river
first, allowing seki's blocked event loop to observe EOF and return normally,
then terminates remaining children with bounded TERM/KILL waits. Cleanup runs
after success, failure, timeout, and asynchronous exceptions. It never searches
for or kills processes globally by executable name.

## Layer-surface fixture

`seki-layer-surface-fixture` is the exclusive-zone client for WP8's
layer-shell scenario (`docs/m1-plan.md`, WP8 item 8). No manifest entry uses
it yet. It is a native Haskell wlr-layer-shell client built on seki's own
Wayland transport, so it does not link `libwayland` and does not depend on a
desktop bar.

```console
seki-layer-surface-fixture [HEIGHT]
```

`HEIGHT` is a positive pixel count and defaults to 32. The fixture connects
through the standard `WAYLAND_DISPLAY` and `XDG_RUNTIME_DIR` variables (or an
inherited `WAYLAND_SOCKET`). It then does the following:

1. Binds `wl_compositor`, `wl_shm`, and `zwlr_layer_shell_v1`, each at
   version 1. A missing global is an error.
2. Creates one layer surface on the top layer with no output, so the
   compositor chooses the output. The surface is anchored top, left, and
   right, with size (0, `HEIGHT`) and exclusive zone `HEIGHT`, in namespace
   `seki-layer-surface-fixture`.
3. Acks the first `configure`, attaches an XRGB8888 shared-memory buffer of
   the configured width by `HEIGHT`, and commits.
4. Prints exactly one line to stdout, `ready <width> <height>`, after a
   `wl_display.sync` round trip that follows the buffer commit.

At this point river has processed the mapped surface and its exclusive zone.

The fixture tears down on stdin EOF, on SIGTERM, or when the compositor sends
the layer surface's `closed` event. It first waits for a sync, so that every
event already in flight reaches a live object. It then destroys the buffer,
the pool, the layer surface, and the surface, waits for a final sync, closes
the connection, and exits 0. Any protocol or connection error is printed to
stderr and exits 1. An invalid argument exits 2.

Harness requirements:

- Keep the fixture's stdin open on a pipe until the scenario wants it to
  leave. Closing that pipe is the stop signal, so `/dev/null` stops the
  fixture at once.
- Read stdout until the `ready` line. A fixture that exits 0 without a `ready`
  line was closed by the compositor before it mapped.
- River closes every layer surface while the window manager has not bound
  `river_layer_shell_v1`. The fixture then exits 0 without a `ready` line, so
  start it only after seki has bound layer shell.

The fixture's protocol binding,
`tools-generated/layer-surface-fixture/Seki/Fixture/Protocol/WlrLayerShellUnstableV1.hs`,
is integration-only. seki's libraries never import it, and
`test/Seki/Scanner/GenerateTest.hs` fails if it drifts from the vendored XML.
Regenerate it with:

```console
cabal run seki-scanner -- generate Seki.Fixture.Protocol.WlrLayerShellUnstableV1 protocol/river/upstream/wlr-layer-shell-unstable-v1.xml 'river v0.4.5, commit f6d961773711dc29d3eb5b60bd14744a6a33894f' tools-generated/layer-surface-fixture/Seki/Fixture/Protocol/WlrLayerShellUnstableV1.hs protocol/river/upstream/wlr-layer-shell-unstable-v1.xml protocol/wayland/wayland.xml protocol/wayland-protocols/xdg-shell.xml
```

## Completion manifest and CI gate

An enabled successful run must produce exactly:

```text
enabled=true
native-wayland-full=passed
two-output-full=passed
xwayland=passed
fcitx5-mozc=passed
```

`ci/run-integration.sh` byte-compares the generated file with
`ci/expected-integration-manifest.txt`. A missing, failed, skipped, reordered,
or unknown scenario therefore fails the release check. Suite exit status alone
is intentionally insufficient because the disabled component exits
successfully.

`.github/workflows/ci.yml` runs unit and real-river checks as separate Linux
jobs. The integration job always attempts to upload:

- `component-versions.json`, evaluated from
  `integrationVersions.x86_64-linux`;
- the exact completion manifest; and
- the final 1 MiB of combined build and suite output.

The workflow actions, Haskell toolchain, and runtime packages are pinned.
`flake.lock` records the source revisions and hashes. `flake.nix` uses a
separate toolchain input because `cabal.project` requires GHC 9.6.6, while the
runtime input supplies the current pinned river/client closure. CI uses the
`pixman` software renderer and does not depend on a runner GPU.

Inspect the resolved versions with:

```console
nix eval --no-write-lock-file --json .#integrationVersions.x86_64-linux
```

## Failure diagnosis

Every wait reports its phase and last observable state. Fixture failures also
report the discovered Wayland globals, seki inventory, seat bindings, runtime
result, child exit status, and bounded child logs.

The harness retains the last 64 KiB of each child output stream in memory. The
release wrapper retains the last 1 MiB of combined output on disk. These bounds
prevent a protocol or client loop from exhausting memory, disk, or CI output.

Common failure categories are:

- no private `DISPLAY`: river was built or started without Xwayland;
- missing `zwp_input_method_manager_v2` or
  `zwp_text_input_manager_v3`: the compositor/client IME path is unavailable;
- foot without the `+ime` build feature;
- an absent Mozc engine, dictionary, D-Bus utility, font, or runtime file;
- a process exiting before its readiness check;
- a timeout, protocol disconnection, or structured `RiverRuntimeError`.

The suite continues to later scenarios after one scenario fails so the
manifest and output show the complete failure set. It does not retry scenarios
in a way that can hide deterministic failures.

## Maintaining the suite

When adding, removing, or renaming a scenario:

1. update the ordered scenario list in `integration/Main.hs`;
2. update `ci/expected-integration-manifest.txt` in the same order;
3. add required programs and runtime data to the `ci` shell in `flake.nix`;
4. add important component versions to
   `integrationVersions.x86_64-linux`;
5. update the coverage table above; and
6. run the pinned release command and confirm both bounded artifacts.

When updating dependencies, keep the flake inputs, lock file, recorded version
set, and commit-pinned workflow actions consistent. Re-evaluate both
`devShells.x86_64-linux.unit` and `devShells.x86_64-linux.ci`, then run unit
tests and the complete release check.

## Limitations

- The headless backend does not test DRM/KMS, physical input devices, a GPU
  driver, rendering fidelity, or screenshots.
- `wtype` creates a virtual keyboard with its own keymap. The integration
  suite therefore uses unmodified function keys for action dispatch rather
  than claiming to reproduce every physical XKB modifier path.
- Unit tests, rather than this fixture, pin the production
  Mod4-Shift-Return and Mod4-Shift-c defaults.
- River does not report whether a response deadline was missed. The observable
  proxy is a clean runtime result followed by successful dimension, focus,
  input, and close effects.
- The suite currently targets Linux and river 0.4.5 or later.
