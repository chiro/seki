# Real-river Integration Tests

Status: **release gate for M0 and later work** / Last updated: 2026-10-25

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
WLR_RENDERER=pixman SEKI_RUN_INTEGRATION=1 cabal test seki-integration
```

An enabled run treats every missing prerequisite as a failure, not a skip.
Without `SEKI_RUN_INTEGRATION=1`, the component prints a disabled message and
exits successfully so that ordinary `cabal test all` remains usable on systems
without river.

### Running a subset

Set `SEKI_INTEGRATION_SCENARIOS` to a comma-separated list of manifest names
to run only those scenarios. They run in suite order, whatever the order of
the list. An unset or empty value runs every scenario.

```console
SEKI_INTEGRATION_SCENARIOS=m1-tall-three,m2-probe-self-test \
WLR_RENDERER=pixman ci/run-integration.sh
```

A name that matches no scenario fails the run before any scenario starts,
and the error lists the known names. With the variable set,
`ci/run-integration.sh` expects `enabled=true` followed by the lines of
`ci/expected-integration-manifest.txt` for the selected names, and writes
that expectation to `expected-manifest.txt` beside the manifest. Without the
variable, the check compares the full expected manifest.

To capture a manifest during a direct run:

```console
WLR_RENDERER=pixman SEKI_RUN_INTEGRATION=1 \
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
| `m1-tall-three` | Three Tall clients, exact content pixels, positions, and complete borders | `wlr-randr` |
| `m1-tall-messages` | Tall shrink, expand, and master-count geometry | `wlr-randr` |
| `m1-focus-borders` | Focus movement, focused borders, and key delivery to exactly one native client | `wlr-randr` |
| `m1-swap-master` | Master swap preserves focus and client input | `wlr-randr` |
| `m1-layout-cycle` | Tall, Mirror Tall, Full, focus changes in Full, and restored Tall | `wlr-randr` |
| `m1-workspace-visibility` | Shift, visible and empty workspaces, keyboard focus, and restoration | `wlr-randr` |
| `m1-greedy-view` | Two output workspace exchange with distinct layouts and key delivery | `wlr-randr` |
| `m1-layer-exclusive-zone` | Native layer fixture map and teardown change usable area and actual client pixels | `wlr-randr` |
| `m1-layer-focus` | An `on_demand` layer surface keeps keyboard focus through a new window and a layout message; a window click and a focus key binding each take it back, and the window keeps it with a focused border after `focus_none` | `wlr-randr` |
| `m1-output-readd` | Actual WM output removal and addition through two, one, zero, one, and two outputs preserve stacks, layouts, focus, and window identities | `wlr-randr` |
| `m2-probe-self-test` | Each probe window spec field and request against river's state: a custom and an absent app_id, the parent window, the dimensions hint, a fixed commit size, fullscreen and exit fullscreen requests, a move during a held press, and a pointer press, motion, and release with the logo modifier held | `wlr-randr` |
| `m2-manage-shift` | A `doShift` manage hook keyed on a probe window's custom app_id puts it on hidden workspace 2: it is not shown and focus stays on the earlier window. Viewing workspace 2 shows it, and a window with another app_id still tiles on workspace 1 | `wlr-randr` |
| `m2-startup-keys` | A public configuration's startup hook `spawnOnce` runs once after the first output, and an unmodified `XF86AudioRaiseVolume` press from `wtype` runs its bound `spawn`; both append to a file the scenario polls | `wlr-randr` |
| `m2-float-dialog` | Under the default manage hook, a probe window with a parent and a fixed 300 by 200 commit floats over its tiled parent, centered on the output at its own size, with no stored geometry in the core. A click on the parent takes keyboard focus and the focused border, and the dialog stays shown above it | `wlr-randr` |
| `m2-fullscreen` | The focused probe window of two tiles requests fullscreen and covers the whole output, hiding the other tile. Viewing workspace 2 shows its window uncovered, and viewing workspace 1 again shows the fullscreen window covering the output with keyboard focus. It then requests exit and returns to its master tile with its focused border and keyboard focus | `wlr-randr` |
| `m2-size-policy` | A probe window commits 600 by 500 whatever it is proposed, wider than its 396 by 596 master tile. Under the default Crop policy it is clipped to the tile, with its focused border around the clipped content, and the neighbouring tile stays whole. Under `tolerant` it shows at its committed size at the tile's top-left over its neighbour, and a window with a 500 by 100 minimum and a 700 by 400 maximum commits the master tile clamped to 500 by 400 | `wlr-randr` |
| `m2-pointer-drag` | With the logo modifier held, a button 1 press with no motion on the bottom stack tile of three focuses it and leaves it tiled. A button 1 drag of 200 by 150 floats it at its tile's content box moved by that offset while the other two retile, and a second drag moves the float by exactly 100 by 50. A button 3 drag of 60 by 40 grows it from its bottom-right corner by that amount, and a button 3 drag toward a smaller size stops at the window's 300 by 200 minimum with its top-left fixed | `wlr-randr` |
| `m2-client-move` | A 300 by 200 dialog floats centered over its parent. With button 1 held on the dialog, the dialog requests `xdg_toplevel.move`, and two motions of 50 by 30 each move it by each offset. Motion after the release leaves it in place | `wlr-randr` |
| `m2-restart` | The real `seki` executable, started by a bare name found on `PATH` through a symlink, restarts with Mod-q. Workspace 1 holds two tiles under Mirror Tall and a dialog float moved by a logo drag, and workspace 2 holds a fullscreen window. Both workspaces show the same pixels, byte for byte, before and after; the pid stays, argv[0] keeps the symlink, keyboard focus returns to the dialog, and the workspace bindings work after the exec. `ci/run-integration.sh` passes the executable in `SEKI_EXECUTABLE` | `wlr-randr` |
| `m2-restart-fallback` | The restart supervisor runs in the test process with a target that passes the restart check but cannot exec. F3 stops the session, the exec fails, and the next session reconnects and restores: workspace 1's tile and floating dialog and workspace 2's window show the same pictures after the restart | `wlr-randr` |
| `m2-reference-config` | The `seki-reference-config` executable, the ported reference configuration in `test-public/PortedConfig.hs`, runs with a private `HOME` and a `PATH` of stubs that log each launch. Each of its four startup commands runs once after the first output. An `Emacs` app_id window goes to workspace `emacs` while `term` keeps its own two tiles with the port's 1-pixel red and `#0000dd` borders. A click focuses a tile and also reaches its client. Mod-Shift-Return, `XF86AudioRaiseVolume`, and Mod-Shift-l launch their commands. After Mod-q the Emacs window is still on `emacs` with keyboard focus, `XF86AudioLowerVolume` works, and no startup command ran again. `ci/run-integration.sh` passes the executable in `SEKI_REFERENCE_CONFIG_EXECUTABLE` | `wlr-randr` |

All scenarios also use the core river, foot, and synthetic-input fixture as
needed. The CI shell in `flake.nix` provides the integration dependency set,
including fonts, runtime data, and `WLR_RENDERER=pixman`. Direct runs must set
the renderer explicitly because the shared fixture inherits it from the host.

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

## Native window and capture fixture

`Seki.Fixture.Probe` owns colored xdg-shell clients on the explicit private
socket. Its actor acknowledges configures and tracks keyboard enter, leave,
and actual key presses by client identity. One owned `wtype -s 60000` process
keeps a virtual keyboard present for the fixture lifetime. It sends no keys.
Separate short `wtype` commands drive the production policy bindings and input.
The actor waits for observed events rather than a startup delay.

`openWindowWith` takes a `WindowSpec`. `windowSpec color` gives the defaults,
and `openWindow` uses them, so its requests are the same as before the spec
existed. Both return after the window's initial `xdg_surface.configure`.
River sends it only after seki proposes dimensions, and seki proposes none
for a window on a hidden workspace, so `m2-manage-shift` opens its shifted
window on a separate thread and collects it once the workspace is shown. The
spec fields are:

| Field | Request | Default |
| --- | --- | --- |
| `specAppId` | `NumberedAppId` sets `seki-color-probe-N`, `CustomAppId` sets the given text, and `NoAppId` sends no `set_app_id` | `NumberedAppId` |
| `specParent` | `xdg_toplevel.set_parent` to an earlier probe window | none |
| `specMinSize`, `specMaxSize` | `set_min_size` and `set_max_size` | none |
| `specCommit` | `CommitFixed w h` paints `w` by `h` on every configure, whatever size it proposes | `CommitConfigured` |

`requestFullscreen`, `requestExitFullscreen`, and `requestMove` send
`set_fullscreen` with no output, `unset_fullscreen`, and `move` on an open
window, and return after a round trip. `requestMove` uses the serial of the
latest button press on that window and fails when there is none. River
forwards `move` only while that button is still held.

The probe binds `wl_pointer` when the seat advertises a pointer, and binds a
new one when the capability returns after a removal. `pointerSnapshot`
reports the window under the pointer, the surface-local position from the
latest enter or motion, and press and release counts per window.
`KeyboardSnapshot` also reports `depressedModifiers`, the `mods_depressed`
value of the latest `wl_keyboard.modifiers`. `committedSizes` reports the
size each window last acknowledged and committed, which the latency driver
waits on after a resize or a fullscreen change.

Pointer input uses a `zwlr_virtual_pointer_v1` on the default seat.
`withVirtualPointer` opens a native connection and keeps one virtual pointer
for the length of an action, so a button can stay held across steps.
`pointerMoveTo` sends absolute motion in the output layout, and
`pointerPress` and `pointerRelease` send `LeftButton` (0x110) or
`RightButton` (0x111). Each step ends with a frame and a round trip. `click`
moves, presses, and releases the left button on a fresh virtual pointer.
River turns a click on a window into `window_interaction` for seki and a click
on an `on_demand` layer surface into layer keyboard focus.

The shared fixture, `Seki.Integration.Fixture`, takes the policy
configuration of its suite. It records each `fullscreen_requested`,
`exit_fullscreen_requested`, `pointer_move_requested`, and
`pointer_resize_requested` event by river window before the policy handles
the event, and `windowRequests` reads that log. `withHeldLogo` holds the logo
modifier for the length of an action with a separate
`wtype -M logo -s 60000` process. It waits until the focused probe window
reports bit 0x40 in `depressedModifiers`, and, after it stops that process,
until the bit clears.

Each screenshot uses a fresh native connection and the pinned
ext-image-capture-source and ext-image-copy-capture protocols. Capture selects
`wl_output.name`, excludes cursors, and reads real compositor SHM bytes after
frame readiness. The compositor must advertise
`ext_output_image_capture_source_manager_v1` and
`ext_image_copy_capture_manager_v1`, with SHM XRGB8888, ARGB8888, XBGR8888, or
ABGR8888 support.
Clicks require `zwlr_virtual_pointer_manager_v1`.
The fixture accepts only normal transform and unit scale.
Each allocation is limited to 16 MiB and each connection to 128 live buffers.
A window buffer counts until river releases it or the connection closes. Captures keep their
local descriptor until the frame bytes have been read.

The M1 scenarios use literal content rectangles for 800 by 600 outputs.
The oracle checks exact color counts and bounds, every pixel of all four
border strips, and omitted colors on every enabled output. A picture lists
its windows bottom to top. A later window's content and 2-pixel border hide
the pixels under them, so the oracle expects only the visible part of an
earlier window and of its border. Border pixels outside the 800 by 600
frame are not checked, so a fullscreen window's paint covers the whole frame
with no border. It imports no production layout or scene
calculation. Capture protocol failures fail at
once. Only a disagreement in rendered pixels is polled.

The output lifecycle case uses `wlr-randr --off` and `--on` and waits for actual
river inventory removal and addition. This exercises WM output lifecycle,
including complete headless recovery. It does not emulate physical DRM unplug.
Per-workspace stacks, layouts, and focus survive, even when the first restored
head receives the last removed workspace.

On pixel failure, the suite writes the last actual image per output as a PPM
under `SEKI_INTEGRATION_ARTIFACT_DIR`. Each named scenario retains at most two
frames. Inventory and bounded child logs remain in the combined suite log.
The scanner freshness test verifies all committed native fixture bindings.

The release wrapper forwards optional arguments to Cabal. For an installed
local compiler, for example, run
`WLR_RENDERER=pixman ci/run-integration.sh -w ghc-9.10.3 --offline`.
The default command continues to use the pinned compiler from `cabal.project`.

## Layer-surface fixture

`seki-layer-surface-fixture` is the layer-shell client for the exclusive-zone
scenario (`m1-layer-exclusive-zone`) and the layer focus scenario
(`m1-layer-focus`). It is a native Haskell wlr-layer-shell client built on
seki's own Wayland transport, so it does not link `libwayland` and does not
depend on a desktop bar.

```console
seki-layer-surface-fixture [HEIGHT [INTERACTIVITY]]
```

`HEIGHT` is a positive pixel count and defaults to 32. `INTERACTIVITY` is the
layer surface's keyboard interactivity, named as in the protocol: `none` (the
default), `exclusive`, or `on_demand`. The fixture connects
through the standard `WAYLAND_DISPLAY` and `XDG_RUNTIME_DIR` variables (or an
inherited `WAYLAND_SOCKET`). It then does the following:

1. Binds `wl_compositor` and `wl_shm` at version 1, and
   `zwlr_layer_shell_v1` at the version that introduced `INTERACTIVITY`:
   version 1 for `none` and `exclusive`, version 4 for `on_demand`. A missing
   global, or a layer shell older than that version, is an error.
2. Creates one layer surface on the top layer with no output, so the
   compositor chooses the output. The surface is anchored top, left, and
   right, with size (0, `HEIGHT`), exclusive zone `HEIGHT`, and keyboard
   interactivity `INTERACTIVITY`, in namespace `seki-layer-surface-fixture`.
3. Acks the first `configure`, attaches an XRGB8888 shared-memory buffer of
   the configured width by `HEIGHT`, and commits.
4. Prints exactly one line to stdout, `ready <width> <height>`, after a
   `wl_display.sync` round trip that follows the buffer commit.

At this point river has processed the mapped surface and its exclusive zone.
River gives an `on_demand` surface non-exclusive keyboard focus when it maps.

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

The probe's virtual pointer binding,
`tools-generated/layer-surface-fixture/Seki/Fixture/Protocol/WlrVirtualPointerUnstableV1.hs`,
is generated from `protocol/wlr-protocols/wlr-virtual-pointer-unstable-v1.xml`,
copied from the `wayland-protocols-wlr` 0.3.12 crate. The same freshness test
covers it. Regenerate it with:

```console
cabal run seki-scanner -- generate Seki.Fixture.Protocol.WlrVirtualPointerUnstableV1 protocol/wlr-protocols/wlr-virtual-pointer-unstable-v1.xml 'wayland-protocols-wlr 0.3.12' tools-generated/layer-surface-fixture/Seki/Fixture/Protocol/WlrVirtualPointerUnstableV1.hs protocol/wlr-protocols/wlr-virtual-pointer-unstable-v1.xml protocol/wayland/wayland.xml
```

## Completion manifest and CI gate

An enabled successful run must produce exactly:

```text
enabled=true
native-wayland-full=passed
two-output-full=passed
xwayland=passed
fcitx5-mozc=passed
m1-tall-three=passed
m1-tall-messages=passed
m1-focus-borders=passed
m1-swap-master=passed
m1-layout-cycle=passed
m1-workspace-visibility=passed
m1-greedy-view=passed
m1-layer-exclusive-zone=passed
m1-layer-focus=passed
m1-output-readd=passed
m2-probe-self-test=passed
m2-manage-shift=passed
m2-startup-keys=passed
m2-float-dialog=passed
m2-fullscreen=passed
m2-size-policy=passed
m2-pointer-drag=passed
m2-client-move=passed
m2-restart=passed
m2-restart-fallback=passed
m2-reference-config=passed
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
separate toolchain input because `cabal.project` requires GHC 9.10.3, while the
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

The harness retains the last 64 KiB of each child output stream in memory.
On exit, the release wrapper retains the last 1 MiB of combined output in
`integration.log` and removes `integration.raw.log`. The raw log grows during
the run, so the final artifact limit is not a temporary disk-space limit.
Combined output also streams to CI during the run.

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

1. update the ordered scenario lists in `integration/Seki/Integration/Harness.hs`,
   `integration/Seki/Integration/M1.hs`, and `integration/Seki/Integration/M2.hs`;
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
  driver, or hardware rendering fidelity.
- `wtype` creates a virtual keyboard with its own keymap. The integration
  suite therefore uses unmodified function keys for action dispatch rather
  than claiming to reproduce every physical XKB modifier path.
- Unit tests, rather than this fixture, pin the production
  Mod4-Shift-Return and Mod4-Shift-c defaults.
- The pinned river does not enforce a WM response timer. A clean runtime result
  and successful dimension, focus, input, and close effects establish protocol
  behavior. The [latency diagnostic](latency.md) measures seki's budgets separately.
- The pinned suite targets Linux and river 0.5.0. seki's runtime minimum of
  river 0.4.5 does not establish the capture fixture's minimum version. Other
  compositor builds must provide the capture globals and formats listed above.

## External ingress

`m3-external-wake` uses the session-bound internal producer API. It verifies
1000 idle workspace actions without input, arrivals during manage and render,
explicit queue overload, FIFO progress across bounded batches, an enqueue
after the final drain, and completion with all outputs removed and reattached.
Each idle sample starts before admission and ends when the policy workspace
is observed. It does not measure frame presentation. The per-action ceiling
is 50 ms. The fixture polls at 100 microseconds for these samples.

Set `SEKI_INTEGRATION_ARTIFACT_DIR` to retain successful PNG captures named
`external-wake`, `manage-queue`, `render-queue`, `queue-limit`, `burst`,
`drain-race`, and `headless`, with the output name appended. Captures use
native compositor pixels and are outside timed intervals. Existing failure
captures remain PPM. Session closure and stale-token rejection also have unit
coverage. The separate `m3-external-session` lane exercises real shutdown with queued
work and restored-session delayed completion.

`m3-external-session` stops a real session with more than one batch queued,
checks that the leftover batch is discarded, restores workspace state into a
fresh session, and rejects delayed completions through the old producer.
It covers internal restart handover on one river connection lifecycle,
without an exec or an IPC transport.
