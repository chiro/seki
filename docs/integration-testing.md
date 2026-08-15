# Real-river Integration Testing Design

Status: **implemented — I0 through I4 complete** / Last updated: 2026-08-16

Implementation status: the opt-in component, native Wayland Full lifecycle,
two-output Full selection, isolated Xwayland lifecycle, and isolated fcitx5/Mozc
commit scenario from I0–I3 are implemented and pass against river 0.4.6, foot
1.27.0, wlr-randr 0.5.0, Xwayland 24.1.13, Xterm 410, fcitx5 5.1.21, and
fcitx5-mozc 3.34.6239.2. I4 adds the pinned Nix CI environment, separate
enabled integration job, exact release-manifest gate, and bounded artifacts.

This note defines how seki's end-to-end tests will run against a real river
compositor without taking over the developer's desktop. It turns the manual M0
validation into a repeatable test while keeping the ordinary unit and mock
protocol suites fast.

## 1. Goals

The integration suite must verify that:

- river, seki, and real Wayland clients agree on the generated protocol;
- seki discovers outputs, seats, and windows and completes every observed
  manage and render sequence without a structured runtime error;
- a keybinding can launch a terminal and a second keybinding can close it;
- the Full policy configures a terminal to occupy the headless output;
- keyboard focus reaches the managed terminal;
- an X11 client is managed through river's Xwayland integration; and
- fcitx5 with Mozc can commit the exact UTF-8 text `日本語` into a native
  Wayland client.

The suite must be isolated from the host Wayland display, X server, D-Bus
session, fcitx configuration, and keyboard shortcuts. A failed test must not
leave compositor, Xwayland, terminal, or input-method processes behind.

## 2. Non-goals

- Replacing the pure, mock-compositor, codec, or transport tests. Those remain
  the primary source of precise failure localization.
- Testing DRM/KMS, physical input devices, or a particular GPU driver. The
  integration compositor uses river's headless backend.
- Pixel comparison or screenshot testing. M0 has no visual decoration whose
  appearance needs to be asserted.
- Performing real-compositor work during an ordinary `cabal test all`
  invocation. The disabled integration component itself may still be invoked
  and exit successfully.
- Proving that a synthetic keyboard has exactly the same keysym normalization
  as every physical keyboard and XKB keymap.

## 3. Test-suite Interface

Add a separate Cabal test suite named `seki-integration`. It is opt-in:

```console
SEKI_RUN_INTEGRATION=1 cabal test seki-integration
```

Without `SEKI_RUN_INTEGRATION=1`, the executable prints that the real-river
suite is disabled and exits successfully. This keeps `cabal test all` suitable
for machines without river and the client programs.

Once explicitly enabled, missing prerequisites are failures, not silent skips.
This distinction prevents release validation from appearing successful when,
for example, river was installed without Xwayland support.

Each enabled run writes a line-oriented completion manifest when
`SEKI_INTEGRATION_MANIFEST` names an output path. The manifest records that the
suite was enabled and the result of every required scenario, for example:

```text
enabled=true
native-wayland-full=passed
two-output-full=passed
xwayland=passed
fcitx5-mozc=passed
```

The CI job must require this file and assert the complete set of scenario names
and `passed` results. Exit status alone is insufficient because the deliberately
disabled suite also exits successfully.

`ci/run-integration.sh` is the release entry point. It builds the integration
component, runs it with both opt-in variables, and byte-compares the result with
`ci/expected-integration-manifest.txt`. It fails when the manifest is missing,
partial, reordered, contains a failed scenario, or contains an unknown scenario.

The normal test suite remains `seki-test`; no real compositor is started by
that component.

## 4. Process Topology

Each scenario owns a fresh fixture:

```text
seki-integration
├── private XDG_RUNTIME_DIR and XDG_CONFIG_HOME
├── private D-Bus session (IME scenario only)
│   └── fcitx5 + Mozc and the virtual-keyboard D-Bus backend
├── river: WLR_BACKENDS=headless, one or two headless outputs
│   └── Xwayland (X11 scenario only)
├── seki runtime connected to river's private Wayland socket
├── wtype connected only to that socket
└── client under test
    ├── foot + terminal probe
    └── xterm
```

The harness does not change its own process-wide `WAYLAND_DISPLAY` or
`DISPLAY`. It passes a constructed environment to each child and opens seki's
connection with `openRiverSessionAt`. This prevents a test from accidentally
connecting to the user's session and avoids global environment races.

River's init command runs a helper mode of the integration-test executable. The
helper reports the nested `WAYLAND_DISPLAY` and, when available, `DISPLAY` to
the parent over a private local socket. The harness therefore does not assume a
name such as `wayland-1` or parse human-oriented river log messages.

## 5. Fixture Lifecycle

For every scenario, the harness performs these steps:

1. Check that the scenario's executables and runtime data are available.
2. Create a mode-0700 temporary runtime directory and a separate temporary
   configuration directory.
3. Remove host `WAYLAND_DISPLAY`, `DISPLAY`, `DBUS_SESSION_BUS_ADDRESS`, and
   input-method variables from river's environment.
4. Start river with `WLR_BACKENDS=headless` and the number of headless outputs
   requested by the scenario.
5. Wait for the river-init helper to report the private display names.
6. Build `defaultRuntimeHandlers` from the scenario's `DefaultConfig`, open
   seki with `openRiverSessionAt`, and run `runRiverSession` in a worker thread.
7. Wait until the output, seat, and configured keybindings are visible in
   seki's runtime snapshot.
8. Start the clients and input driver required by the scenario.
9. Wait for observable state transitions, each with a diagnostic timeout.
10. Terminate river, wait for seki's event loop to observe `DispatchFinished`
    and return, then terminate any remaining fixture children and remove the
    temporary directories.

Cleanup uses bracketed resource ownership and runs after success, assertion
failure, timeout, or asynchronous exception. `runRiverSession` has no external
wakeup or shutdown primitive: concurrently calling `closeRiverSession` while
its event loop blocks in `dispatchNextEvent` would race with the read. The
harness therefore terminates river first and gives the event loop a bounded
wait to return normally on EOF. A bounded fallback kills only the fixture's
known child process groups. The harness never searches broadly for user
processes by executable name. Adding a runtime wakeup may be reconsidered if
this ordering proves insufficient, but it is not required by the initial
design.

Startup and assertions use readiness messages or bounded polling of seki's
runtime state. Fixed sleeps are not synchronization. A small leading delay may
be passed to `wtype` only to let its virtual-keyboard object finish registering;
the actual assertion still waits for a state transition with a deadline.

## 6. Scenarios and Assertions

### 6.1 Native Wayland Full lifecycle

Prerequisites: `river`, `foot`, and `wtype`.

The harness runs seki in-process with the default Full policy and test-specific
unmodified function-key bindings. It does not call `runDefault`, because that
entry point selects its connection from the process environment. Instead it
uses the same policy construction and runtime path explicitly:

```text
defaultRuntimeHandlers testConfig
  -> openRiverSessionAt privateSocket
  -> runRiverSession
```

It then:

1. verifies that river advertises the required window-manager and XKB-binding
   globals at supported versions;
2. verifies that seki records one usable output and one seat with initialized
   bindings;
3. injects the terminal binding and waits for a new foot window in the runtime
   inventory;
4. verifies that `riverWindowDimensions` exactly matches the dimensions of the
   selected output;
5. waits for a terminal probe to report its final terminal size;
6. sends a known line of input and verifies that the probe receives it, proving
   that keyboard focus reached the client;
7. injects the close binding; and
8. waits for both the window to leave seki's inventory and the foot process to
   exit.

The terminal probe is another mode of the integration-test executable, run by
foot. It reports `TIOCGWINSZ` rows, columns, and pixel dimensions, then reports
received UTF-8 input over a private local socket. Foot uses a generated minimal
configuration with a fixed font and zero padding. The reported pixel extent is
checked separately from river's window dimensions. Foot quantizes its terminal
grid to whole cells, so the probe may be less than one cell narrower or shorter
than the window. The harness derives cell width and height at run time from the
reported pixel extent and column/row counts; it does not use a hand-picked
tolerance. The extent must be positive, no larger than the actual window, and
short by less than one derived cell on each axis. This checks actual client
configuration rather than merely rechecking the pure Full-plan function.

The suite uses unmodified function keys for action dispatch because `wtype`
creates a virtual keyboard with its own keymap, and modifier/letter
normalization is not a faithful model of every hardware XKB path. Existing unit
tests continue to pin the production defaults, Mod4-Shift-Return and
Mod4-Shift-c. Exact physical-keyboard chord compatibility remains a short
on-device release check until a suitable unprivileged input backend can model
it reliably.

### 6.2 Xwayland window lifecycle

Prerequisites: the core prerequisites, `xterm`, `xdotool`, and a river build
with Xwayland. `xdotool` is used only to query the private X server's input
focus; keybinding injection still goes through `wtype` and river.

The river-init helper must report `DISPLAY`; otherwise the scenario fails with
an explicit Xwayland-unavailable message. The harness starts xterm with the
private `DISPLAY`, then verifies that:

- a new river window appears in seki's inventory;
- it receives normal manage and render processing without a protocol error;
- its `riverWindowAppId` is unconditionally one of the expected `xterm` or
  `XTerm` values derived by river from `WM_CLASS`; and
- it can be focused and closed through the same policy path as a native client.

The assertion does not depend on PID metadata because river may mark a PID as
unreliable.

### 6.3 fcitx5 and Mozc commit

Prerequisites: the core prerequisites, `dbus-daemon`, `dbus-test-tool`,
`gdbus`, `fcitx5`, `fcitx5-remote`, the Mozc engine and dictionaries, and a
foot build with IME support.

This scenario starts a private D-Bus session and fcitx5 with fresh configuration
under the fixture directory. Before launching clients, the harness requires
river's registry to contain both `zwp_input_method_manager_v2` for fcitx5 and
`zwp_text_input_manager_v3` for foot. It also verifies foot's advertised build
features include IME support. A missing protocol or client feature fails with a
specific prerequisite error rather than a commit timeout.

The generated foot configuration fixes the settings needed by the probe, and
the probe emits foot's IME-enable DECSET sequence before reading input instead
of inheriting an installed release's current default. The harness starts a
persistent `wtype` virtual keyboard before foot so the nested seat has keyboard
capability, waits for focus, and selects Mozc through the private D-Bus session.

River does not route `wtype`'s virtual-keyboard events through the input-method
keyboard grab. The harness therefore owns fcitx5's virtual-keyboard UI name on
the private bus, enables its full-function backend, and sends the romaji,
conversion, and commit key events through `ProcessKeyEvent`. These events target
foot's focused native Wayland input context and Mozc commits through
`input-method-v2`/`text-input-v3`. A final `wtype` Return supplies only the line
delimiter after the conversion has committed. The probe must receive a line
whose decoded UTF-8 contents are exactly:

```text
日本語
```

Running on river's headless backend prevents the host compositor from consuming
the activation chord or conversion keys. Fresh configuration prevents the
user's fcitx state and learned conversion history from affecting the test.

### 6.4 Two-output Full selection

Prerequisites: the core prerequisites and `wlr-randr`.

This scenario starts two headless outputs and configures them with distinct
dimensions through the standard output-management protocol. After both outputs
have reported complete, non-overlapping rectangles, the harness verifies that
`selectFullOutputRectangle` chooses the rectangle belonging to the lowest river
object ID. It then launches foot and requires `riverWindowDimensions` to match
that selected output rather than the other output.

Using distinct dimensions makes an incorrect selection observable. This adds a
real-compositor check for output enumeration and deterministic Full selection;
the pure tests continue to cover incomplete-output filtering precisely.

## 7. Failure Reporting

Every wait has a named timeout and reports the last observable state. On
failure, the suite prints:

- the failed scenario and phase;
- discovered Wayland globals and negotiated versions;
- the final seki inventory and keybinding state;
- the full `Either RiverRuntimeError ()` returned by seki's event loop,
  including any nested `RiverSequenceError`, plus exit status for river and
  each client;
- bounded river, client, and fcitx logs; and
- the fixture path when artifact preservation is requested.

Logs are bounded so a protocol loop cannot exhaust disk or swamp CI output.
Successful fixtures are removed. A dedicated environment switch may preserve
a failed fixture for local diagnosis, but it must be opt-in and print the exact
path retained.

Protocol disconnection, a sequence error, premature process exit, or timeout is
always a test failure. River does not notify the client that a response deadline
was missed, so the suite cannot assert that deadline directly. Its observable
proxy is that the event loop returns no structured error and subsequent
dimension, focus/input, and close effects are all accepted and observed. The
harness may record sequence turnaround duration for diagnostics, but it must not
present that measurement as a compositor-reported deadline result. The harness
must not retry a complete scenario in a way that hides a deterministic failure.

## 8. Dependency and Platform Policy

The suite is Linux-only. The initial supported integration target is river
0.4.5 or later, matching seki's protocol requirement. Release validation must
record the exact versions of river, foot, Xwayland, fcitx5, Mozc, and the output
configuration tool.

The headless backend still needs a renderer. Local runs may use the system Mesa
stack. Hosted CI explicitly sets `WLR_RENDERER=pixman`; it does not guess at or
depend on a runner render device.

The test has no network dependency. All protocol definitions and generated
bindings come from the repository, and all external programs must already be
installed.

The hosted jobs install Nix through a commit-pinned action and evaluate
`flake.nix`. Its runtime nixpkgs input is pinned to
`0e251e24a4f24e036a084b6b4b2d2491af4167f4`, while its toolchain input is pinned
to `50ab793786d9de88ee30ec4e4c24fb4236fc2674`, the revision supplying the exact
GHC 9.6.6 required by `cabal.project`. Together they pin the compiler and the
complete river/foot/Xwayland/wtype/wlr-randr/fcitx5/Mozc tool closure. The
integration job evaluates `integrationVersions.x86_64-linux` into its artifact
before the test, so every run records both revisions, the resolved component
versions, and the `pixman` renderer choice.

## 9. Implementation Sequence

### I0: Fixture and native lifecycle

- Add the opt-in `seki-integration` Cabal component.
- Implement private runtime directories, river startup handshake, explicit
  seki connection through `defaultRuntimeHandlers` and `openRiverSessionAt`,
  timeouts, logs, and river-first process-group cleanup.
- Implement the terminal probe and native launch/focus/Full/close scenario.

Acceptance: the scenario passes repeatedly and leaves no processes or files
behind; forced failures produce useful diagnostics.

### I1: Two-output Full

- Start and configure two differently sized headless outputs.
- Assert deterministic output selection and matching actual window dimensions.

Acceptance: the lowest-object-ID output is selected regardless of event order.

### I2: Xwayland

- Capture river's private `DISPLAY` through the init helper.
- Add the isolated xterm lifecycle scenario.

Acceptance: a missing Xwayland build fails explicitly, and an Xterm window is
managed and closed on an enabled build.

### I3: Input method

- Add private D-Bus and fcitx5 lifecycle management.
- Preflight both Wayland IME protocols and foot's IME build feature, and
  explicitly enable IME in the terminal probe.
- Add the exact Mozc commit assertion.

Acceptance: the terminal probe receives exactly `日本語` without manual input or
access to the host compositor.

### I4: CI and release entry point

- Install pinned integration dependencies in a Linux CI job.
- Run the opt-in suite separately from unit tests.
- Require and validate the complete scenario manifest.
- Preserve bounded logs for failures and record component versions.

Acceptance: CI cannot report the integration job as passed when a required
scenario was disabled or skipped.

Implemented by `.github/workflows/ci.yml`, `flake.nix`, and
`ci/run-integration.sh`. Unit/mock tests and the enabled real-river suite run as
separate jobs. The integration job always uploads its version record, exact
manifest, and a log truncated to the last 1 MiB; the release script gates
success on both the suite exit status and byte-for-byte complete manifest.

## 10. Initial Feasibility Result

On 2026-08-15, the implemented I0–I3 suite passed locally against river 0.4.6.
It exercised one- and two-output headless fixtures, native and Xwayland client
lifecycles, and an exact Mozc `日本語` commit over river's Wayland input-method
protocols. Every scenario used private runtime/configuration directories; the
IME scenario additionally used a private D-Bus session and fresh fcitx5 state.
