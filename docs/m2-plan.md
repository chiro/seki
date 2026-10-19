# M2 Practical Implementation Plan

Status: **Draft** / Last updated: 2026-10-20

This document turns the M2 row in the specification into an ordered, testable
implementation plan. It builds on the completed [M1 plan](m1-plan.md) and
narrows the spec's original M2 row to the work that a ported xmonad
configuration needs, plus the floating, restart, and size-mismatch behavior
that daily use hits first.

Design questions are tracked in [Open design issues](#9-open-design-issues).
All six issues are **Decided**, and M2-D01, M2-D02, and M2-D03 are
**Verified**. Each other issue becomes **Verified** when its required
regression lands.

## 1. Outcome

M2 makes seki able to run a ported personal xmonad configuration. The reference
configuration is the maintainer's `xmonad.hs`, which uses a custom terminal and
border colors, `Tall ||| Mirror Tall ||| Full` under `avoidStruts`, a
`className`-based `manageHook` with `doShift`, a `startupHook` built from
`spawnOnce`, `ewmhFullscreen`, and modifier-less media and brightness keys
added with `additionalKeys`.

At the end of M2, the default `seki` process and the public `Seki` API must:

1. run a pure `ManageHook` on every new window, with `appId`, `title`,
   `isDialog`, `composeAll`, `(-->)`, `(=?)`, `doShift`, `doFloat`, and
   `doSink`;
2. run a `startupHook` once per session, with `spawnOnce` that does not
   repeat after a restart;
3. provide `additionalKeys` and `removeKeys`, and accept bindings with no
   modifier;
4. place floating windows above the tiled layer in output-relative logical
   coordinates, float dialogs by default, and expose `float` and `sink`
   actions;
5. move and resize floating windows with mod plus a pointer drag, and honor
   client move and resize requests for floating windows;
6. honor client fullscreen requests and provide a `toggleFullscreen` action;
7. apply a per-layout `Crop` or `Tolerate` policy when a client commits
   dimensions other than the proposed tile; and
8. restart into a new seki binary without losing workspaces, stack order,
   floating geometry, or layout state, by matching windows on river's
   version 4 identifier.

The ported reference configuration compiles against the public API and runs
in the release integration suite.

### 1.1 M1 work reused as complete

- The StackSet already holds the `floating` map with internal `float` and
  `sink`, and its properties pass.
- The inventory already stores each window's version 4 identifier, app_id,
  title, parent, and dimension hints.
- `setWindowCapabilities`, `setWindowContentClipBox`, and `setWindowClipBox`
  already have phase-safe wrappers.
- The real-river harness, the virtual-pointer probe, and the latency harness
  are release-gated.

### 1.2 Moved out of M2

These items were in the spec's M2 row or M1's deferral list. The reference
configuration does not use them.

| Item | New milestone | Reason |
|---|---|---|
| `inputRules` and the three input protocols | M3 | Laptop touchpad and keyboard settings are a daily-use need, not a porting need |
| `seki --recompile`, `--check`, and the configuration skeleton | M3 | M3 already owns the skeleton; M2 restarts into a binary the user built |
| Submaps and binding repeat | M4 onwards | They need a WM-side wake source that no other M2 feature needs |
| Focus-follows-mouse and the `focusModel` field | M4 onwards | The reference configuration sets `focusFollowsMouse = False` |
| Layer-shell focus arbitration beyond M1-D01 | M4 onwards | No design is recorded yet |

Bar output through EWMH, which the reference configuration gets from `ewmh`,
remains M3's IPC work. Session locking stays delegated to river and an
external locker.

## 2. Reference Baseline and Compatibility Rules

### 2.1 xmonad baseline

Port semantics from **xmonad 0.18.1**, the M1 pin, and from
**xmonad-contrib 0.18.1** for `ManageHook` combinators, `EZConfig`'s
`additionalKeys` and `removeKeys`, `SpawnOnce`, and `ewmhFullscreen`'s
`doFullFloat` behavior. Record the contrib tarball SHA-256 beside the M1 pin.

Port the xmonad ManageHook and Floating property suites where they apply.
Neither release archive has a ManageHook property suite, so WP2's properties
are written from `XMonad.ManageHook`'s definitions: `(=?)`, `(-->)`, and
`composeAll` against the Endos they build, and `doShift` and `doSink` against
`shiftWin` and `sink`. The
X11 resize and aspect-hint properties that M1 excluded return only where river
supplies the input. River supplies minimum and maximum dimensions and no
aspect ratio or size increment, so the aspect and increment properties stay
excluded with that reason recorded.

### 2.2 River baseline

The baseline remains river 0.4.5 or later with window management version 4.
M2 binds no new globals. Restoration requires version 4 identifiers. Against
version 3 it places every window with the normal initial-placement rules.

### 2.3 Semantic deviations must be named

Each deviation from xmonad gets a test and a module-doc note:

- `className =?` has no direct equivalent. Wayland clients report `app_id`,
  Xwayland clients report their class through the same field, and either may
  be absent;
- `isDialog` means "has a parent window", because river reports no window
  type; and
- `spawnOnce` state survives a seki restart through the state file, not
  through xmonad's extensible-state serialization.

## 3. Target Architecture

### 3.1 Data path

```text
river events -> Inventory -> CoreEvent (WindowCreated carries WindowProps)
             -> stepCore: ManageHook, Action, pointer op
             -> CoreState (StackSet + fullscreen set + pointer op + spawned-once set)
             -> planScene: tiles, floats, fullscreen
             -> reconcileManage / reconcileRender
```

### 3.2 New core types

- `WindowProps` holds `appId :: Maybe Text`, `title :: Maybe Text`,
  `parent :: Maybe WindowId`, and the dimension hints. The runtime fills it
  from the inventory at `manage_start`, after river guarantees these events
  have arrived.
- `Query a` is a reader over `WindowProps`. `ManageHook = Query (Endo
  StackSet)`, as in xmonad. It is pure and has no IO.
- `Placement` gains `Floating Rect`, `Centered Rect` for a float with no
  stored box, and `Fullscreen OutputId` beside `Omitted` and `Placed Rect`.
- `CoreState` gains a fullscreen set, the float raise order (M2-D02), an
  optional in-progress pointer operation, and a spawned-once set. These live beside the StackSet, not in it,
  so the ported StackSet properties remain unchanged.
- `SizePolicy = Crop | Tolerate` is a `LayoutClass` method with a `Crop`
  default and a `tolerant` modifier.
- `RestoreState` is a versioned record of identifier-keyed window entries,
  workspace layout state, output assignment by `wl_output` name, floating
  geometry, the float raise order, the fullscreen set, and the spawned-once
  set.

## 4. Work Packages

Each package lands with its unit tests, its real-river scenario, and its
documentation. A package is done only when all three land.

### WP1. Harness extensions

Deliverables:

- a scenario filter for the integration runner, so one scenario runs alone;
- probe windows that set `app_id`, set a parent, set minimum and maximum
  size, request `set_fullscreen` and `move`, and commit a size other than
  the configured one on demand; and
- probe pointer steps for press, absolute motion, and release with a held
  modifier, for mod plus drag.

Tests:

- the existing 14 scenarios pass unchanged, alone and as a suite; and
- a probe self-test checks each new request against river's echoed state.

### WP2. Window properties and ManageHook

Deliverables:

- `WindowProps` and `Query`, with `appId`, `title`, `isDialog`, `(=?)`,
  `(-->)`, `composeAll`, `doShift`, `doSink`, and `idHook`;
- a `manageHook` field in `Config` and `PolicyConfig`, run by `stepCore` in
  place of the bare `manageWindow` insert; and
- `def`'s `manageHook = idHook` until WP4 adds `isDialog --> doFloat`.

Tests:

- ported xmonad ManageHook properties and composition-order examples;
- a hook keyed on a missing `app_id` does not match;
- `doShift` to a hidden workspace hides the new window and keeps focus; and
- real-river scenario `m2-manage-shift`.

### WP3. Startup hook and key helpers

Deliverables:

- `startupHook :: Action l ()`, run once when the first output attaches;
- `spawnOnce`, which records its command string in the spawned-once set;
- `additionalKeys` and `removeKeys` over `keys`; and
- a public-API test that builds a configuration with modifier-less
  `Keysym 0x1008FF13`.

Tests:

- the startup hook runs exactly once across headless detach and reattach;
- `spawnOnce` with the same command spawns once;
- `additionalKeys` replaces a duplicate identity instead of failing
  validation; and
- real-river scenario `m2-startup-keys` presses an unmodified key and sees
  its spawn.

### WP4. Floating placement

Depends on M2-D01 and M2-D02.

Deliverables:

- `planScene` removes floating windows from the stack given to the layout
  and places them as `Floating Rect`, translated from output-relative to
  global coordinates;
- clamping and normalization per spec 3.5 on workspace moves and output
  resizes;
- floats stack above tiles, families stay together, and the focused float's
  family is on top;
- initial float size through `propose_dimensions(0, 0)`, centered on the
  output when no geometry is stored;
- public `float`, `sink`, and `doFloat`; and
- `def`'s `manageHook = isDialog --> doFloat`, which supersedes M1-D06.

Tests:

- ported xmonad Floating properties;
- generated scene properties: every float is inside its output after
  clamping, and no float is passed to a layout;
- the M1-D06 trace test changes on purpose: under `Full`, refocusing the
  parent leaves the dialog shown above it; and
- real-river scenario `m2-float-dialog`.

### WP5. Pointer operations

Depends on WP4 and M2-D05.

Deliverables:

- phase-safe wrappers for `get_pointer_binding`, `op_start_pointer`,
  `op_end`, and `inform_resize_start` and `inform_resize_end`, each covered
  by `ci/check-phase-types.sh`;
- a `pointerBindings` field with mod plus button 1 to move and mod plus
  button 3 to resize, which floats a tiled window first;
- core events for `op_delta` and `op_release`, using the cumulative offset;
  and
- client `pointer_move_requested` and `pointer_resize_requested` handling
  per M2-D05.

Tests:

- a pure reducer trace for move and resize, including a release after the
  window closed;
- a resize never produces a dimension below the window's minimum hint or one;
  and
- real-river scenario `m2-pointer-drag` drags and resizes by pixels.

### WP6. Fullscreen

Depends on M2-D03.

Deliverables:

- the `fullscreen` capability advertised to every new window, replacing
  M1's empty set;
- wrappers for `fullscreen(output)`, `exit_fullscreen`, `inform_fullscreen`,
  and `inform_not_fullscreen`;
- the fullscreen set, `toggleFullscreen`, and handling of
  `fullscreen_requested` and `exit_fullscreen_requested`;
- exit and output-removal handling that proposes and positions the window in
  the same manage sequence; and
- exit on any focus change to another window on the same output (M2-D03).

Tests:

- the unsupported-request test changes on purpose: fullscreen requests now
  change state, and maximize and minimize still do not;
- removing the fullscreen window's output places it where its workspace went
  in one sequence: fullscreen on the output that now shows the workspace, or
  hidden with no request when the workspace is hidden. River has already
  exited fullscreen, so seki sends no `exit_fullscreen`;
- a hidden workspace's fullscreen window covers no output: it is hidden, and
  a workspace switch sends no fullscreen request. Showing the workspace on
  another output sends `fullscreen` alone, and a window that leaves the set
  while hidden exits fullscreen at once; and
- real-river scenario `m2-fullscreen`.

### WP7. Crop and Tolerate

Depends on M2-D04.

Deliverables:

- `sizePolicy` in `LayoutClass`, `tolerant`, and `ModifiedLayout`;
- `Crop` sends `set_content_clip_box` so content plus border stays inside the
  tile, including while a proposal is pending; and
- `Tolerate` places the actual size at the tile's top-left and honors the
  minimum and maximum hints when proposing.

Tests:

- generated reconcile traces: under `Crop` no content pixel leaves its tile
  for any commit size;
- the M1 accepted-oversized-commit test changes on purpose; and
- real-river scenario `m2-size-policy` with an oversized probe window under
  both policies.

### WP8. Restart and restoration

Depends on WP4, WP6, and M2-D06.

Deliverables:

- `Read` and `Show` for `Tall`, `Full`, `Choose`, `Mirror`, and the
  `ModifiedLayout` from WP7, plus a `Read` superclass for `LayoutClass`;
- `RestoreState` with a version number, written atomically under
  `$XDG_RUNTIME_DIR/seki/`;
- a `restart` action that checks the target is an executable file, sends
  `stop`, waits for `finished`, writes the state, closes the connection, and
  calls `executeFile` on the executable path recorded at startup, passing
  `--restore <file>` (M2-D06);
- reconnection and restoration in the same process when `executeFile`
  fails;
- `seki --restore <file>` and restoration on startup; and
- a `Mod-q` default binding.

Tests:

- `RestoreState` round-trips through `show` and `read` for generated states;
- a restore drops entries whose windows are gone and places new windows
  normally;
- an unknown state version starts fresh and logs why;
- a missing or non-executable target refuses the restart before `stop`, and a
  failing `executeFile` reconnects and restores in-process; and
- real-river scenario `m2-restart` restarts with floats, a non-default layout,
  and windows on two workspaces, then compares pixels before and after.

### WP9. Reference configuration port

Deliverables:

- `test-public/PortedConfig.hs`, a port of the reference configuration that
  compiles against the public API only; and
- the integration suite runs it as `m2-reference-config`.

Tests:

- the port compiles with `-Werror`;
- its `manageHook` shifts an `Emacs` app_id window to `emacs`; and
- its startup hook spawns each command once across a restart.

### WP10. Latency evidence and closeout

Deliverables:

- rerun the latency harness with floats, a fullscreen window, and the
  `Crop` clip box in the workload;
- update the spec milestone table, README, `wire-protocol.md`,
  `reconciliation.md`, and `integration-testing.md`; and
- the coverage ledger in this plan.

Tests:

- the M1 budgets still hold;
- all M1 and M2 scenarios pass in the pinned release command.

## 5. Dependency and Merge Order

1. WP1 harness extensions.
2. WP2 ManageHook and WP3 startup hook and keys, in either order.
3. WP4 floating.
4. WP5 pointer operations, WP6 fullscreen, and WP7 size policy, in any order.
5. WP8 restart and restoration.
6. WP9 reference configuration port.
7. WP10 latency and closeout.

WP1 lands first because every later package adds a real-river scenario.
WP4 lands before WP5 because a drag needs a float layer. WP8 lands after
WP4, WP6, and WP7 because it serializes the state they add.

## 6. Exit Criteria

M2 is complete only when all of the following are true:

- every design issue in section 9 is **Verified** with linked evidence;
- the ManageHook and Floating property suites pass at 1000 QuickCheck cases;
- the reference configuration port compiles against the public API and
  passes `m2-reference-config`;
- dialogs float by default and the M1-D06 test asserts the new behavior;
- mod plus drag moves and resizes a float on real river;
- a client fullscreen request covers the output, and exit restores the tile
  in one sequence;
- under `Crop` no content leaves its tile, and under `Tolerate` the window
  keeps its committed size;
- a restart on real river preserves workspaces, stack order, focus, float
  geometry, and layout state for every surviving window;
- all 14 M1 scenarios and every M2 scenario pass;
- the latency report meets the M1 budgets with the M2 workload; and
- `cabal build all`, `cabal test all`, `fourmolu --mode check`,
  `ci/check-package.sh`, `ci/check-phase-types.sh`, and the pinned release
  integration command pass.

## 7. Risks and Early Spikes

| Risk | Early check | Mitigation |
|---|---|---|
| River does not keep version 4 identifiers across a WM reconnect | A WP8 spike before any restore code: stop seki, start a second instance, compare identifiers on real river | Fall back to version 3 placement and record the gap in spec 3.7 |
| `executeFile` from a non-threaded runtime leaks the Wayland socket or child reapers | The same spike execs a trivial binary and checks open fds | Close the connection explicitly before exec and set close-on-exec on every fd |
| A rebuilt binary crashes at startup and leaves river unmanaged | WP8 scenario restarts into a binary that exits immediately | Document the river init loop (M2-D06); a precheck refuses a missing target |
| `set_content_clip_box` does not cover the pending-proposal frame | A WP7 mock trace with a delayed `dimensions` event | Send the clip box with every render position, not only after settlement |
| Fullscreen exit conflicts with M1's one-proposal-in-flight rule | A WP6 mock trace that exits fullscreen while a proposal is pending | Resolve in M2-D03 before WP6 |
| Probe changes destabilize the M1 scenarios | WP1 runs the 14 scenarios alone and together | Keep the old probe defaults and add requests behind arguments |
| Holding a media key no longer repeats, unlike xmonad on X11 | Note it in the reference port | Binding repeat is in M4 onwards; pull it into M3 if daily use shows the gap |

## 8. Planning Decisions

1. **Use the maintainer's xmonad.hs as the acceptance oracle.** The spec's
   criterion "the main parts of my own xmonad.hs can be ported" becomes a
   compiled test file instead of a judgment call.
2. **Split the spec's M2 row.** `inputRules`, recompile, submaps, binding
   repeat, and focus-follows-mouse move out, because the reference
   configuration does not need them and two of them need a wake source no
   other M2 feature uses.
3. **Keep floating, restart, and Crop/Tolerate in M2.** Tiled dialogs,
   losing state on every configuration change, and overflowing clients are
   the first problems daily use hits.
4. **Keep new state beside the StackSet.** The fullscreen set, pointer
   operation, and spawned-once set live in `CoreState`, so the ported
   StackSet properties stay as they are.
5. **Restart into a prebuilt binary.** M2 adds no build orchestration. M3's
   recompile workflow reuses the M2 restart path.

## 9. Open Design Issues

Status lifecycle and rules follow [M1 section 9](m1-plan.md#9-open-design-issues).

| ID | Issue | Priority | Status | Affected work |
|---|---|---|---|---|
| M2-D01 | Float geometry and the border | P1 | Verified | WP4, WP5 |
| M2-D02 | Float stacking against families | P2 | Verified | WP4 |
| M2-D03 | Fullscreen representation and exit | P1 | Verified | WP6, WP8 |
| M2-D04 | Crop while a proposal is pending | P1 | Decided | WP7 |
| M2-D05 | Client move and resize requests | P3 | Decided | WP5 |
| M2-D06 | Restore state contents and output mapping | P1 | Decided | WP8 |

### M2-D01. Float geometry and the border

- **Problem:** M1-D07 makes a tile include the border and insets content.
  Spec 3.5 stores `FloatGeom = Rect` without saying whether it includes the
  border. The StackSet comment says global coordinates, while spec 3.5 says
  output-relative.
- **Proposed resolution:** a float `Rect` is the content box in
  output-relative logical coordinates, and the border is drawn outside it.
  A drag moves the content box. Fix the StackSet comment.
- **Decision (2026-10-18):** adopt the proposed resolution. Proposals,
  committed dimensions, size hints, `set_position`, and `op_delta` all address
  the content box, so storing content needs no conversion, and a border-width
  change does not move content. Tiles include the border only because layouts
  partition space. Rejected: the outer box including the border, which converts
  by twice the border width at every request; and xmonad's `RationalRect`,
  which rescales floats on a differently sized output and reverses spec 3.5.
- **Affected text:** spec 3.5, and the `FloatGeom` comment in
  `src-core/Seki/Internal/Core/StackSet.hs`.
- **Required regression:** a scene test that a float's content rectangle
  equals its stored geometry plus the output origin.
- **Verified (WP4):** "M2-D01: a float's content rectangle is its stored
  geometry plus the output origin" in
  [SceneTest](../test/Seki/Internal/Core/SceneTest.hs). The `FloatGeom`
  comment in `StackSet.hs` and spec 3.5 now describe the output-relative
  content box. A float with no stored box is `ClientSized`, proposed 0 × 0
  and centered at its committed size; the committed size never becomes
  stored geometry.

### M2-D02. Float stacking against families

- **Problem:** M1-D03 raises the focused window's family. It does not say
  where floats sit when a tiled parent has a floating child, or which float
  is on top when focus moves to a tile.
- **Proposed resolution:** all tiled families first, then floating families
  in a remembered raise order, with the focused float's family last. A
  floating child of a tiled parent stacks with the floats.
- **Decision (2026-10-18):** adopt the proposed resolution. `CoreState`
  keeps the float raise order beside the StackSet. Focusing or clicking a
  float moves its family to the top of that order; focusing a tile leaves it
  unchanged. A focused tile never rises above a float, as in xmonad. Parent
  adjacency from M1-D03 holds within each tier only. WP8 serializes the raise
  order. Rejected: StackSet order with only the focused float raised, which
  reshuffles overlapping floats whenever focus leaves them; and M1-D03
  unchanged, which lets a focused tile cover floats.
- **Affected text:** spec 3.1 stacking order, and plan sections 3.2 and
  M2-D06.
- **Required regression:** scene tests for a floating dialog over its tiled
  parent and for focus moving to a tile while two floats overlap.
- **Verified (WP4):** "M2-D02: a floating dialog stacks over its tiled parent,
  focused or not" and "M2-D02: focus moving to a tile leaves two overlapping
  floats in their order" in
  [SceneTest](../test/Seki/Internal/Core/SceneTest.hs), the raise-order
  reducer tests and generated property in
  [EventTest](../test/Seki/Internal/Core/EventTest.hs), the M1-D06 trace in
  [ReconcileTraceTest](../test/Seki/Run/ReconcileTraceTest.hs), and the
  real-river scenario `m2-float-dialog`. The raise order is
  `coreRaiseOrder :: [WindowId]`, bottom to top. Floating families follow
  the raise order of their most recently raised member.

### M2-D03. Fullscreen representation and exit

- **Problem:** the StackSet has no fullscreen field. Exit needs a proposal in
  the same sequence, while M1 keeps one proposal in flight per window.
- **Evidence:** river's `fullscreen` request makes the compositor own the
  window's position and size, hides its border, and answers with a
  `dimensions` event. After `exit_fullscreen` the position and size are
  undefined until a manage sequence sends `propose_dimensions` and
  `set_position`, which river asks for in the same sequence as the exit.
- **Proposed resolution:** a fullscreen set in `CoreState` keyed by window.
  A fullscreen window on a visible workspace is placed as `Fullscreen` on that
  workspace's output and stacked on top. Exit supersedes the pending proposal
  for that window. Fullscreen is serialized by WP8.
- **Decision (2026-10-18):**
  - Representation: river's native fullscreen with a fullscreen set in
    `CoreState`. The window keeps its StackSet position, its tile slot, and
    its float state. When its workspace moves to another output, seki sends
    `fullscreen` for the new output. While its workspace is hidden the window
    stays in the set and is hidden like any other window.
  - Stacking: a fullscreen window is placed as `Fullscreen` and stacks above
    both tiers from M2-D02.
  - Focus: any StackSet focus change from a fullscreen window to another window
    on the same output exits fullscreen in the same manage sequence. This
    includes a key binding, a click, and a new window that the ManageHook
    inserts and focuses there. Switching workspaces keeps the window in the
    set.
  - Refinement (WP6, 2026-10-20): the focus rule is an invariant that
    `syncFullscreen` restores after every core event. A window stays in the
    set only while it is managed and is its own workspace's focus, on a shown
    or hidden workspace. A fullscreen request from a window that is not its
    workspace's focus is dropped, so a client request never steals focus.
    The output hint in `fullscreen_requested` is ignored: the window goes
    fullscreen on its own workspace's output, since honoring the hint would
    move the window to another workspace.
  - Hidden workspace: the window stays fullscreen in river on its last
    output and its client stays informed, so a workspace switch sends no
    fullscreen request. It is hidden, so it covers no output and the
    workspace shown there stays visible. Showing its workspace on the same
    output sends nothing fullscreen-related, and on another output
    `fullscreen` alone. A window that leaves the set while hidden gets
    `exit_fullscreen` and `inform_not_fullscreen` at once, so river and the
    client never hold a fullscreen state the core has dropped.
  - Output removal: river exits fullscreen itself, as if `exit_fullscreen`
    were made in the manage sequence that reports the removal. In that
    sequence seki sends `fullscreen` for the output that now shows the
    window's workspace. When the workspace becomes hidden seki sends
    nothing, since the client still believes it is fullscreen, and showing
    the workspace later sends `fullscreen` alone. A window that leaves the
    set before then gets `inform_not_fullscreen` alone.
  - Proposals: entering fullscreen drops the window's proposal state, and
    `dimensions` events while fullscreen settle nothing. Exit sends a new
    proposal for the window's tile or float in the same sequence, regardless
    of any earlier pending proposal. `set_position` is rendering state, which
    river applies at the next `render_finish` whichever sequence carries it.
    Seki sends it in the render sequence once the client commits the proposed
    size, as it does for every tile.
  - Rejected: xmonad's `doFullFloat` and `sink`, which returns a previously
    floating window to the tiled layer and draws the border itself; leaving
    the fullscreen window above a newly focused window, which sends keyboard
    input to a covered window; and drawing the focused window above the
    fullscreen window.
- **Affected text:** spec 4.1 fullscreen row and 4.2 request policy; plan
  sections 3.2 and WP6.
- **Required regression:** a mock trace that exits fullscreen with a
  proposal pending and settles to the tile, and a reducer test that a focus
  change on the same output exits fullscreen while a workspace switch does
  not.
- **Verified (WP6):** "M2-D03: exiting fullscreen with a proposal pending
  proposes the tile again and settles to it" in
  [ReconcileTraceTest](../test/Seki/Run/ReconcileTraceTest.hs), and "M2-D03:
  a focus change on the same output exits fullscreen while a workspace switch
  does not" in [EventTest](../test/Seki/Internal/Core/EventTest.hs). The
  generated property "the fullscreen set holds only managed windows that
  focus their own workspace" checks the invariant. "M2-D03: a fullscreen
  tile keeps its slot and covers its workspace's output above both tiers" in
  [SceneTest](../test/Seki/Internal/Core/SceneTest.hs) checks placement and
  stacking. The output-removal and hidden-workspace traces, and the
  real-river scenario `m2-fullscreen`, check the rest. The set is
  `coreFullscreen :: Set WindowId`. A hidden workspace's member is placed
  `HiddenFullscreen`, distinct from `Omitted`, so the reconciler keeps its
  fullscreen state while hidden. The reconciler records per window
  whether river has it fullscreen on an output, or dropped it with a removed
  output while the client still believes it is fullscreen.

### M2-D04. Crop while a proposal is pending

- **Problem:** M1 shows a window at its last settled size at the new
  top-left while a proposal is pending, which can overflow the tile.
- **Proposed resolution:** under `Crop` every render position also sends a
  content clip box equal to the tile's content box.
- **Decision (2026-10-18):**
  - `Crop`, the default, sends a content clip box equal to the tile's content
    box on every render, including while a proposal is pending. Content and
    border never leave the tile.
  - `Tolerate` never clips. It proposes the tile clamped to the window's
    minimum and maximum hints and shows the committed size at the tile's
    top-left, so an oversized client may overlap its neighbors. This matches
    the default placement of xmonad's `layoutHints`.
  - Under either policy a commit smaller than the tile leaves a gap, because a
    clip box cannot enlarge content. Floats and fullscreen windows take no
    size policy.
  - Rejected: clipping only after settlement, which lets the pending frame
    overflow; hiding a window while its proposal is pending, which blinks on
    every layout change; centering under `Tolerate`; and clipping oversized
    commits under `Tolerate`, which would leave it different from `Crop` only
    in its proposals.
- **Affected text:** spec 3.3 mismatch policy; `reconciliation.md` once WP7
  lands.
- **Required regression:** a generated trace asserting that the clip box is
  inside the tile at every render under `Crop`, and that `Tolerate` sends no
  clip box and keeps the committed size.

### M2-D05. Client move and resize requests

- **Problem:** spec 4.2 allows either starting an operation or ignoring the
  request.
- **Proposed resolution:** start the operation for a floating window and
  ignore the request for a tiled window.
- **Decision (2026-10-18):** adopt the proposed resolution. A client-drawn
  title bar or edge drag moves or resizes a float through the same operation
  as mod plus drag, and never floats a tiled window. Rejected: floating a
  tiled window on a title-bar drag, which a small accidental drag triggers;
  and ignoring every request, which leaves client title bars on floats inert.
- **Affected text:** spec 3.5 and the 4.2 `pointer_move/resize_requested` row.
- **Required regression:** a reducer test for both cases.

### M2-D06. Restore state contents and output mapping

- **Problem:** spec 3.7 lists tag, floating state, and stack position.
  The StackSet also holds focus, the output assignment, and per-workspace
  layout state. `OutputId` values are Wayland object IDs and change across a
  reconnect.
- **Proposed resolution:** serialize every workspace's stack by identifier,
  each workspace's layout, the current and visible assignment keyed by the
  output's `wl_output` global name, floats, the float raise order, the
  fullscreen set, and the spawned-once set. Unknown output names leave their workspaces hidden. The
  inventory stores that registry name as a `Word32`, not the connector name.
  The WP8 spike checks that it is stable across a WM reconnect; if it is not,
  bind `wl_output` version 4 and key by connector name.
- **Decision (2026-10-18):**
  - Contents: adopt the proposed resolution. The file is `Read`/`Show` text
    behind a version header, written atomically. An unknown version starts
    fresh and logs why. Against window management version 3 no window
    matches, so every window gets normal placement.
  - Restart target: seki records its executable path at startup and execs
    that path, so a build replaced in place runs on restart. `/proc/self/exe`
    is rejected because it refers to the old file after a rebuild replaces it.
  - `spawnOnce`: the spawned-once set is serialized, as in xmonad-contrib's
    `SpawnOnce`. Rejected: an `isRestart` flag, which never runs a command
    added to the configuration after the session's first start.
  - Failure: the restart action refuses before `stop` when the target is not
    an executable file. If `executeFile` fails, the same process reconnects
    and restores from the state file it wrote. A new binary that crashes at
    startup is covered by a documented river init loop that starts seki again.
    Rejected: a shipped fallback binary under a supervisor, which overlaps
    M3's configuration skeleton.
- **Affected text:** spec 3.7 and 5.2; plan WP8; README requirements once WP8
  lands.
- **Required regression:** the `m2-restart` scenario and a round-trip
  property.
