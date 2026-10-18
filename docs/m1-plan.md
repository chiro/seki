# M1 Core Implementation Plan

Status: **Complete** / Last updated: 2026-10-18

This document turns the M1 row in the specification into an ordered,
testable implementation plan. It is intentionally narrower than the complete
v1 design in `spec.md`.

The review findings are tracked in [Open design issues](#9-open-design-issues).
All seven are Verified against their required regressions, and every
[exit criterion](#6-exit-criteria) is met. M1 is complete.

The [README](../README.md#project-direction) describes the current public API.
The [wire and runtime reference](wire-protocol.md) and
[integration guide](integration-testing.md) describe the implementation through
WP10. The [latency report](latency-baseline/report.md) retains historical
latency and GC evidence. The [coverage ledger](#91-regression-coverage-and-closeout)
links the regression evidence and records the closeout checks.

## 1. Outcome

M1 delivers the first stateful tiling window manager built on the M0 wire and
runtime foundation. At the end of M1, the default `seki` process must:

1. maintain xmonad-style workspaces and window stacks across one or more river
   outputs;
2. run `Tall`, `Mirror Tall`, and `Full`, including layout cycling and Tall's
   resize/master-count messages;
3. support `greedyView`, `view`, workspace shift, focus movement, and
   `swapMaster` with xmonad-compatible semantics;
4. propose tiled dimensions in manage sequences and place, stack, show, hide,
   and border windows in render sequences;
5. use each output's layer-shell `non_exclusive_area` by default;
6. focus the StackSet's focused window and visibly distinguish focused and
   unfocused windows with compositor-drawn borders; and
7. retain a measured, documented latency budget for manage/render sequence
   work and GHC pauses.

The result should be usable for basic tiling, but it is not yet the practical
daily-use milestone.

### 1.1 M0 work reused as complete

M0 already delivered several items in the M1 row:

- `ManageM` and `RenderM` are distinct pure request plans;
- the runtime validates manage/render sequence order;
- all M1 window requests already have phase-safe wrappers, including borders,
  tiled edges, focus, show/hide, positioning, and stacking;
- window/output inventory, node acquisition, key binding objects, and the
  real-river integration harness are release-gated.

M1 extends these pieces. It must not replace the native transport, scanner, or
phase state machine.

### 1.2 Explicitly deferred to M2+

- Floating placement/rendering, public floating actions, and pointer move/resize.
  M1 tiles a window with a parent, such as a dialog, like any other window;
  see [M1-D06](#m1-d06--dialogs-are-tiled-in-m1)
- `ManageHook`
- Fullscreen policy
- Focus-follows-mouse
- `Crop` / `Tolerate` and content clip boxes
- Submaps and binding repeat
- Hot-swap, serialization, and restoration
- Input-device rules
- Layer-shell focus arbitration beyond the M1-D01 rules
- Bar IPC, `sekictl`, scratchpads, urgency, and prompt UI

M1 temporarily accepts a client's actual dimensions when they differ from the
proposed tile. M2 replaces that behavior with the configured `Crop` or
`Tolerate` policy.

M1 has one fixed focus policy: `window_interaction` focuses the interacted
window, and the scene planner raises the focused window's family to the top
of the explicit stacking order (section 3.5). `pointer_enter` / `pointer_leave` cause no core policy
transition until focus-follows-mouse is implemented in M2.

For every new window, M1 explicitly sends an empty `set_capabilities` value. It
ignores `show_window_menu_requested`, maximize/minimize/fullscreen requests,
and client-initiated pointer move/resize requests. M2 advertises `fullscreen`
only when its fullscreen policy is present. This avoids river's protocol
default, which otherwise tells clients that every capability is supported.

## 2. Reference Baseline and Compatibility Rules

### 2.1 xmonad baseline

Port semantics from **xmonad 0.18.1**, not from memory or from an evolving
branch:

- release archive:
  `https://hackage.haskell.org/package/xmonad-0.18.1/xmonad-0.18.1.tar.gz`
- SHA-256:
  `988b5f3104ac03aa078248afea7efcaed86661dfb239efd44995f58cd4f7a1f0`
- reference modules: `XMonad.StackSet`, `XMonad.Layout`
- reference properties: `tests/Properties/{StackSet,Delete,Failure,Focus,
  GreedyView,Insert,Screen,Shift,Stack,Swap,View,Workspace,Floating}.hs` and
  `tests/Properties/Layout/{Tall,Full}.hs`

Property disposition for M1 is explicit:

- port all applicable properties from StackSet, Delete, Focus, GreedyView,
  Insert, Shift, Stack, Swap, View, and Workspace;
- adapt Screen's StackSet and geometry properties, but exclude its X11
  resize/aspect-hint properties; M2 revisits them with `Tolerate`;
- replace Failure's partial `abort` / invalid-`new` checks with total
  construction and configuration validation returning typed errors; and
- port Floating's pure map-bookkeeping properties internally, while deferring
  public floating actions, scene generation, and interactive movement to M2.

The module header of
[StackSetTest](../test/Seki/Internal/Core/StackSetTest.hs) records each
replaced or excluded property, and the
[TallTest](../test/Seki/Internal/Layout/TallTest.hs) and
[FullTest](../test/Seki/Internal/Layout/FullTest.hs) headers record the layout
adaptations. Every other property is ported under its xmonad name. Of the 121
`prop_*` definitions in xmonad 0.18.1's `tests/Properties`, 108 are ported by
name and 12 are replaced or excluded. The remaining `prop_promote_reversible`
is commented out upstream and not registered there.

Record copied or closely derived BSD-3-Clause code in
`THIRD_PARTY_NOTICES.md`. Keep the pure core independent of X11 and do not add
the `xmonad` package as a runtime dependency.

### 2.2 River baseline

Vendor `river-layer-shell-v1.xml` from the same river v0.4.5 source pin as the
existing window-management and XKB protocols. Update `protocol/SOURCES.md`,
`protocol/SHA256SUMS`, package source lists, generated bindings, and scanner
tests together.

The pinned release integration environment currently runs river 0.5.0. A
source audit of releases 0.4.6 and 0.5.0 found that `river_window_manager_v1.error` defines
`unresponsive`, but river does not start a response timer while waiting for
`manage_finish` or `render_finish`. The 100 ms timer in
`river/WindowManager.zig` waits for application configure acknowledgements and
is not a WM-unresponsive threshold. Therefore M1 must use a documented seki
latency budget; it must not describe results as margin against river's 100 ms
timer.

### 2.3 Semantic deviations must be named

Every deliberate difference from xmonad must have a focused test and a note in
the module documentation. Expected M1 differences are:

- river window/output identifiers replace X11 windows and screen IDs;
- rectangles use signed global logical coordinates;
- screens carry no geometry; the planner takes usable output rectangles
  separately;
- floating state and pure bookkeeping exist, but floating scene/runtime
  behavior is deferred;
- the runtime may be temporarily headless;
- a live output can exist when there is no hidden workspace left to assign;
- visibility is an explicit protocol state derived from the active layout's
  output, rather than X11 map state;
- the focused window's family is raised, where xmonad never raises the
  focused window (M1-D03); and
- windows with a parent are tiled rather than floated (M1-D06).

## 3. Target Architecture

The M1 data path is:

```text
river events + queued Actions
              |
              v
     pure state transition
       CoreState / StackSet
              |
              v
        pure scene planner <--- layout + usable output rectangles
              |
              v
          DesiredScene
           /         \
          v           v
   manage diff     render diff
      ManageM         RenderM
          \           /
           v         v
          M0 runtime/connection
```

Protocol inventory owns live object handles and facts reported by river. The
pure core owns policy. The reconciler is the only layer that joins pure IDs to
protocol objects.

### 3.1 Implemented modules

| Module | Responsibility |
|---|---|
| `Seki.Core.Geometry` | `Point`, `Size`, `Rect`, checked construction and translation, splitting, mirroring |
| `Seki.Core.StackSet` | Workspace/screen/stack zipper and xmonad-compatible pure operations |
| `Seki.Core.Layout` | `LayoutClass`, messages, `Choose` / `(|||)`, `Mirror` |
| `Seki.Layout.Tall` | `Tall` and `tile`; its `Shrink`, `Expand`, and `IncMasterN` messages live in `Seki.Core.Layout` |
| `Seki.Layout.Full` | `Full` |
| `Seki.Internal.Core.State` | Headless/attached policy state, window manage/unmanage/focus, output hotplug |
| `Seki.Internal.Core.Action` | Pure user actions: `windows`, `sendMessage`, kill/spawn intents |
| `Seki.Internal.Core.Event` | Core events and the reducer that applies them in arrival order |
| `Seki.Internal.Core.Scene` | `DesiredScene` and pure planning |
| `Seki.Run.LayerShell` | layer-shell global binding and per-output/per-seat objects |
| `Seki.Run.Reconcile` | persistent proposal/applied state and phase-safe diffs |
| `Seki.Run.Policy` | Queues river events and key-binding actions, drains them into core state at `manage_start`, plans scenes, and runs intents and reconciliation |

The public geometry, StackSet, layout, Tall, and Full facades live in
`src-public`; their implementations and the private `Seki.Internal.Core.*`
modules live in `src-core`. `Seki.Run.*` lives in `src`.

WP7 removed `Seki.Run.Full` and `Seki.Run.Default` after migrating the
executable, integration fixture, and tests to `Seki.Run.Policy`. Inventory
tracks protocol facts without choosing focus. The policy's core-event queue
owns the ordering of callbacks and key actions.

The implemented Cabal component graph is acyclic. The private `seki-wire`
library depends on private `seki-core`, and the public `seki` library depends
on both. The public library exposes the six modules listed in
[the specification](spec.md#51-configuration--program-the-shape-of-the-public-api).
Tests address internal libraries directly. Each shared core or wire
implementation module belongs to its own private library.
`test-public/ConfigMain.hs` checks a downstream-style import, and
`ci/check-package.sh` checks the source distribution and public Haddock index.

### 3.2 Core identifiers and geometry

The pure modules must not contain `Object interface` handles. Use small
`WindowId` and `OutputId` newtypes that can be converted to/from Wayland
`ObjectId` in the runtime bridge.

Geometry rules:

- positions are signed `Int32` global logical coordinates;
- dimensions are non-negative and use wider intermediates for multiplication,
  ratio application, and addition before checked conversion;
- a zero-area `non_exclusive_area` is valid input but produces no visible tile;
- splits may yield zero-width or zero-height tiles; geometry keeps them, and
  the planner treats them as omitted (section 3.5);
- splitting assigns every input pixel exactly once, including odd dimensions;
- `Mirror` exchanges axes without losing negative origins.

### 3.3 StackSet and headless state

Keep the active StackSet zipper semantically aligned with xmonad. Represent
the no-output case with a wrapper state rather than weakening the StackSet's
non-empty current-screen invariant:

```haskell
data CoreState i l
  = HeadlessState (HeadlessWorkspaces i l)
  | AttachedState (StackSet i l) [OutputId]
    -- ^ outputs that have no workspace yet, in arrival order
```

The StackSet retains `floating :: Map WindowId FloatGeom`, with
`FloatGeom = Rect`, from the accepted data model. M1 ports the small pure
`float` / `sink` bookkeeping and ensures `delete` removes a window from this
map. These operations remain internal to the core in M1: the scene planner
always tiles StackSet windows, and the public API exposes no floating action
until M2.

The wrapper must preserve workspace layout and stacks while headless. The first
output promotes the remembered/default workspace to current; removal of the
last output demotes every workspace without losing order or focus. If an output
is added when no hidden workspace exists, track it as unassigned and emit no
scene for it until a workspace becomes available.

At all times:

- every configured workspace appears exactly once;
- every managed window appears in at most one workspace stack;
- current and visible screens have distinct outputs and workspaces;
- hidden workspaces have no output;
- the focused window, when present, is a member of the current workspace; and
- no live inventory window is lost: its creation event waits in the policy
  queue until the next `manage_start` drains it, and a window created while
  headless joins the remembered current workspace, so it is shown when the
  first output arrives.

### 3.4 Events and Actions

Inventory callbacks and key-binding presses queue small pure events in one
queue. A `manage_start` drains the queue in arrival order and applies it to
`CoreState` before planning. The M1 core events are:

- window created (after its node is acquired) and closed;
- output created and removed;
- window interaction (click-to-focus); and
- key-binding action, queued on `pressed`.

Output geometry, the layer-shell usable area, and layer-shell focus are not
core events. The StackSet holds no geometry, so the scene planner and
reconciler (WP5/WP6) read those facts from inventory. Pointer enter and leave
produce no event in M1.

The M1 `Action` surface should be stable enough for the later configuration
API:

```haskell
windows     :: (StackSet i l -> StackSet i l) -> Action i l ()
sendMessage :: Message a => a -> Action i l ()
spawn       :: String -> Action i l ()
kill        :: Action i l ()
```

`Action` records pure state changes and external intents; it cannot lift
arbitrary IO. Process launch happens asynchronously outside the active river
sequence. Completion that changes WM state re-enters through `manage_dirty`.

### 3.5 Desired scene

`DesiredScene` contains a map from window IDs to `Omitted` or `Placed Rect`,
a stable window order, proposed stacking, and the selected focus and output IDs.
`Placed` selects a positive content rectangle; the window can still await its
first dimensions. The scene does not store actual visibility, tiled flags,
decorations, borders, or a request to focus a window.

The reconciler derives those requests from the scene, style, and live inventory.
It recomputes family stacking for the subset that can actually be shown.
The following rules describe planning and its reconciliation:

- windows that the active layout places on a visible workspace receive its
  rectangles;
- windows that the active layout omits on a visible workspace are explicitly
  hidden, as xmonad unmaps them;
- hidden workspace windows are always explicitly hidden;
- incomplete outputs or zero-area usable rectangles show no windows;
- a tile with zero width or zero height is treated as omitted by the layout:
  the window is hidden and receives no dimension proposal, because a zero
  `propose_dimensions` value lets the client choose its own size;
- `Full` places only the focused window, at the full rectangle, and omits the
  rest, as in xmonad;
- stacking is deterministic and explicitly complete, built bottom to top as
  follows:
  1. parent links from inventory group the shown windows into families; a
     window whose parent is closed, unknown, or not shown starts its own
     family;
  2. families stack in the StackSet's window order, except that the focused
     window's family is on top;
  3. within a family each parent sits directly below its children; among
     siblings the branch containing the focused window is on top and the rest
     keep StackSet order, so the focused window is topmost unless it has shown
     descendants;
  4. hidden windows sit below every shown window, in StackSet order; and
  5. stacking never changes the StackSet's stack order;
- every window the layout places, including one whose first dimensions are
  pending, receives all four `set_tiled` edges; windows the layout omits keep
  their last flags, since they are hidden;
- with border width `b`, a window's content rectangle is its tile inset by
  `b` on all four sides, because river draws borders outside the content box
  (M1-D07). The proposed content and border fill the tile exactly, as in
  xmonad's `tileWindow`. A client that commits a larger size can overflow the
  tile under M1's accepted mismatch policy. A tile whose inset content has zero
  width or height, that is
  a tile no larger than `2b` in either dimension, is degenerate and follows
  the zero-sized tile rule above. `Full` keeps the border on its window;
- the scene selects at most one focus candidate, the StackSet's focused window,
  only when the layout places it. The reconciler sends window focus only after
  that selection is shown and layer-focus arbitration permits it. The StackSet
  focus itself never changes for rendering reasons; and
- the focused border is drawn on the StackSet's focused window whenever it is
  shown, even before the following manage sequence sends `focus_window`. While
  a layer surface holds keyboard focus, every window draws the unfocused
  border, so the border always shows where keystrokes go (M1-D01).

### 3.6 Proposal and render state machine

The reconciler owns state that persists across sequences:

- last applied manage attributes;
- last applied rendering attributes;
- most recent actual dimensions;
- at most one in-flight proposal per window, with its dimensions revision;
- the last requested focus per seat, its layer-focus revision, and whether
  non-exclusive focus has been reclaimed; and
- the last applied stacking order.

The runtime snapshot supplies reported layer ownership. The latest desired
rectangle remains in Policy's current scene, rather than in a per-window
queued target field.

Manage reconciliation sends, in deterministic order:

1. decoration changes and an explicit empty M1 capability set;
2. tiled-edge changes;
3. required dimension proposals;
4. focused-output layer-shell `set_default`; and
5. `focus_window` for the scene's focus target, or `clear_focus` when the
   scene has none.

Render reconciliation sends, in deterministic order:

1. positions for settled visible windows;
2. a complete bottom-to-top stacking order;
3. borders;
4. explicit show/hide state; and
5. render completion through the existing runtime.

A window is shown only when the active layout places it on a visible
workspace and its dimensions are settled. A window that the layout omits
receives no dimension proposal and keeps its last applied dimensions. Cycling
focus in `Full` therefore resizes a window only the first time `Full` selects
it, and returning from `Full` to `Tall` sends no proposal to the windows that
`Full` omitted.

A proposal is settled only after a newer dimensions revision is observed.
While a new window has no settled dimensions it remains explicitly hidden.
When the layout selects an existing window whose dimensions are not yet
settled for its new rectangle, show that selection at its last settled size
and the desired top-left. A prototype on river 0.5.0 compared this with
retaining the previous selection. Both rendered correctly with a deliberately
delayed client. Showing the new selection keeps it aligned with StackSet
focus. See [Scene reconciliation](wp6-reconciliation.md) for the experiment.
If the desired dimensions change while a proposal is in flight, retain only
the latest desired rectangle, finish the current render sequence safely, and
request another manage sequence with `manage_dirty` after sequence completion.

Focus follows the shown state across both phases. When the StackSet's focused
window is not shown, for example while its tile is degenerate or its first
dimensions are pending, the manage sequence sends `clear_focus`. When that
window becomes shown in a render sequence, the reconciler requests
`manage_dirty`, and the next manage sequence sends `focus_window`. Focus is
never sent together with a proposal, so keyboard input never reaches a hidden
window. The same path restores a window whose degenerate tile regains a
positive size, after a ratio change or a usable-area change.

Layer surfaces take keyboard focus outside the StackSet (M1-D01). River ends
every manage sequence by focusing an exclusive layer surface and ignores WM
focus requests until it reports otherwise. A non-exclusive layer surface keeps
focus only through manage sequences in which the WM sends no focus request;
any `focus_window` or `clear_focus` wins, and river then reports `focus_none`.
The reconciler therefore:

- sends no focus request while an exclusive layer surface holds focus;
- sends no focus request from routine reconciliation while a non-exclusive
  layer surface holds focus, so new windows, layout changes, and output
  changes leave keyboard input with the layer surface;
- reclaims non-exclusive focus for an explicit user focus action, meaning
  `window_interaction` or a key-binding action that applies `windows`, by
  sending the scene's focus request in that manage sequence; and
- on `focus_none`, treats its last focus request as void and, in the same
  manage sequence, sends `focus_window` for the scene's focus target, or
  `clear_focus` when there is none. River does not restore window focus
  itself, so this request is needed even when the target has not changed.

The StackSet changes exactly as it would without a layer surface. A new window
still becomes the StackSet focus; only keyboard input stays with the layer
surface until the user acts or the surface gives up focus.

For M1, a settled size mismatch keeps the desired top-left and uses the actual
client size. This behavior must be isolated behind the reconciler so M2 can add
`Crop` and `Tolerate` without changing StackSet or layout semantics.

## 4. Work Packages

Each package below is intended to be independently reviewable. A package is
done only when its tests and documentation land with it.

### WP1 — Reference pin, geometry, and source hygiene

Deliverables:

- record the xmonad 0.18.1 baseline and license attribution;
- establish the `seki-core` / `seki-wire` / public `seki` component graph with
  a minimal re-exported module, without compiling one module into multiple
  components;
- add `Seki.Core.Geometry`;
- define checked rectangle construction, split, mirror, and translation;
- document all signed-coordinate and overflow behavior.

Tests:

- build a downstream-style module that imports the re-export through the
  public library;
- run `cabal haddock all` and inspect the generated public module index;
- verify the source distribution contains every core source and generated
  documentation input exactly once;
- unit examples for negative origins, zero area, one-pixel and odd splits;
- properties for area preservation, no overlap, bounds, and mirror involution.

M1-D05 removed edge adjacency from WP1: M1's tiled flags do not depend on
geometry.

### WP2 — StackSet port and properties

Deliverables:

- port the zipper data types and the M1 operation subset;
- retain the floating map and port internal pure `float` / `sink` bookkeeping,
  without exposing floating placement through the M1 public API;
- add construction/validation for unique non-empty workspace tags;
- add headless attach/detach and output hotplug transitions.

Required operation subset:

- observation: `peek`, `index`, `integrate`, `integrate'`, `differentiate`,
  `currentTag`, `member`, `findTag`, `lookupWorkspace`, `screens`,
  `workspaces`, `allWindows`;
- focus: `focusUp`, `focusDown`, `focusMaster`, `focusWindow`;
- ordering: `swapUp`, `swapDown`, `swapMaster`, `shiftMaster`;
- membership: `insertUp`, `delete`, `filter`, `shift`, `shiftWin`;
- workspaces: `view`, `greedyView`, `mapWorkspace`, `mapLayout`;
- internal floating bookkeeping: `float`, `sink`.

Port all applicable xmonad 0.18.1 properties in the referenced StackSet,
Delete, Focus, GreedyView, Insert, Shift, Stack, Swap, View, Workspace, and
Floating suites. Adapt Screen's StackSet/geometry properties. Replace Failure's
partial-construction assertions with typed-error properties. X11 resize and
aspect-hint properties are intentionally excluded until M2.

### WP3 — Layout algebra and standard layouts

Deliverables:

- `LayoutClass`, existential messages, `Choose`, `(|||)`, and `Mirror`;
- `Tall`, `Full`, `Shrink`, `Expand`, `IncMasterN`, `NextLayout`, and
  `FirstLayout`;
- per-workspace layout state updates.

Tests:

- port applicable xmonad Tall/Full properties, including that `Full` returns
  only the focused window at the full rectangle;
- prove tiles remain within the supplied rectangle and do not overlap;
- cover `Tall` ratios 0 and 1, zero masters, more masters than windows, odd
  dimensions, negative origins, Mirror, nested choices, and layout wraparound;
- cover ratios that floor to a zero-width column and splits of a one-pixel
  rectangle, through Mirror;
- golden examples for the default `Tall 1 (3/100) (1/2)`.

### WP4 — Persistent core policy and Action execution

Deliverables:

- introduce runtime-owned `CoreState` and an ordered event/action queue;
- translate inventory deltas into pure events at sequence boundaries;
- implement default actions for layout cycling, Tall shrink/expand and master
  count, focus up/down, swap master, workspace view/shift, terminal spawn, and
  close;
- keep process spawning off the event-loop sequence path.

Tests:

- reducer unit tests and invariant properties over generated event sequences;
- headless startup, first output, two-output add/remove, last-output removal,
  window create/close, and interaction focus;
- pointer-enter/leave events leave M1 core policy unchanged;
- action ordering when multiple bindings arrive before one `manage_start`.

### WP5 — Layer-shell protocol and usable rectangles

Deliverables:

- vendor and generate `RiverLayerShellV1`;
- vendor river v0.4.5's pinned upstream
  `wlr-layer-shell-unstable-v1.xml` as an integration-only protocol and
  generate its fixture binding;
- bind its global as a required runtime capability;
- create and destroy one layer-shell output object per river output;
- track the latest `non_exclusive_area` as each output's usable core `Rect`;
- create the seat object needed to track whether a layer surface owns focus;
- track river's keyboard owner from `focus_exclusive`, `focus_non_exclusive`,
  and `focus_none` per seat as `RiverLayerFocus` in the runtime snapshot. The
  M1-D01 focus and border rules belong to WP6, because the reconciler
  generates focus requests and borders;
- send `set_default` for the current screen's output in every manage sequence
  while the core state is attached. Sending it every sequence is deliberate:
  the request is idempotent, and river forgets the default when that output's
  layer-shell object is destroyed;
- add a `seki-layer-surface-fixture` Haskell executable using the existing
  native Wayland transport. It binds `wl_compositor`, `wl_shm`, and
  `zwlr_layer_shell_v1`, maps one top-anchored shared-memory surface with a
  fixed exclusive zone, reports readiness to the harness, and tears down
  cleanly. It must not add a `libwayland` dependency to seki or the fixture.

Tests:

- scanner parse/generation coverage for cross-protocol object references;
- global missing/version/bind failures;
- output/seat child lifetime, duplicate prevention, removal cleanup;
- mock event sequence proving a usable-area update affects the next plan;
- fixture socket-pair coverage for registry binding, configure/ack, buffer
  attachment, readiness, and teardown;
- zero-area and negative-origin usable rectangles.

### WP6 — Scene planner and reconciler

Deliverables:

- pure `DesiredScene` generation;
- deterministic manage/render diff compilation;
- persistent proposal state and dimensions revisions;
- full stacking establishment, visibility invariant, tiled edges, focus, SSD
  policy, explicit empty M1 capabilities, and borders;
- post-render `manage_dirty` scheduling for superseded proposals and for a
  focus target that becomes shown.

Tests:

- scene properties for window uniqueness, layout selection, at most one
  selected focus target, positive inset content inside each tile, and
  deterministic stacking; request-level checks for all four tiled edges,
  shown-state focus, and no degenerate tile shown or proposed;
- request-level tests for focus recovery: when a degenerate tile regains a
  positive size and when a spawned window's first dimensions settle, `show`
  arrives in a render sequence with the focused border, then `focus_window`
  in the next manage sequence, with `clear_focus` until then;
- request-level tests for layer focus: an unchanged scene and a newly mapped
  window send no focus request during non-exclusive focus; a window
  interaction or a `windows` key binding sends `focus_window`, followed by
  `focus_none` handling; exclusive focus suppresses every focus request;
  `focus_none` re-sends `focus_window` even for an unchanged target; and every
  window draws the unfocused border while a layer surface holds focus;
- an interaction-focused window's family becomes the top of the stacking
  order, with the focused window topmost unless it has shown descendants,
  without disturbing its workspace's stack order;
- request-level mock tests for unchanged scenes, focus-only changes, workspace
  hiding, layout changes, delayed dimensions, superseded proposals, closed
  pending windows, and output geometry changes;
- request-level tests proving that layout-omitted windows receive `hide` and
  no dimension proposal, and that Tall → Full → Tall sends no proposal to the
  windows `Full` omitted;
- request-level tests proving every new window receives empty capabilities and
  unsupported client requests do not mutate core state;
- compile-surface tests that manage-only operations cannot appear in
  `RenderM` and render-only operations cannot appear in `ManageM`.

### WP7 — Default runtime and minimal public API

Implemented by the canonical pure configuration in `Seki.Internal.Config`,
the boundary adapter in `Seki.Run.Config`, and the public `Seki` front door.
The executable and release integration suite both use `Seki.Run.Policy`.
The former M0 Full/Default policies, inventory focus selection, and runtime
deferred-action queue are removed. Protocol callbacks and key actions enter
the policy's one core-event queue.

`seki-core` owns configuration and actions. `seki-wire` depends on core and
validates the lowered configuration before connecting. The public library
depends on both components; user code imports no wire types.

The public compile fixture covers a custom layout and custom bindings. A
layout type change also replaces `keys`, for example
`def{layoutHook = Full, keys = defaultKeys}`.

Deliverables:

- replace the executable's M0 Full policy with the M1 policy;
- provide nine default workspaces and
  `Tall ||| Mirror Tall ||| Full`;
- add default keys for next layout, focus up/down, swap master, workspace
  greedy-view/shift, terminal, and close;
- add `Mod-h` / `Mod-l` for `Shrink` / `Expand` and `Mod-comma` /
  `Mod-period` for `IncMasterN 1` / `IncMasterN (-1)`;
- introduce the M1-stable slice of `Config`, `Action`, `BorderConfig`, `def`,
  and `seki` through the `Seki` front door;
- validate configuration before connecting to Wayland.

Do not publish placeholder M2 fields merely to mimic the final record shown in
`spec.md`. Add later fields with their real semantics. In particular, M1's
`Config` does not expose `focusModel`; M2 adds it with focus-follows-mouse.

Tests:

- default configuration values and binding-conflict validation;
- basic user `Main.hs` compile test importing only `Seki`;
- default binding tests for every Tall message;
- regression coverage for the existing terminal and close defaults.

### WP8 — Real-river scenarios

Implemented by the ten `m1-*` entries in the exact release manifest: the nine
scenarios below and `m1-layer-focus`, which M1-D01 requires. The native
Haskell fixture checks actual compositor pixels and keyboard delivery. See
`docs/integration-testing.md` for the oracle, fixture limits, and output lifecycle
coverage.

Extend the pinned release suite with deterministic scenarios for:

1. three windows in Tall, including observable geometry;
2. Tall shrink/expand and master-count changes, with observable geometry;
3. focus movement plus focused/unfocused border changes;
4. swap master;
5. Tall → Mirror Tall → Full layout cycling;
6. hiding/showing workspaces;
7. `greedyView` swapping workspaces across two outputs;
8. the WP5 native-Haskell layer-surface fixture changing
   `non_exclusive_area`; and
9. output removal/re-add without losing workspace/window state.

The fixture is the only exclusive-zone client used by this scenario. The
release gate must not depend on a desktop bar's network, DBus, or host
configuration.

The existing native Wayland, Xwayland, and fcitx5/Mozc scenarios remain in the
manifest and must continue to pass.

### WP9 — Latency and GC evidence

Deliverables:

- monotonic timestamps around `manage_start` → `manage_finish` submission and
  `render_start` → `render_finish` submission;
- an opt-in stress harness with fixed seed/workload;
- RTS statistics including maximum GC pause;
- a report under `docs/` recording machine, compiler, RTS options, workload,
  sample count, p50/p95/p99/max sequence latency, allocation, residency, and
  maximum GC pause;
- comparison of the normal and nonmoving collectors before choosing runtime
  defaults.

Preliminary engineering budget: under the fixed reference workload, p99 pure
step+plan time should be below 2 ms, p99 complete sequence handling below
10 ms, and maximum observed GC pause below 10 ms. These are seki budgets, not
river guarantees. The first reproducible baseline may revise the numbers, but
any revision must be explained in the report. If a future compositor documents
an enforced timeout `T`, the adopted complete-sequence budget must be at most
`T / 10`, and the measured p99 must meet that budget, before compatibility is
described as having a 10x margin.

Performance measurements are a manual/release evidence gate, not a flaky
wall-clock assertion in ordinary unit CI. Pure allocation/regression checks may
still run in CI.

Implementation and reproduction are documented in [latency.md](latency.md).
The opt-in `seki-latency` executable and `tools/latency/run.py` retain raw
measurements and generate the repeated-trial report; collector selection remains
an evidence gate.

### WP10 — Documentation and M1 closeout

Deliverables:

- update `README.md`, `docs/spec.md`, and `docs/wire-protocol.md` to distinguish
  the M1 public/runtime API from the final v1 target;
- document default keys and a minimal basic configuration;
- update the integration manifest and maintenance guide;
- run formatting, lint, unit tests, source-distribution checks, and the pinned
  real-river release gate;
- change the milestone table only after every exit criterion below is met.

## 5. Dependency and Merge Order

Resolve each [open design issue](#9-open-design-issues) to **Decided** before
implementing its affected work packages.

Recommended merge sequence:

1. WP1 component/re-export spike, geometry, and reference pin; WP5's protocol
   vendoring/generation may proceed independently
2. WP2 StackSet
3. WP3 layouts and WP5 runtime/fixture integration
4. WP4 persistent policy/Actions
5. WP6 scene/reconciler
6. WP7 default runtime/public API
7. WP8 real-river coverage
8. WP9 performance evidence
9. WP10 documentation/closeout

WP2 and WP3 should land before runtime refactoring so semantic failures are
debugged in pure tests. WP5 protocol vendoring and scanner work do not depend
on WP1; only its usable-rectangle plumbing depends on WP1 geometry. WP5 must
land before the M1 runtime is declared usable because binding layer shell
changes whether river permits layer clients to remain mapped.

## 6. Exit Criteria

M1 is complete only when all of the following are true:

- all design issues in section 9 are **Verified**, with implementation and
  regression-test evidence linked from their entries;
- all properties in the explicit xmonad suite disposition pass or have their
  documented typed-error/M2 replacement;
- StackSet/event-sequence invariant tests pass at the configured high
  QuickCheck count;
- Tall, Mirror Tall, Full, and choice-message properties pass;
- mock protocol tests cover delayed dimensions and prove every sequence is
  completed or returns a structured runtime error;
- windows on hidden workspaces and windows omitted by the active layout
  receive `hide`, and only windows the layout places with settled dimensions
  receive `show`;
- focus, borders, complete stacking, and tiled edges are observable in tests,
  and proposed content plus borders stays inside each tile. Actual geometry
  stays inside for size-conforming clients; an oversized commit may overflow
  under M1's accepted mismatch policy. Configurable mismatch handling remains
  deferred to M2;
- keyboard focus never targets a hidden window, no degenerate tile is
  proposed or shown, and focus returns through the post-render
  `manage_dirty` path;
- a window click or focus key binding takes keyboard input back from a
  non-exclusive layer surface on real river, routine reconciliation does not,
  and window focus returns after `focus_none`;
- every new M1 window receives empty capabilities and unsupported client
  requests leave policy state unchanged;
- Tall resize and master-count messages work through the default bindings;
- layer-shell binding and `non_exclusive_area` affect real placement;
- one- and two-output real-river tiling scenarios pass;
- existing Xwayland and Japanese-input release scenarios remain green;
- the latency/GC report meets the adopted seki budgets;
- `cabal build all`, `cabal test all`, formatting/lint, sdist validation, and
  the pinned release integration command pass; and
- public docs no longer describe the executable as Full-only.

## 7. Risks and Early Spikes

| Risk | Early check | Mitigation |
|---|---|---|
| No protocol correlation ID for dimension proposals | WP6 mock spike with delayed/superseded proposals | One in-flight proposal per window plus dimensions revisions and the latest target in Policy's scene |
| Headless/hotplug state distorts StackSet invariants | WP2 generated attach/detach sequences | Sum type around an unchanged active StackSet zipper |
| Layer-shell child creation does not fit current handler composition | WP5 socket-pair prototype before policy integration | Give runtime explicit layer-shell object ownership and cleanup |
| Private-sublibrary re-exports build but produce broken public docs or sdist contents | WP1 downstream import, Haddock, and sdist spike | Fix the component/source-dir topology before porting core modules; never compile one module into multiple components |
| Scene and inventory diverge on close/remove during pending work | WP6 generated reducer/reconciler traces | Key every state table by pure ID and prune atomically at sequence boundaries |
| Full stacking requests become quadratic | Planner/request-count property at 100/500 windows | Emit one bottom anchor and an ordered chain; no all-pairs constraints |
| Public API hardens temporary internals | Compile a minimal config only after WP6 types settle | Export actions/configuration, not inventory or reconciliation state |
| Performance evidence is host-noisy | Fixed seed, workload, build, and repeated samples | Report distributions; keep wall-clock gating out of unit CI |

## 8. Planning Decisions

M1 adopted the following choices:

1. **Include a minimal public configuration/Action slice in M1.** Otherwise the
   default binary can demonstrate tiling but users cannot exercise the core in
   the intended configuration-as-program model.
2. **Wrap an xmonad-compatible active StackSet for headless operation.** Do not
   change `current` to `Maybe`, which would invalidate much of the reference
   semantics and property suite.
3. **Treat layer shell as required.** Binding it is what tells river to keep
   layer surfaces alive; silently continuing without it produces a misleadingly
   running but incomplete WM.
4. **Use xmonad 0.18.1 as the semantic source pin.** This matches the installed
   development package and provides a stable property corpus.
5. **Adopt seki-owned latency budgets.** River 0.5.0 has no enforceable WM
   response timeout to measure margin against.
6. **Retain floating state in the M1 StackSet.** Pure `float` / `sink` map
   bookkeeping is ported and tested internally, while public/runtime floating
   behavior remains M2 work.
7. **Use an intentionally narrow M1 interaction policy.** M1 advertises no
   optional window capabilities, ignores unsupported client requests, and uses
   click-to-focus with the focused window's family raised. Fullscreen and
   focus-follows-mouse are enabled only when M2 implements them.
8. **Own the exclusive-zone integration fixture.** A deterministic Haskell
   fixture uses seki's native Wayland transport and a pinned integration-only
   wlr-layer-shell binding; it does not link `libwayland` or depend on Waybar.

## 9. Open Design Issues

These entries track the spec/plan review findings. Keep their IDs stable and
retain resolved entries as a short decision history.

Status progression:

- **Open**: the problem is recorded; the proposed resolution is not yet adopted.
- **Decided**: the chosen behavior is recorded and the affected spec sections,
  plan rules, and acceptance criteria have been updated consistently.
- **Verified**: implementation and the required regression tests have landed;
  link the supporting commits or PRs in the entry.

The design review is complete when all seven entries are **Decided** or
**Verified**. M1 completion requires every entry to be **Verified**. Proposed
resolutions below remain as decision history. The coverage ledger links the
regression evidence for each entry.

| ID | Issue | Priority | Status | Affected work |
|---|---|---|---|---|
| M1-D01 | Reclaiming non-exclusive layer focus | P1 | Verified | WP5, WP6 |
| M1-D02 | Full visibility and layout-omitted windows | P2 | Verified | WP3, WP6 |
| M1-D03 | Parent/child stacking versus focus raising | P2 | Verified | WP6 |
| M1-D04 | Zero-sized tiles and their focus behavior | P2 | Verified | WP1, WP3, WP6 |
| M1-D05 | Tiled flags at output boundaries | P2 | Verified | WP1, WP6 |
| M1-D06 | Dialogs are tiled in M1 | P3 | Verified | WP6 |
| M1-D07 | Borders outside the tile | P2 | Verified | WP6 |

### M1-D01 — Reclaiming non-exclusive layer focus

- **Problem:** WP5 suppresses window focus requests while any layer surface has
  accepted focus, restoring focus only after `focus_none`. For non-exclusive
  layer focus, a WM request to move focus away is itself a trigger for
  `focus_none`; this policy can leave keyboard input with the layer surface
  after the user clicks an application.
- **Evidence:** river 0.5.0's `Seat.manageFinish` focuses an exclusive layer
  surface at the end of every manage sequence. For non-exclusive focus it
  focuses the layer surface only when the WM made no focus request in that
  sequence; otherwise the WM's request wins and river schedules `focus_none`.
  This applies even to a `focus_window` for the already focused window. River
  grants non-exclusive focus when an on-demand layer surface maps or is
  clicked, and never restores window focus itself. River 0.4.6 has the same
  `manageFinish` logic. A second defect follows from the diff-based
  reconciler: after `focus_none` an unchanged focus target would send no
  request and leave no window focused.
- **Decision:** only explicit user focus actions reclaim non-exclusive focus.
  These are `window_interaction` and key-binding actions that apply
  `windows`. Routine reconciliation, including a newly mapped window, sends no
  focus request while any layer surface holds focus, and nothing is sent
  during exclusive focus. `focus_none` voids the last focus request, and the
  same manage sequence sends the scene's focus request. While a layer surface
  holds keyboard focus every window draws the unfocused border. The StackSet
  is unaffected throughout.
- **Affected text:** plan sections 1.2, 3.4–3.6, WP5/WP6, and exit criteria;
  `spec.md` sections 3.6 and 4.5.
- **Required regression:** accept non-exclusive layer focus, then click a
  window or invoke a keyboard focus action. Verify the focus request and border
  transition, including the subsequent `focus_none`. Also verify that an
  unchanged scene preserves non-exclusive focus and exclusive focus remains
  protected.

### M1-D02 — Full visibility and layout-omitted windows

- **Problem:** section 3.5 places every Full window at the full rectangle,
  whereas xmonad's Full displays only the focused window. With M1's accepted
  size mismatches, a smaller focused window exposes other applications beneath
  it. The spec's workspace-based shown-state invariant also fails to express
  windows omitted by a layout.
- **Proposed resolution:** preserve xmonad Full semantics and explicitly hide
  windows omitted by the active layout. Define shown-state eligibility using
  layout output as well as workspace visibility and dimension readiness.
- **Affected text:** plan sections 2.3, 3.5–3.6, WP3/WP6, and exit criteria;
  `spec.md` sections 3.1, 3.3, and 3.4.
- **Required regression:** switch a three-window workspace to Full with a
  focused client that commits a smaller size. Verify that only the selected
  window is shown, focus cycling hides the previous selection, and returning
  to Tall restores eligible windows. Cover a custom layout omitting a window.
- **Decision (2026-10-16):** preserve xmonad Full semantics. `Full` returns only
  the focused window. The active layout's output, workspace visibility, and
  settled dimensions together define the shown state. Layout-omitted windows
  are hidden and keep their last applied dimensions, with no new proposal.
  Rejected: showing every Full window at the full rectangle, which makes
  focus cycling a restack but lets a smaller or transparent focused window
  expose the windows beneath it and keeps covered windows rendering. The
  WP6 real-river prototype selected display at the new selection's last
  settled size during an unsettled change (section 3.6). Updated: plan
  sections 2.3, 3.5, 3.6, WP3, WP6,
  and exit criteria; `spec.md` sections 3.1 and 3.4. Section 3.3 already
  allows a provisional or hidden placement and needed no change.
- **Additional regression:** Tall → Full → Tall sends no dimension proposal to
  the windows `Full` omitted.

### M1-D03 — Parent/child stacking versus focus raising

- **Problem:** section 3.5 requires children above parents, while section 1.2
  and WP6 require the focused window to become the top node. Both cannot hold
  when a parent with a visible child receives focus.
- **Proposed resolution:** define precedence explicitly, for example by raising
  the focused window's family while preserving child-above-parent order.
  Specify how hidden relatives and relatives on other workspaces participate.
- **Affected text:** plan sections 1.2, 3.5, WP6, and planning decision 7;
  `spec.md` sections 3.1 and 3.6.
- **Required regression:** focus a parent with a visible child and grandchild
  among unrelated windows. Assert the chosen complete stacking order without
  changing workspace stack order; also cover hidden relatives and parent close.
- **Decision (2026-10-16):** raise the focused window's family. Parent links
  group shown windows into families. The focused family stacks on top, the
  others follow StackSet order, each parent sits directly below its children,
  and the focused branch is on top among siblings. Hidden windows sit below all
  shown windows. A window whose parent is closed, unknown, or not shown starts
  its own family. This satisfies river's recommendation that children render
  directly above their parent. xmonad 0.18.1 never raises the focused window
  and floats transients instead, so the raise is a named seki deviation.
  Rejected: raising only the focused window, which covers a parent's dialog,
  and raising only the focused subtree, which lets unrelated windows separate
  a dialog from its parent. The top family is not remembered after focus
  leaves it; M2 floating may need a remembered raise order. Updated: plan
  sections 1.2, 3.5, WP6, and planning decision 7; `spec.md` sections 3.1
  and 3.6.
- **Additional regression:** focusing a child places its parent below it,
  with only that parent's other descendants between them and no unrelated
  window.

### M1-D04 — Zero-sized tiles and their focus behavior

- **Problem:** section 3.2 handles an empty usable area, but splitting a positive
  rectangle can also yield individual zero-width or zero-height tiles. Passing
  zero to `propose_dimensions` lets the client choose that dimension; it does
  not produce an empty tile and can cause overlap under M1's mismatch policy.
- **Proposed resolution:** define how degenerate tiles are handled before
  dimension proposals, for example by hiding them. Explicitly decide the
  keyboard-focus behavior when the StackSet focus has no drawable tile.
- **Affected text:** plan sections 3.2, 3.5–3.6, WP1/WP3/WP6, and exit criteria;
  `spec.md` sections 3.3, 3.4, and 3.6.
- **Required regression:** split a one-pixel rectangle among multiple windows,
  test Tall ratios at and near 0 and 1, and repeat through Mirror. Assert that
  no degenerate tile becomes a client-selected visible size, verify focus, and
  verify recovery when the available area grows.
- **Decision (2026-10-16):** a tile with zero width or zero height is treated
  as omitted by the layout (M1-D02): the window is hidden, receives no
  proposal, and keeps its last dimensions. Only exact zero is degenerate; a
  tile a few pixels wide is still proposed, and an oversized commit is M1's
  general mismatch behavior. The keyboard-focus target is the StackSet's
  focused window only while it is shown; otherwise the manage sequence sends
  `clear_focus`. The StackSet focus never changes for rendering reasons. When
  the window becomes shown, the render sequence shows it with the focused
  border and requests `manage_dirty`; the next manage sequence sends
  `focus_window`. Rejected: clamping to one pixel as xmonad's `tileWindow`
  does, because clients commit their minimum size and overlap neighbours;
  focusing the hidden window, which sends keystrokes to an invisible client;
  moving StackSet focus to a visible window, which breaks xmonad semantics;
  and sending focus with the proposal, which saves one manage round trip but
  briefly focuses a hidden window. Updated: plan sections 3.2, 3.5, 3.6, WP3,
  WP6, and exit criteria; `spec.md` sections 3.3, 3.4, and 3.6.
- **Additional regression:** a spawned window has no focus until its first
  dimensions settle, then is shown with the focused border and receives
  `focus_window` in the following manage sequence.

### M1-D05 — Tiled flags at output boundaries

- **Problem:** section 3.5 allows tiled flags only on edges adjacent to another
  tile. A single Tall window receives no flags, which the protocol defines as
  not being part of a tiled layout. This can produce inappropriate client-side
  decorations for a tiled window.
- **Proposed resolution:** define how output and usable-area boundaries count
  toward tiled edges, ensuring single-window tiled layouts remain marked tiled.
  Keep geometric tile adjacency distinct from the final protocol flags.
- **Affected text:** plan sections 3.2, 3.5, WP1/WP6, and exit criteria;
  `spec.md` section 4.3. WP1's adjacency API should accommodate the WP6 decision.
- **Required regression:** assert protocol flags for a single Tall window,
  Full's selected window, a multi-window split, and an output reduced by an
  exclusive zone. Check both internal edges and usable-area boundaries.
- **Decision (2026-10-16):** every window the layout places receives all four
  tiled edges, independent of geometry. Hidden windows keep their last flags;
  M2 floating windows will receive none. For `Tall`, `Mirror Tall`, and
  `Full`, which partition the usable area exactly, this equals counting both
  tile adjacency and usable-area boundaries. Rejected: tile adjacency alone,
  which tells a single tiled window that it is not tiled; and adjacency plus
  boundaries as a general rule, which tells an isolated tile in a future gap
  layout that it is not tiled. Gap layouts decide their own rule when one
  exists. WP1's `adjacentEdges` is removed as unused. Updated: plan sections
  3.1, 3.5, WP1, and WP6; `spec.md` section 4.3.
- **Required regression (revised):** all four flags for a single Tall window,
  Full's selected window, every tile of a multi-window split, and a layout
  over a usable area reduced by an exclusive zone; a window with a pending
  first proposal is flagged in the same manage sequence as the proposal.

### M1-D06 — Dialogs are tiled in M1

- **Problem:** xmonad 0.18.1 floats every transient or fixed-size window in
  `manage`, so dialogs sit above the tiled layer. M1 defers floating, so a
  window with a parent is an ordinary tile. Under `Full` (M1-D02) a dialog
  takes focus and fills the area; refocusing its parent hides the dialog, and
  the application can look frozen until focus returns to the dialog.
- **Decision (2026-10-16):** accept this as a known M1 limitation and revisit
  it with M2's ManageHook and floating. M1 adds no special case to M1-D02's
  shown-state rule.
- **Affected text:** plan section 1.2.
- **Required regression:** a request-level test pins the M1 behavior: a new
  child window is inserted and focused like any window, and under `Full`
  focusing its parent hides it. M2 changes this test deliberately when it
  floats dialogs.

### M1-D07 — Borders outside the tile

- **Problem:** section 3.5 says borders do not affect content positions or
  dimensions, so the planner proposes the whole tile as content. River's
  `set_borders` states that a window's position and dimensions are its
  content's and are unaffected by borders, which implies that borders extend
  outside the content box. If they do, each border overlaps the neighbouring
  tile and extends past the usable area, under a bar or off the output.
  xmonad 0.18.1's `tileWindow` instead shrinks the window by twice the border
  width so that content and border fit inside the tile.
- **Evidence:** river 0.4.6's `drawBorders` (`river/Window.zig`) places the
  left border at `x = -width` and the right border at `x = content.width`,
  and likewise for top and bottom, so borders surround the content box from
  outside. River 0.5.0 draws them the same way. A nested headless river 0.5.0
  run proposed 1180×620 at (50, 50) with a 20 px border. The screenshot showed
  content at x 50–1229 and y 50–669, and the border at x 30–49 and 1230–1249
  and y 30–49 and 670–689. River 0.4.6 also calls `setSize` on every edge
  after the per-edge branch, which reads as drawing edges that were not
  requested; 0.5.0 removed that call. This was read from source, not tested.
- **Decision:** inset content by the border width on all four sides, as
  xmonad does. A placed window's content rectangle is its tile shrunk by `b`
  on each side, at the tile origin plus `(b, b)`. A tile no larger than `2b`
  in either dimension leaves zero content and is degenerate under M1-D04: the
  window is hidden and receives no proposal, instead of xmonad's 1 px clamp.
  `Full` keeps the border, as in core xmonad; hiding a lone window's border is
  a later layout modifier like xmonad-contrib's `smartBorders`. River draws the
  border around the content the client commits, so a smaller commit leaves a
  gap inside the tile and a larger one overflows it, which is M1's general
  size-mismatch behavior. A border width of zero gives no inset.
- **Affected text:** plan sections 3.5, 3.6, WP6, and exit criteria;
  `spec.md` sections 3.3 and 4.1 (the focus border row).
- **Required regression:** on real river, record where a border is drawn
  relative to its content. In request-level tests with border width `b`,
  assert that proposed content plus borders stays inside each tile. For
  size-conforming clients, assert that adjacent borders do not overlap and a
  single window's border stays inside the usable area. Verify that a tile of
  at most `2b` follows M1-D04. Oversized actual commits may overflow under the
  accepted M1 mismatch policy.

### 9.1 Regression coverage and closeout

Each entry below names the tests that assert its required and additional
regressions. WP10 ([#14](https://github.com/chiro/seki/pull/14)) added the
last of them.

| Decision | Regression evidence |
|---|---|
| M1-D01 | `testLayerFocus` in [ReconcileTest](../test/Seki/Run/ReconcileTest.hs) covers key-action reclaim, its border transition and the following `focus_none`, and exclusive focus ignoring both a click and a focus key. `testFamilies` in [ReconcileTraceTest](../test/Seki/Run/ReconcileTraceTest.hs) covers click reclaim, its focused border, the focus request resent after `focus_none`, and a new window leaving layer focus in place. The live `m1-layer-focus` scenario in [M1.hs](../integration/Seki/Integration/M1.hs) maps an on-demand layer surface on real river. It checks that a new window and a layout message keep keyboard input with the layer, and that a click and a focus key each return it to a window after river's `focus_none`. |
| M1-D02 | "Full shows only a smaller focused commit and focus cycling hides it" in [ReconcileTraceTest](../test/Seki/Run/ReconcileTraceTest.hs) covers three windows under Full and focus cycling. "a custom layout omitting windows hides them" in [SceneTest](../test/Seki/Internal/Core/SceneTest.hs) covers a custom omitting layout. `testLayouts` in [ReconcileTest](../test/Seki/Run/ReconcileTest.hs) covers Tall → Full → Tall with no proposal to omitted windows. The live `m1-layout-cycle` scenario checks the same transitions in pixels. |
| M1-D03 | [SceneTest](../test/Seki/Internal/Core/SceneTest.hs) covers a focused parent with a child and grandchild above unrelated shown windows, a focused child with an unrelated shown window, a parent on another workspace, a closed parent, and hidden relatives below every shown window. `testFamilies` in [ReconcileTraceTest](../test/Seki/Run/ReconcileTraceTest.hs) checks that a raise leaves the workspace stack order unchanged. |
| M1-D04 | [TallTest](../test/Seki/Internal/Layout/TallTest.hs) covers one-pixel rectangles and extreme ratios through Mirror. "one-pixel Tall and Mirror Tall scenes omit every zero tile" and the generated scene property in [SceneTest](../test/Seki/Internal/Core/SceneTest.hs) cover omission and focus. The generated Full, Tall, and Mirror Tall traces in [ReconcileTraceTest](../test/Seki/Run/ReconcileTraceTest.hs) assert that no degenerate tile is proposed or shown, focus only a shown settled window, and end in recovery. `testSettlement` in [ReconcileTest](../test/Seki/Run/ReconcileTest.hs) covers `clear_focus` and the `manage_dirty` recovery path. |
| M1-D05 | `testSingleTall` in [ReconcileTest](../test/Seki/Run/ReconcileTest.hs) covers a single Tall window. `testSettlement` covers Full's selected window over a usable area that an exclusive zone reduced to 100x80 on a 1000x1000 output, flagged in the same sequence as its first proposal. `testLayouts` covers both tiles of a Tall split. The live probe does not observe xdg tiled states. |
| M1-D06 | "Full focuses a new child and hides it when its parent is refocused" in [ReconcileTraceTest](../test/Seki/Run/ReconcileTraceTest.hs) pins M1's tiled dialogs. M2 changes it deliberately when it floats dialogs. |
| M1-D07 | [SceneTest](../test/Seki/Internal/Core/SceneTest.hs) checks the exact border inset of every placed window and degenerate tiles of at most `2b`. `testSettlement` and `testLayouts` in [ReconcileTest](../test/Seki/Run/ReconcileTest.hs) check the request-level content, border, and accepted oversized commit. `checkBorder` in [M1.hs](../integration/Seki/Integration/M1.hs) checks real border pixels outside the content of conforming clients in every M1 scenario. |

The StackSet invariant, hotplug history, and generated event-sequence
properties run at 1000 QuickCheck cases in `cabal test all`. Unsupported client
requests leave the policy state unchanged in `testSettlement`.

WP10 ran these closeout checks on 2026-10-18 with GHC 9.6.7 and system river
0.5.0:

- `cabal build all` and `cabal test all` pass;
- `fourmolu --mode check` reports no changes, and `hlint` remains advisory;
- `ci/check-package.sh` validates the source distribution and Haddock; and
- `WLR_RENDERER=pixman ci/run-integration.sh --offline` passes all 14
  scenarios and verifies the complete manifest.

The pinned command `nix develop --no-write-lock-file .#ci --command
ci/run-integration.sh` passed in the CI `integration` job of
[run 39114975118](https://github.com/chiro/seki/actions/runs/39114975118) on the same tree that
[#14](https://github.com/chiro/seki/pull/14) merged. The historical
[latency report](latency-baseline/report.md) remains the recorded latency evidence;
it does not certify a new host or toolchain.
