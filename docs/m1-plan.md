# M1 Core Implementation Plan

Status: **planning baseline** / Last updated: 2026-08-17

This document turns the M1 row in the specification into an ordered,
testable implementation plan. It is intentionally narrower than the complete
v1 design in `spec.md`.

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

M0 already delivered more of the original M1 row than the roadmap wording
suggests:

- `ManageM` and `RenderM` are distinct pure request plans;
- the runtime validates manage/render sequence order;
- all M1 window requests already have phase-safe wrappers, including borders,
  tiled edges, focus, show/hide, positioning, and stacking;
- window/output inventory, node acquisition, key binding objects, and the
  real-river integration harness are release-gated.

M1 extends these pieces. It must not replace the native transport, scanner, or
phase state machine.

### 1.2 Explicitly deferred to M2+

- Floating placement/rendering, public floating actions, and pointer move/resize
- `ManageHook`
- Fullscreen policy
- Focus-follows-mouse
- `Crop` / `Tolerate` and content clip boxes
- Submaps and binding repeat
- Hot-swap, serialization, and restoration
- Input-device rules
- Layer-shell focus arbitration beyond the state needed to avoid false focus
  borders
- Bar IPC, `sekictl`, scratchpads, urgency, and prompt UI

M1 temporarily accepts a client's actual dimensions when they differ from the
proposed tile. M2 replaces that behavior with the configured `Crop` or
`Tolerate` policy.

M1 has one fixed focus policy: `window_interaction` focuses the interacted
window, and the scene planner raises the focused window to the top of the
explicit stacking order. `pointer_enter` / `pointer_leave` cause no core policy
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
- adapt Screen's StackSet and geometry properties, but defer its X11
  resize/aspect-hint properties with `Tolerate` to M2;
- replace Failure's partial `abort` / invalid-`new` checks with total
  construction and configuration validation returning typed errors; and
- port Floating's pure map-bookkeeping properties internally, while deferring
  public floating actions, scene generation, and interactive movement to M2.

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
- floating state and pure bookkeeping exist, but floating scene/runtime
  behavior is deferred;
- the runtime may be temporarily headless;
- a live output can exist when there is no hidden workspace left to assign;
- visibility is an explicit protocol state rather than an X workspace side
  effect.

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

### 3.1 Proposed modules

| Module | Responsibility |
|---|---|
| `Seki.Core.Geometry` | `Point`, `Size`, `Rect`, safe splitting/mirroring, tiled-edge adjacency |
| `Seki.Core.StackSet` | Workspace/screen/stack zipper and xmonad-compatible pure operations |
| `Seki.Core.Layout` | `LayoutClass`, messages, `Choose` / `(|||)`, `Mirror` |
| `Seki.Layout.Tall` | `Tall`, tile/split functions, `Shrink`, `Expand`, `IncMasterN` |
| `Seki.Layout.Full` | `Full` |
| `Seki.Core.State` | Headless/attached policy state, queued core events, output hotplug |
| `Seki.Core.Action` | Pure user actions: `windows`, `sendMessage`, kill/spawn intents |
| `Seki.Core.Scene` | `DesiredScene` and pure planning |
| `Seki.Run.LayerShell` | layer-shell global binding and per-output/per-seat objects |
| `Seki.Run.Reconcile` | persistent proposal/applied state and phase-safe diffs |
| `Seki.Run.Policy` | bridge from inventory/events/actions to core state and scenes |

`Seki.Run.Full` remains available while the M1 policy is developed, then
becomes a compatibility implementation or delegates to the new `Full` planner.

The Cabal component graph must stay acyclic when the pure core becomes public.
Introduce an internal `seki-core` library, make `seki-wire` depend on it, and
make the public `seki` library depend on both. The public library re-exports the
supported `Seki.Core.*` / `Seki.Layout.*` modules and provides the `Seki` front
door; tests may continue to address the internal libraries directly. Do not
compile the same source module independently into multiple components.

WP1 must prove this topology before the core port grows: build a minimal
cross-component module, re-export it from the public library, and verify a
downstream import plus `cabal haddock all` and source-distribution contents.

### 3.2 Core identifiers and geometry

The pure modules must not contain `Object interface` handles. Use small
`WindowId` and `OutputId` newtypes that can be converted to/from Wayland
`ObjectId` in the runtime bridge.

Geometry rules:

- positions are signed `Int32` global logical coordinates;
- dimensions are non-negative and use wider intermediates for multiplication,
  ratio application, and addition before checked conversion;
- a zero-area `non_exclusive_area` is valid input but produces no visible tile;
- splitting assigns every input pixel exactly once, including odd dimensions;
- `Mirror` exchanges axes without losing negative origins.

### 3.3 StackSet and headless state

Keep the active StackSet zipper semantically aligned with xmonad. Represent
the no-output case with a wrapper state rather than weakening the StackSet's
non-empty current-screen invariant:

```haskell
data CoreState i l
  = HeadlessState (HeadlessWorkspaces i l)
  | AttachedState (StackSet i l)
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
- live inventory windows not yet incorporated at a sequence boundary are
  explicitly pending, never silently lost.

### 3.4 Events and Actions

Inventory callbacks queue small pure events. A `manage_start` drains the queue
in arrival order and applies it to `CoreState` before planning. The initial M1
event set is:

- window created/closed;
- output created/removed/geometry changed;
- window interaction (click-to-focus);
- layer-shell usable-area changed;
- layer-shell focus changed (`exclusive`, `non-exclusive`, or `none`); and
- key-binding action.

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

`DesiredScene` contains only pure identifiers and values. For every known
window it specifies:

- shown or hidden;
- target content rectangle when shown;
- tiled adjacency edges;
- SSD/CSD choice;
- border edges, width, and premultiplied RGBA color;
- stacking position; and
- whether it is the keyboard-focus target.

Planner rules:

- visible workspace windows receive the active layout's rectangles;
- hidden workspace windows are always explicitly hidden;
- incomplete outputs or zero-area usable rectangles show no windows;
- `Full` maps each stack window to the full rectangle and places the focused
  window on top;
- stacking is deterministic, explicitly complete, and respects child-above-
  parent constraints from inventory;
- `set_tiled` edges name only edges adjacent to another tiled rectangle;
- borders do not affect content positions or dimensions; and
- exactly one window may be the focus target.

### 3.6 Proposal and render state machine

The reconciler owns state that persists across sequences:

- last applied manage attributes;
- last applied rendering attributes;
- most recent actual dimensions and their inventory revision;
- at most one in-flight proposal per window; and
- a queued target rectangle when layout changes supersede an in-flight
  proposal.

Manage reconciliation sends, in deterministic order:

1. decoration changes and an explicit empty M1 capability set;
2. tiled-edge changes;
3. required dimension proposals;
4. focused-output layer-shell `set_default`; and
5. seat focus or clear-focus.

Render reconciliation sends, in deterministic order:

1. positions for settled visible windows;
2. a complete bottom-to-top stacking order;
3. borders;
4. explicit show/hide state; and
5. render completion through the existing runtime.

A proposal is settled only after a newer dimensions revision is observed.
While a new window has no settled dimensions it remains explicitly hidden.
If the desired dimensions change while a proposal is in flight, retain only
the latest desired rectangle, finish the current render sequence safely, and
request another manage sequence with `manage_dirty` after sequence completion.

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
- define checked rectangle construction, split, mirror, translation, and edge
  adjacency;
- document all signed-coordinate and overflow behavior.

Tests:

- build a downstream-style module that imports the re-export through the
  public library;
- run `cabal haddock all` and inspect the generated public module index;
- verify the source distribution contains every core source and generated
  documentation input exactly once;
- unit examples for negative origins, zero area, one-pixel and odd splits;
- properties for area preservation, no overlap, bounds, and mirror involution.

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

- port applicable xmonad Tall/Full properties;
- prove tiles remain within the supplied rectangle and do not overlap;
- cover `Tall` ratios 0 and 1, zero masters, more masters than windows, odd
  dimensions, negative origins, Mirror, nested choices, and layout wraparound;
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
- track `non_exclusive_area` revisions;
- create the seat object needed to track whether a layer surface owns focus;
- suppress window focus requests and focused borders while a layer surface has
  accepted focus, restoring StackSet focus after `focus_none`;
- send `set_default` for the focused output in a manage sequence;
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
- post-render `manage_dirty` scheduling for superseded proposals.

Tests:

- scene properties for window uniqueness, visibility, adjacency symmetry, one
  focus target, and deterministic stacking;
- an interaction-focused window becomes the top node without disturbing its
  workspace's stack order;
- request-level mock tests for unchanged scenes, focus-only changes, workspace
  hiding, layout changes, delayed dimensions, superseded proposals, closed
  pending windows, and output geometry changes;
- request-level tests proving every new window receives empty capabilities and
  unsupported client requests do not mutate core state;
- compile-surface tests that manage-only operations cannot appear in
  `RenderM` and render-only operations cannot appear in `ManageM`.

### WP7 — Default runtime and minimal public API

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

- all properties in the explicit xmonad suite disposition pass or have their
  documented typed-error/M2 replacement;
- StackSet/event-sequence invariant tests pass at the configured high
  QuickCheck count;
- Tall, Mirror Tall, Full, and choice-message properties pass;
- mock protocol tests cover delayed dimensions and prove every sequence is
  completed or returns a structured runtime error;
- hidden workspace windows receive `hide`, and only visible settled windows
  receive `show`;
- focus, borders, complete stacking, and tiled edges are observable in tests;
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
| No protocol correlation ID for dimension proposals | WP6 mock spike with delayed/superseded proposals | One in-flight proposal per window plus dimensions revisions and queued latest target |
| Headless/hotplug state distorts StackSet invariants | WP2 generated attach/detach sequences | Sum type around an unchanged active StackSet zipper |
| Layer-shell child creation does not fit current handler composition | WP5 socket-pair prototype before policy integration | Give runtime explicit layer-shell object ownership and cleanup |
| Private-sublibrary re-exports build but produce broken public docs or sdist contents | WP1 downstream import, Haddock, and sdist spike | Fix the component/source-dir topology before porting core modules; never compile one module into multiple components |
| Scene and inventory diverge on close/remove during pending work | WP6 generated reducer/reconciler traces | Key every state table by pure ID and prune atomically at sequence boundaries |
| Full stacking requests become quadratic | Planner/request-count property at 100/500 windows | Emit one bottom anchor and an ordered chain; no all-pairs constraints |
| Public API hardens temporary internals | Compile a minimal config only after WP6 types settle | Export actions/configuration, not inventory or reconciliation state |
| Performance evidence is host-noisy | Fixed seed, workload, build, and repeated samples | Report distributions; keep wall-clock gating out of unit CI |

## 8. Planning Decisions

This baseline recommends the following choices:

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
   click-to-focus with the focused window raised. Fullscreen and
   focus-follows-mouse are enabled only when M2 implements them.
8. **Own the exclusive-zone integration fixture.** A deterministic Haskell
   fixture uses seki's native Wayland transport and a pinned integration-only
   wlr-layer-shell binding; it does not link `libwayland` or depend on Waybar.

If any of these decisions changes, revise the affected work packages before
implementation begins.
