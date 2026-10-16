# seki Specification — A river Window Manager in Haskell

Status: **v1 design accepted — M0 complete, M1 next** / Last updated: 2026-10-16
Legend: [DECIDED] = current design decision, [RESOLVED] = checked against the protocol or other available evidence

This is the target design for v1, not a description of the currently exported
API. Implementation availability is tracked by the milestones in section 6.2.

---

## 1. Goals and Non-Goals

### 1.1 Goals
- Carry xmonad's semantics and development experience (configuration = a Haskell program importing a library) forward onto Wayland.
- Operate as a window manager implementing river's (0.4+) river-window-management-v1 protocol. We do not write a compositor. [DECIDED]
- Reuse xmonad's pure core (StackSet, layout algebra) — its design and its already-verified properties — as far as possible.

### 1.2 Non-Goals
- Compositor functionality (DRM/KMS, rendering, Xwayland implementation). All delegated to river.
- Animations and window effects (also outside river's scope).
- Multi-seat support (v1 is single-seat only). [DECIDED]
- VR, non-planar desktops.

## 2. Architecture

```
┌──────────────────────────────────────────────┐
│ User's Main.hs (= config = WM executable)    │
├──────────────────────────────────────────────┤
│ This library (public API)                    │
│   ├ Pure core: StackSet / LayoutClass /      │
│   │            ManageHook / DesiredScene     │
│   └ Runtime: manage/render sequence state    │
│              machine, reconciler, event loop │
├──────────────────────────────────────────────┤
│ Protocol binding layer                       │
│   river-window-management-v1 and other       │
│   river extensions (Haskell wire             │
│   implementation + own codegen — see 2.2)    │
├──────────────────────────────────────────────┤
│ river (compositor, separate process)         │
└──────────────────────────────────────────────┘
```

- The WM is a privileged client connected to the Wayland socket. If it crashes the session survives, and hot-swapping is possible.
- Keybinding matching happens on the compositor side (river_xkb_binding), and pointer motion alone does not wake the WM. Actions handled by the WM can still be delayed by a GC pause.

### 2.1 Protocol Dependencies (river extensions)
- river-window-management-v1 (core)
- river-input-management-v1 / river-libinput-config-v1 / river-xkb-config-v1 (input configuration)
- river-layer-shell-v1 (policy delegation for layer surfaces)
- [RESOLVED] river 0.4.5 ships river-window-management-v1 version 4. v2 added set_clip_box / unreliable_pid / set_xcursor_theme / pointer_position; v3 added set_content_clip_box / pointer_warp; v4 added, among other features, the unique window identifier used for state restoration. This WM **requires version 3 functionality** for Crop and uses version 4 functionality when available. The minimum supported river release advertises version 4.
- Keybindings live in river-xkb-bindings-v1 (a separate protocol, version 3 in river 0.4.5). [RESOLVED] Read; reflected in 3.8.

### 2.2 Binding Strategy [DECIDED: Haskell wire implementation without libwayland]
- Implement the Wayland wire protocol (unix domain socket + fd passing via SCM_RIGHTS) in Haskell without depending on libwayland. This avoids directly integrating libwayland's C event loop with the GHC runtime and follows the general approach used by Rust's wayland-rs.
- Also maintain our own code generator from protocol XML to a typed Haskell API. Owning the generator lets us handle special marshalling — such as libinput's array of doubles (3.9) — in a typed way.
- The hs-wayland-scanner + libwayland FFI approach is retained as a fallback and as a reference during implementation.

## 3. Design of the Pure Core

### 3.1 Mapping StackSet

Port xmonad's StackSet structure, changing only the type parameters. [DECIDED]

```haskell
data StackSet i l = StackSet
  { current  :: Screen i l          -- focused output
  , visible  :: [Screen i l]        -- workspaces shown on other outputs
  , hidden   :: [Workspace i l]     -- hidden workspaces
  , floating :: Map WindowId FloatGeom }

data Screen i l    = Screen { workspace :: Workspace i l
                            , output    :: OutputId
                            , outRect   :: Rect }      -- logical coordinates
data Workspace i l = Workspace { tag :: i, layout :: l
                               , stack :: Maybe (Stack WindowId) }
data Stack a       = Stack { focus :: a, up :: [a], down :: [a] }
```

| xmonad | This spec | Notes |
|---|---|---|
| `a` (X11 Window) | `WindowId` (river_window_v1) | xdg_toplevel and Xwayland handled transparently |
| `sid` (ScreenId) | `OutputId` (river_output_v1) | |
| `sd` (Rectangle) | `Rect` (from the output's position/dimensions) | Logical coordinates; scaling is river's job |
| `i` (WorkspaceId) | `String` (or a newtype) | The compositor knows nothing about it |

- Pure operations such as `greedyView` / `view` / `shift` / `focusUp` / `focusDown` / `swapMaster` are ported from xmonad with their meaning unchanged. The QuickCheck properties are ported too.
- [RESOLVED] A hidden workspace means **explicitly sending** river_window_v1.hide (rendering state; borders and decorations are hidden along with it). New windows are shown by default, so hide must always be sent when placing a window on a hidden workspace. [DECIDED] Reconciler invariant: "placed by the active layout on a visible workspace, with settled dimensions ⇔ in the shown state". A window the layout omits (for example every non-focused window under `Full`) is hidden, as xmonad unmaps it, and keeps its last dimensions without a new proposal.
- [RESOLVED] Inputs available to ManageHook: app_id (**may be null**), title (**may be null**), parent (for dialog detection), decoration_hint, dimensions_hint (min/max, 0 = unspecified), unreliable_pid, and the version 4 identifier (non-empty printable ASCII). These are guaranteed to arrive after the window event and before the next manage_start. Since app_id/title can be null for Xwayland windows, the core types are `Maybe Text`.
- [RESOLVED] The parent event makes windows form a tree (the server guarantees there are no loops). The protocol recommends drawing child windows directly above their parent → fold this into the stacking-order computation in `plan`. This information was not directly available in xmonad's X11 model.
- [DECIDED] Stacking order: parent links group the shown windows into families. The focused window's family is on top and the others follow StackSet order. Within a family each parent sits directly below its children, and among siblings the branch containing the focused window is on top. Hidden windows sit below every shown window. Stacking is derived from the StackSet and never changes it. Deviation from xmonad, which never raises the focused window.

### 3.2 Multi-Monitor Semantics [DECIDED: xmonad standard]

- The workspace pool is shared across all outputs.
- The default for switching workspaces is `greedyView`: if the target workspace is displayed on another output, swap workspaces between that output and the current one. `view` (no swap) is also provided as a library function so users can choose per keybinding.
- Output hotplug:
  - On add: assign the first hidden workspace to the new output (xmonad-compatible).
  - On remove: the workspace that was displayed returns to hidden. Focus moves to a surviving output.
  - [RESOLVED] Output removal is signalled in the order river_output_v1.removed → manage_start. In the protocol windows do not belong to outputs (only global logical coordinates exist), so there is no collateral state change; reassigning workspaces is purely a StackSet operation in the WM. Exception: a fullscreen window on a removed output is treated as having been exit_fullscreen'd in the same manage sequence, so the WM must re-issue propose_dimensions + set_position for that window within the same sequence.
  - [RESOLVED] The server guarantees that the logical regions of multiple outputs do not overlap (as of the position/dimensions events).
- IndependentScreens-style behaviour (a separate workspace set per output) is a non-goal for v1. However, keeping the core's `i` polymorphic keeps it implementable later as an extension library.

### 3.3 Two-Phase Commit and the Reconciler

Split the core into three stages. ① and ② are pure; only ③ does IO.

```haskell
step      :: Event -> StackSet i l -> StackSet i l           -- ① state transition
plan      :: StackSet i l -> DesiredScene                    -- ② layout computation
reconcile :: DesiredScene -> ActualState -> Protocol ()      -- ③ apply the diff
```

- `DesiredScene` is a declarative description holding, per window, (proposed dimensions, position, stacking order, decoration, dimension-mismatch policy), plus (keyboard focus, decoration differences for the focused window).
- [RESOLVED] The rectangle a layout is given can be either the whole output or river_layer_shell_output_v1.non_exclusive_area (the region left after subtracting exclusive zones such as bars, in global coordinates); the latter is the default. **This gives us the equivalent of avoidStruts with no computation of our own.** Layouts are free to ignore it (fullscreen-like layouts).
- ③ sends proposed dimensions, focus, and so on in the manage sequence, then finalizes positions in the render sequence once the client's actually-committed dimensions (the dimensions event) arrive.
- Handling of the mismatch between proposed and actual dimensions is **selectable per layout**. [DECIDED: option (c)]
  - `Tolerate`: place the window at its actual size and allow gaps (equivalent to xmonad honouring resize hints).
  - `Crop`: maintain strict tiling using the protocol's window cropping.
  - The default is `Crop` (favouring visual consistency as a tiling WM). Switch with the layout modifier `tolerant :: l -> ModifiedLayout Tolerate l` (an API isomorphic to xmonad's `layoutHints`).
  - [RESOLVED] Crop is implemented with set_content_clip_box (v3). It clips only the content, using a rectangle relative to the window's top-left; compositor-drawn borders are drawn around the intersected region after clipping, so the appearance does not break. It is ignored while fullscreen, but fullscreen is compositor-managed so this does not matter.
- [RESOLVED, IMPORTANT] The dimensions event is **not guaranteed to come back in the render sequence immediately following** a propose (slow-to-respond clients). In that case the server sends it in some future render sequence. The reconciler therefore keeps a set of "proposed but not yet finalized" windows and calls render_finish with a provisional placement (or hidden) until they settle. Because the protocol guarantees a new window is not displayed until its first dimensions are settled, there is no flicker on initial display.

### 3.4 LayoutClass

```haskell
class LayoutClass l where
  pureLayout  :: l -> Rect -> Stack WindowId -> [(WindowId, Rect)]
  handleMsg   :: l -> SomeMessage -> Maybe l    -- Shrink / Expand / IncMaster ...
  sizePolicy  :: l -> SizePolicy                -- Crop | Tolerate (default from 3.3)
  description :: l -> String
```

- The algebra of `Choose` (`|||`), `Mirror`, and `ModifiedLayout` is ported from xmonad.
- [DECIDED] `pureLayout` may omit windows of the stack. Omitted windows are hidden (3.1). `Full` returns only the focused window at the whole rectangle, as in xmonad.
- Layouts bundled in v1: `Tall`, `Mirror Tall`, `Full`. Anything else arrives incrementally as the equivalent of contrib.

### 3.5 Floating [DECIDED: absolute logical pixel coordinates]

- `FloatGeom = Rect` (logical pixels). The coordinate origin, however, is **the top-left of the output currently displaying that workspace** (not global coordinates).
  - Rationale: when a workspace moves to another output (a greedyView swap), global coordinates would strand the window in the old output's region. Making it output-relative gives the intuitive behaviour that floating windows travel with their workspace.
- Normalization rules on inter-output moves and resolution changes:
  - Reinterpret the origin as the new output's top-left (coordinate values are preserved).
  - If the window would fall outside, clamp so that its top-left is always inside the output. Dimensions are not changed (respecting user-chosen size), except that a window larger than the output is proposed down to the output's size.
- Multi-DPI: because the coordinate system is logical, river absorbs scale differences. The WM never deals in physical pixels.
- [RESOLVED] Interactive move/resize flow:
  - Client-initiated (e.g. dragging a CSD titlebar): pointer_move_requested / pointer_resize_requested events → the WM responds at its discretion.
  - WM-initiated (mod+drag): river_seat_v1.get_pointer_binding (button + modifiers) → pressed event → op_start_pointer.
  - During the drag, op_delta events carry the **cumulative offset from the starting point** (not per-event deltas, which keeps float geometry math simple); op_release on button release; the WM finishes with op_end. During a resize, send inform_resize_start / inform_resize_end to the window.
  - Initial floating size: propose_dimensions(0, 0) lets the window decide for itself. Make this the default for the initial placement of dialogs and similar floats.

### 3.6 Focus Model

- The StackSet's `focus` = the keyboard focus of the single seat. [DECIDED: single seat]
- Both click-to-focus and focus-follows-mouse are selectable in configuration.
  M1 implements a fixed click-to-focus policy; the configurable focus model and
  focus-follows-mouse arrive in M2.
- [RESOLVED] The protocol supports both focus models directly:
  - focus-follows-mouse: pointer_enter / pointer_leave events (per-window crossing notifications; the window's region includes the input regions of borders and decorations) → focus_window on enter.
  - click-to-focus / raise: the window_interaction event (a "this window was interacted with" notification abstracting over button presses, touch, and tablet input — policy over mechanism). Raising applies to the focused window's family (3.1).
  - If raw pointer coordinates are needed, the pointer_position event provides them (only when changed, once per manage sequence; because coordinate changes alone do not trigger a sequence, pointer motion will not swamp a WM written in a GC'd language).
- [RESOLVED] Arbitration with layer surfaces: signalled by river_layer_shell_seat_v1's focus_exclusive / focus_non_exclusive / focus_none events. While exclusive (lock screens, keyboard-grabbing launchers, etc.) **the WM's focus requests are ignored**, so there are periods where StackSet focus and actual keyboard focus diverge. The core leaves the StackSet untouched and reflects "no actual focus" only in the decoration computation of DesiredScene. On focus_none, return focus to the StackSet's focused window.

### 3.7 State Persistence and Hot-Swap

- On WM exit/restart (the equivalent of mod-q), serialize the window identifier→(tag, floating state, position within the stack) mapping and restore it on reconnect.
- [RESOLVED] river-window-management-v1 version 4 sends each window a non-empty identifier that is unique and never reused. Use it to match surviving windows during a WM hot-swap instead of app_id/title/PID heuristics.
  - Windows created during the handover have no serialized entry and follow the normal initial-placement rules. Serialized entries whose windows closed during the handover are ignored.
  - A protocol-compatible compositor exposing only version 3 can run the core WM, but state restoration degrades to placing re-enumerated windows on the configurable default workspace. Do not guess identity from unreliable_pid, which is explicitly unsuitable for identity and security decisions.
  - Validate identifier continuity across a real river WM reconnect during M2.
- Layout internal state (master ratio, etc.) is also serialized via Read/Show (the same approach as xmonad).

### 3.8 Keybinding Model and Submaps [RESOLVED: river-xkb-bindings-v1 (v3) read]

- Bindings are registered as individual objects via get_xkb_binding(seat, **keysym**, modifiers) — based on xkbcommon keysyms, not keycodes. Key events matching a binding are not delivered to the focused window.
- Each binding can be enabled/disabled within a manage sequence. Implement **submaps as enable/disable switching of sets of bindings**:
  - Core representation: `Bindings = Map SubmapName (Map (Keysym, Mods) Action)`. Submap transitions are pure state transitions; the reconciler turns the enable/disable diff into protocol requests.
  - Escaping a submap via an unbound key: requesting river_xkb_bindings_seat_v1.ensure_next_key_eaten causes an ate_unbound_key event when the next non-modifier key does not match a binding (the spec's Rationale explicitly names submaps as the use case). Leave the submap on ate_unbound_key.
  - Escaping via timeout: WM-side timer → manage_dirty → cancel_ensure_next_key_eaten. The spec allows a race where ate_unbound_key arrives before the cancel, so handle it idempotently.
- Both pressed and released events arrive (released fires when the "main" key is released; releasing only modifiers does not fire it). Repeating an action while a key is held (volume keys, etc.) is **implemented with a WM-side timer**, stopped by the stop_repeat event (sent when another key is pressed). Note that set_repeat_info is the repeat configuration for ordinary keys delivered to clients — a different thing from binding repeat.
- set_layout_override: pins the xkb layout used to interpret a binding. **When several layouts are in use (Japanese/US, etc.), registering bindings pinned to layout 0 makes "the same physical key triggers the binding regardless of the active layout" the default behaviour** (can be disabled in configuration).
- Which binding receives the event when several match the same physical key is compositor policy (unspecified in the protocol). The library detects and warns about duplicate registrations of (keysym, mods, layout_override).
- [RESOLVED] modifiers_watch / modifiers_update are since-version-3 features in the river-xkb-bindings-v1 XML shipped by river 0.4.5. Watching modifier state makes alt-tab-style "commit on modifier release" UIs implementable. **The target river version is 0.4.5 or later.**

### 3.9 Input Device Configuration [RESOLVED: river-input-management-v1 (v1) read]

- The input_device event provides the type (keyboard / pointer / touch / tablet) and the name. The configuration API is a declarative map from device-name pattern to settings, applied automatically on hotplug.
- Settings offered here: assign_to_seat (unused under the single-seat policy), set_repeat_info (rate/delay, per keyboard), set_scroll_factor, map_to_output / map_to_rectangle (touch and tablet).
- [RESOLVED: river-libinput-config-v1 read] libinput details such as tap and acceleration live here. Design points:
  - river_libinput_device_v1 exists only for libinput devices (not every input device is one → applying inputRules must branch on whether the device exists).
  - Capability-discovery event pattern: `*_support` (once, right after creation) → `*_default` → `*_current` (on every change). **This lets us check support before applying inputRules and warn the user instead of silently swallowing unsupported settings.**
  - Each configuration request returns a river_libinput_result_v1 (success / unsupported / invalid). Surface the result in logs and over IPC.
  - Coverage is the full libinput configuration surface (send_events, tap-to-click, acceleration profiles; custom acceleration curves are possible via create_accel_config + set_points).
  - Binding implementation note: libinput's floating-point values are packed into Wayland array arguments as native-endian IEEE-754 doubles. Our own code generator (2.2) types this array as `Double`.
- Keymaps are set per device through river-xkb-config-v1 by passing an fd (create_keymap). Layouts can be switched programmatically with set_layout_index / set_layout_name, and active_layout / capslock / numlock state events are available (usable for a layout indicator in the bar).

## 4. Protocol Mapping (river-window-management-v1 v4, read)

Required functionality: river_window_manager_v1 **version 3** (depends on set_content_clip_box). River 0.4.5 advertises version 4, whose window identifier is used for hot-swap restoration.

### 4.1 xmonad Concept → Protocol

| xmonad concept | river protocol | Notes |
|---|---|---|
| Window | river_window_v1 | Unified abstraction over xdg_toplevel and Xwayland |
| Window position | river_node_v1.set_position | Global logical coordinates, negatives allowed. In the render sequence |
| Stacking order (raise, etc.) | river_node_v1.place_above/below/top/bottom | **Initial order is undefined** → the reconciler explicitly establishes the full order every time |
| Screen / rescreen | river_output_v1 (position/dimensions/removed) | The corresponding wl_output global name is also provided (usable for screencopy integration, etc.) |
| Keyboard focus | river_seat_v1.focus_window / clear_focus | In the manage sequence |
| Focus border | river_window_v1.set_borders | **Drawn by the compositor** (edge selection + width + RGBA; hidden while fullscreen). No drawing needed on the WM side |
| Keybindings | river-xkb-bindings-v1 (separate protocol) | Read; see 3.8 |
| Mouse bindings (mod+drag) | river_seat_v1.get_pointer_binding + op_start_pointer | See 3.5 |
| kill | river_window_v1.close → closed event | Delete from the StackSet on closed |
| Hiding (hidden workspace) | river_window_v1.hide / show | In the render sequence |
| fullscreen | river_window_v1.fullscreen (with output) / exit_fullscreen | Position, dimensions, and clipping while fullscreen are **compositor-managed**. On exit, propose + set_position are required in the same manage sequence |
| logHook → bar | river_shell_surface_v1 + external IPC | sync_next_commit allows drawing frame-synchronized with the WM's commits |
| XMonad.Prompt | river_shell_surface_v1 + focus_shell_surface | Keyboard focus can be moved to a shell surface, providing the protocol mechanism needed for a prompt |
| SSD/CSD control | use_ssd / use_csd + decoration_hint event | Default policy: send use_ssd and use only set_borders. Windows that are only_supports_csd are allowed to keep CSD |
| Decorations such as titlebars | river_decoration_v1 (above/below) | A non-goal in v1 (borders only). Recorded as the place future extensions would go |

### 4.2 Protocol Event → Core Event

| Event | Handling in the core |
|---|---|
| window (+app_id/title/parent/hints/identifier) | WindowCreated → ManageHook → insert / float / shift |
| closed | WindowClosed → StackSet.delete |
| dimensions | Update ActualState (reconciler only; does not affect the StackSet) |
| dimensions_hint | A hint to the layout (honoured under Tolerate) |
| pointer_enter / pointer_leave | Change focus only when FFM is enabled |
| window_interaction / shell_surface_interaction | click-to-focus, raise, handling clicks on the prompt |
| maximize/minimize/fullscreen/menu _requested family | Policy hooks (equivalent to xmonad's ClientMessage handling). Default: honour only fullscreen_requested, ignore the rest |
| pointer_move/resize_requested | Depending on configuration, float the window and start an op, or ignore |
| output / removed / position / dimensions | Equivalent to rescreen (apply the rules in 3.2) |
| session_locked / unlocked | Switch binding restrictions (minimize WM bindings while locked) |
| unavailable | Duplicate instance → report an error and exit |
| finished | Destroy all objects → clean exit |

### 4.3 Design Notes Drawn from Reading the Protocol

- **set_capabilities**: in final v1, declare only `fullscreen` and leave maximize / minimize / window_menu undeclared. Clients may use this information to omit the corresponding controls. The protocol default is "all supported", so the WM must send the intended capability set explicitly. M1 sends an empty capability set and ignores all corresponding client requests; M2 advertises `fullscreen` only when its fullscreen policy is implemented.
- **set_tiled**: `plan` computes which edges adjoin others within the tiling and sends this per window (suppressing CSD drop shadows). Added as a field of DesiredScene.
- **Type-level separation of the two phases**: window management state may only be changed inside a manage sequence, rendering state only inside manage/render, and violations are protocol errors (sequence_order). Split the `Protocol ()` of 3.3 into two monads, `ManageM` and `RenderM`, and expose each request through the phase or phases in which it is valid. This makes invalid phase-specific calls unrepresentable through these APIs; the lower-level binding and sequence state machine still require runtime validation.
- **The unresponsive error**: the compositor may disconnect a WM that fails to respond. Do no heavy work during manage/render: `step`/`plan` are lightweight pure computations, and hook IO (spawning processes, etc.) is deferred to asynchronous tasks outside the sequence, re-entering via manage_dirty on completion.
- **manage_dirty**: the entry point for state changes driven by external factors (IPC, timers, dbus). logHook external commands and scratchpad toggles come through here.

### 4.4 Protocol Coverage

| Protocol | Reflected in |
|---|---|
| river-window-management-v1 (v4; v3 functionality required) | 3.1 / 3.2 / 3.3 / 3.5 / 3.6 / 3.7 / 4.1–4.3 |
| river-xkb-bindings-v1 | 3.8 (requires river 0.4.5+) |
| river-layer-shell-v1 (v1) | 3.3 / 3.6 / 4.5 |
| river-input-management-v1 (v1) / river-xkb-config-v1 | 3.9 |
| river-libinput-config-v1 | 3.9 |

### 4.5 Handling layer-shell Delegation (river-layer-shell-v1 v1, read)

- **Binding the river_layer_shell_v1 global is itself the declaration of layer shell support.** If it is not bound, the compositor closes layer surfaces immediately. This WM always binds it (required for Waybar, fuzzel, lock screens, and so on to work).
- Per output (river_layer_shell_output_v1): the non_exclusive_area event (the avoidStruts equivalent from 3.3), and set_default (the default output for layer surfaces that do not specify one — make it follow the focused output).
- Per seat (river_layer_shell_seat_v1): focus_exclusive / focus_non_exclusive / focus_none (the focus arbitration in 3.6).

## 5. Configuration Model, Build, and Operation

### 5.1 Configuration = Program (the shape of the public API)

As in xmonad, there is no configuration file. The user writes a Haskell program importing this library. A minimal Main.hs:

```haskell
import Seki

main :: IO ()
main = seki def
  { modMask    = Mod4
  , terminal   = "foot"
  , workspaces = map show [1..9 :: Int]
  , layoutHook = tall ||| Mirror tall ||| Full
  , manageHook = composeAll
      [ appId =? Just "org.pulseaudio.pavucontrol" --> doFloat
      , isDialog                                   --> doFloat ]
  , keys       = defaultKeys
  }
  where tall = Tall 1 (3/100) (1/2)
```

Module layout (following xmonad's core / contrib split):

| Module | Contents |
|---|---|
| `Seki` | The front door, re-exporting everything needed for configuration. Users normally import only this |
| `Seki.Core.StackSet` | The pure data structure and operations from 3.1 |
| `Seki.Core.Layout` | LayoutClass, `\|\|\|`, Mirror, ModifiedLayout |
| `Seki.Core.Scene` | DesiredScene, plan |
| `Seki.Run.*` | ManageM / RenderM, reconciler, event loop (users normally do not touch these) |
| `Seki.Wire.*` | The wire protocol implementation and generated code (2.2) |
| `Seki.Layout.*` / `Seki.Hooks.*` / `Seki.Actions.*` | The contrib equivalent. Extensions get added here |

The configuration record in full:

```haskell
data Config l = Config
  { modMask         :: Mods
  , terminal        :: String
  , workspaces      :: [WorkspaceId]
  , layoutHook      :: l
  , manageHook      :: ManageHook          -- Query type: composed from appId / title / parent / isDialog etc.
  , keys            :: Config l -> Bindings  -- Map SubmapName (Map (Keysym, Mods) (Action ()))
  , pointerBindings :: Config l -> PointerBindings  -- mod+drag move / resize / float
  , logHook         :: Action ()           -- on every state change (IPC emission is built in; this is for extra work)
  , startupHook     :: Action ()
  , focusModel      :: FocusModel          -- ClickToFocus | FocusFollowsMouse
  , borders         :: BorderConfig        -- width, RGBA for focused / unfocused / urgent
  , bindingsUseFixedLayout :: Bool         -- layout_override from 3.8 (default True)
  , inputRules      :: [(DevicePattern, InputConfig)]  -- device settings from 3.9
  , sizePolicyDefault :: SizePolicy        -- default Crop | Tolerate from 3.3
  }

instance Default (Config StdLayouts)  -- def supplies the standard configuration
```

- `def` is intended to provide a usable baseline configuration. Users specify differences with a record update.
- The `Action` monad is the user-facing execution context (equivalent to xmonad's `X`). The internal two-phase `ManageM`/`RenderM` split (4.3) is fully hidden; once an `Action` completes, the framework runs step → plan → reconcile in one go. Main operations:
  - `windows :: (StackSet -> StackSet) -> Action ()` — the way pure operations are brought in (`windows (greedyView "3")` and the like)
  - `spawn :: String -> Action ()` — **asynchronous** process launch. As the countermeasure to the unresponsive error (4.3), no API is offered for synchronous heavy IO inside an `Action` (where needed, a continuation-style API such as `spawnAndRead` re-enters via manage_dirty)
  - `sendMessage :: Message a => a -> Action ()` — messages to layouts (Shrink / Expand / IncMaster)
  - `enterSubmap / exitSubmap`, `toggleScratchpad`, etc.
- An ExtensibleState equivalent (user state stored keyed by type) is also provided. Essential for porting the contrib culture.

### 5.2 Build, Startup, and Hot-Swap

- Location: `~/.config/seki/` is a cabal project (Main.hs + seki.cabal). A skeleton is generated automatically on first run.
- Startup: launch the WM binary from river's `~/.config/river/init` (a shell script). If the user configuration is unbuilt or fails to build, attempt to start the **default WM binary** shipped with the library as a fallback.
- Restart (the mod-q equivalent) flow:
  1. Run `cabal build` in a subprocess (the current WM keeps running throughout)
  2. Failure → the old binary continues; the error is sent to the bar over IPC and written in full to a log file
  3. Success → serialize the state from 3.7 (identifier→tag / floating / stack order / layout state / submap) to a file → request stop → receive finished → exec the new binary
  4. The new process reads the state file and matches it against the post-reconnect window enumeration using the version 4 identifiers from 3.7
  - During the handover gap river keeps the session and windows alive. Surviving windows recover their WM-owned state by identifier; newly created windows use initial-placement rules, and entries for windows closed during the gap are discarded.
- CLI: `seki --recompile`, `--check` (type-check the configuration only), `--restore <file>` (specify a state file).
- Experimenting with a switch to a different WM implementation (river's WM hot-swap) works through the same stop/exec path.

### 5.3 Bar Integration and External IPC

- The WM speaks JSON Lines over `$XDG_RUNTIME_DIR/seki/ipc.sock` (a UNIX domain socket).
  - **Subscribe (push)**: a state snapshot is broadcast on every state change. Contents: per-output workspace list (occupied / visible / focused / urgent), layout name, focused window title, submap name, xkb layout name (from active_layout in 3.9). The equivalent of xmonad's logHook + xmonad-log.
  - **Command (pull)**: operations from outside (`workspace 3`, `spawn ...`, `scratchpad term`, etc.). Received → manage_dirty → the `Action` runs inside a manage sequence, riding the re-entry path from 4.3.
- A bundled CLI, `sekictl`, makes this scriptable (our replacement for riverctl).
- Waybar is supported for now through a custom module (subscribing to the socket). Standard protocols such as ext-workspace need compositor-side implementation, so they are out of scope for v1; we will consider following along once river supports them.
- Writing our own bar (river_shell_surface_v1 + shm drawing) is optional and post-M3. We start out running an external bar.

### 5.4 Startup Sequence and Environment

- Daemons (fcitx5, mako, kanshi, etc.) are recommended to be started from river's init script (so they are unaffected by WM restarts). Only things that depend on WM state go in startupHook.
- The WM exports XCURSOR_THEME / XCURSOR_SIZE to child processes consistently with set_xcursor_theme (matching the note in 4.1).
- Hooking into a systemd user session (graphical-session.target) is optional; document both approaches.

## 6. Risks and Milestones

### 6.1 Risk Table

| Risk | Impact | Mitigation |
|---|---|---|
| river's bus factor of 1 and funding difficulties | Development halt, maintenance stagnation | The protocol is stable and MIT-licensed. Investment on the WM side carries over to any protocol-compatible compositor (a future wlroots-based implementation, say). Most of this WM's code (the pure core) is protocol-independent |
| Low-level correctness of the Haskell wire implementation (fd passing via SCM_RIGHTS, message framing) | Protocol bugs or disconnection | The completed M0 suite covers pure codecs, socket-pair transport, mock protocol behavior, and real river. The pinned real-river suite remains a release gate |
| A compositor disconnecting an unresponsive WM (4.3) | Losing the WM mid-session | Only pure computation inside sequences; IO re-enters via manage_dirty (enforced at the API level in 5.1). M1 measures sequence latency and GC pauses against seki-owned budgets, including evaluating the nonmoving GC. The pinned river 0.5.0 declares the protocol error but does not enforce a WM response timer, so no river threshold is claimed |
| Windows created or closed during WM handover (3.7) | Serialized and enumerated window sets differ | Match surviving windows by the version 4 identifier; apply normal placement to new windows and discard stale entries |
| shm drawing ecosystem (bar, prompt) | M3 effort | Avoided for now by using an external bar (Waybar). A dmenu-family replacement lets us start using the prompt too |
| river's IM (text-input / input-method) compatibility | Japanese input via fcitx5 | [RESOLVED] M0 validated an exact `日本語` commit through river 0.4.6, foot, fcitx5, and Mozc; the scenario remains in the release suite |
| modifiers_watch version (3.8) | alt-tab-style UI | [RESOLVED] Confirmed as added in 0.4.5. Resolved by targeting river 0.4.5+ |

### 6.2 Milestones

| Milestone | Status | Contents | Acceptance criteria |
|---|---|---|---|
| M0 Validation | **Complete** | Haskell wire protocol implementation without libwayland; own XML code generator; connection and window/output event handling; Full placement; terminal and close bindings; Japanese input and Xwayland validation | Completed on nested river, including exact fcitx5/Mozc input and the real-river release suite |
| M1 Core | **Next** | StackSet port + QuickCheck properties, Tall / Mirror / Full / `\|\|\|`, greedyView, focus movement, swapMaster, borders, non_exclusive_area, the two-phase monads (ManageM/RenderM) | Tiling operations follow the intended xmonad semantics. Sequence latency and GC pauses meet the documented seki budgets; see the [M1 implementation plan](m1-plan.md) |
| M2 Practical | Planned | Floating (mod+drag), ManageHook, fullscreen, submaps, hot-swap + state restoration, inputRules, Crop/Tolerate | The main parts of my own xmonad.hs can be ported |
| M3 Daily use | Planned | Bar IPC + `sekictl`, scratchpad, urgent handling, documentation and configuration skeleton | **Full migration from xmonad; used for daily work** |
| M4 onwards | Planned | Prompt (shell surface + shm drawing), the contrib-equivalent extension set, IndependentScreens, further protocol integrations | — |

### 6.3 Resolved Questions and Follow-up

The following investigations informed the accepted design:

1. [RESOLVED] river's IM support → the M0 release suite verified an exact
   Japanese commit through fcitx5 and Mozc.
2. [RESOLVED] river-libinput-config-v1 → reflected in 3.9.
3. [RESOLVED] modifiers_watch / update → reflected in 3.8. **Required river version: 0.4.5+**.
4. [RESOLVED] Xwayland → the M0 release suite verified Xterm management,
   focus, and close behavior. Deployment packages must still enable river's
   Xwayland support.
5. [RESOLVED] The project name is **seki** (堰, a weir): a structure that controls the flow of a river. Derived names: package `seki` / modules `Seki.*` / binary `seki` / CLI `sekictl` / socket `$XDG_RUNTIME_DIR/seki/ipc.sock`.
   - Collision check (as of 2026-07): no collisions on Hackage or in the Wayland/WM space. Same-name packages in other registries are in distant fields and small (crates.io: a log-analysis CLI; npm: dormant since 2022; PyPI: a security tool). Same-name projects on GitHub are likewise small (a Go/baduk library and similar).
   - Follow-up: repeat the name check before publishing to Hackage
     (`seki-wm` remains the fallback) and when preparing distribution packages.
