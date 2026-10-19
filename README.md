# seki

A tiling window manager for [river](https://codeberg.org/river/river), written
in Haskell and aiming for xmonad's semantics and configuration model.

> **Status: M1 Core is complete.** See the [milestones](docs/spec.md#62-milestones)
> and the [latency and GC report](docs/latency-baseline/report.md).

## Current status

River 0.4 split the compositor from the window manager. The compositor handles DRM/KMS, rendering, input devices, and Xwayland, while delegating window-management policy to a separate process over the `river-window-management-v1` protocol. That process is an ordinary Wayland client and can be written in any language with suitable protocol bindings.

seki is that process, and it does not link to `libwayland`. The current
implementation provides:

- a native Haskell Wayland transport, connection layer, and XML binding
  generator;
- typed river startup, window/output/seat inventory, and manage/render phase
  handling;
- xmonad-style workspaces shared across outputs, with `greedyView` switching
  and window shifting;
- the Tall, Mirror Tall, and Full layouts, with focus movement, master
  swapping, and master resize and count messages;
- compositor-drawn focus borders and click-to-focus;
- layer-shell support, with tiling confined to each output's non-exclusive
  area; and
- default key bindings, described [below](#project-direction).

The release integration suite exercises the native Wayland path, two outputs,
Xwayland, and an exact Japanese input commit through fcitx5 and Mozc. Ten M1
scenarios check captured geometry, borders, focus, workspace visibility,
native input, layer exclusive zones and layer focus, and complete headless
output recovery. See the
[integration testing manual](docs/integration-testing.md) for reproduction and
maintenance instructions.

## Project direction

seki is intended for xmonad users moving from X11. The accepted v1 design has
these goals:

- **Your configuration is the window manager.** As in xmonad, there is no config file. You write a Haskell program that imports `Seki`; `def` is intended to provide a usable default configuration.
- **The core is pure.** `StackSet`, the layout algebra (`|||`, `Mirror`, layout modifiers), and `ManageHook` are based on xmonad, with its semantics and QuickCheck properties retained where applicable. Layouts and actions are pure. Policy owns mutable state and launches effects; the runtime owns connection IO.
- **Workspaces use xmonad-style semantics.** They form a shared pool across outputs, with `greedyView` as the default. River itself has no workspace abstraction, so seki implements this policy in the window manager.

A configuration program imports `Seki` and runs `seki def`. The current M1 API supports these overrides.

```haskell
import Seki

main :: IO ()
main = seki def
  { modMask    = Mod4
  , terminal   = "foot"
  , workspaces = map show [1..9 :: Int]
  , layoutHook = tall ||| Mirror tall ||| Full
  , keys       = defaultKeys
  }
  where tall = Tall 1 (3/100) (1/2)
```

The default modifier is Mod4. Mod-Shift-Return launches `terminal`, and
Mod-Shift-c closes the focused window. Mod-Space cycles layouts. Mod-j and
Mod-k move focus, and Mod-Return swaps the focused window with the master.
Mod-h and Mod-l resize the master area. Mod-comma and Mod-period change the
master count. Mod-t tiles a floating window again. Mod-1 through Mod-9
select workspaces with `greedyView`; adding Shift moves the focused window
there.

`def`'s `manageHook` is `defaultManageHook`, which floats dialogs: the windows
whose client set a parent. A floating window sits above every tiled window
and keeps the size its client picks, centered on its output. `float` and
`sink` float and tile a window, so `withFocused float` floats the focused one.
`doFloat` floats a new window from a manage hook.

A client's fullscreen request makes it cover its workspace's output above
every other window, using river's native fullscreen. Only a workspace's
focused window can be fullscreen: focusing another window on that workspace
ends it, and switching workspaces does not. `withFocused toggleFullscreen`
does the same from a key binding.

`keys` receives the final configuration, so changing `modMask`, `terminal`,
or `workspaces` also changes `defaultKeys`. Bindings use XKB keysym numbers,
with `Keysym 0xff0d` representing Return. Combine `Mods` with `(.|.)`, which
`Seki` exports. River excludes locked modifiers, so bit 16 and unknown modifier
bits fail validation before connection. Named modifiers are `Shift`, `Control`,
`Mod1`, `Mod3`, `Mod4`, and `Mod5`. `bindingsUseFixedLayout = True`
matches group zero regardless of the active keyboard layout. A binding may
have no modifier, as in `KeyBinding (Mods 0) (Keysym 0x1008FF13)` for a
volume key.

`additionalKeys` and `removeKeys` follow xmonad-contrib's EZConfig. A binding
added with `additionalKeys` replaces one with the same identity, and both keep
the final-configuration behavior of `keys`. `startupHook` runs once per seki
process when the first output arrives. Use `spawnOnce` in it for programs
that must start only once.

When changing the layout's type, replace the layout-dependent key function,
manage hook, and startup hook in the same record update, as in
`def{layoutHook = Full, keys = defaultKeys, manageHook = defaultManageHook, startupHook = pure ()}`.
`defaultManageHook` names no layout, so it composes with a hook of your own,
as in `composeAll [appId =? "mpv" --> doShift "2", defaultManageHook]`.
Custom bindings are an association list of `(KeyBinding, Action l ())` pairs.
Duplicate identities, empty workspace lists or duplicate tags, negative borders,
and colors whose RGB channels exceed alpha fail before Wayland startup.
`BorderColor` channels are unsigned 32-bit premultiplied RGBA values.

`Action` is pure. Use `windows`, `sendMessage`, `spawn`, `spawnOnce`, `kill`,
`float`, `sink`, `toggleFullscreen`, and `withFocused` to change state or record effects. Arbitrary IO is not part of the configuration API.
Import `Seki.Core.StackSet` qualified for its full operation set; `Seki`
exports selected operations without conflicting with `Config.workspaces`.

The pure configuration lives in `seki-core`. The private `seki-wire`
component lowers keys and styles into the production policy. The public
library depends on both, and the executable imports only `Seki`.

## Architectural consequences

Splitting the WM from the compositor changes the behavior of restarts, failures, and input processing:

- **The WM can exit or crash without ending the session.** River keeps the
  compositor and windows alive, and starting a window manager again restores
  active management. M2 will use river's version 4 window identifier to
  restore WM-owned state across a seki restart.
- **Ordinary client input remains compositor-side.** Keybinding matching also happens in the compositor, and pointer motion alone does not wake the WM. Actions handled by the WM can still be delayed by a GC pause, so pause duration remains an implementation concern.
- **Types represent the protocol phases.** River separates window-management state from rendering state and rejects requests made in the wrong phase. `ManageM` and `RenderM` make many invalid phase-specific calls unrepresentable in the higher-level API.

## Requirements

- **river 0.4.5 or later** (0.4.5 added the modifier-watching requests seki uses)
- GHC 9.6.6 and cabal-install 3.10 or later
- No `libwayland` dependency: the Wayland wire protocol is implemented in Haskell, with a code generator for the protocol XML

## Development

The development toolchain is GHC 9.6.6, cabal-install 3.10 or later,
and Haskell Language Server. Install those with your system package manager or
[GHCup](https://www.haskell.org/ghcup/).

Build and test the project with:

```console
cabal build all
cabal test all
```

Check that the source distribution ships every module once and that the public
Haddock index documents every public module with:

```console
ci/check-package.sh
```

Run the release integration check, including exact manifest validation, from
the pinned Nix environment with:

```console
nix develop --no-write-lock-file .#ci --command ci/run-integration.sh
```

The command writes a completion manifest and retains the final 1 MiB of its
log beneath `artifacts/integration/`. The temporary raw log grows during the run.

Format and lint Haskell sources with:

```console
fourmolu --mode inplace src src-core src-public test test-public tools integration
hlint src src-core src-public test test-public tools integration
```

With a Wayland compositor running, print the synchronized global registry with:

```console
cabal run seki-registry-smoke
```

Run the default configuration against the display selected by the standard
`WAYLAND_DISPLAY`/`XDG_RUNTIME_DIR` environment variables with:

```console
cabal run seki
```

Validate the pinned runtime protocol set and regenerate the committed runtime
bindings with the commands below. The integration guide covers the
[fixture bindings](docs/integration-testing.md#layer-surface-fixture).

```console
cabal run seki-scanner -- validate protocol/wayland/wayland.xml protocol/river/river-window-management-v1.xml protocol/river/river-xkb-bindings-v1.xml protocol/river/river-layer-shell-v1.xml
cabal run seki-scanner -- generate Seki.Protocol.Wayland protocol/wayland/wayland.xml 'Wayland 1.25.0, commit 3e673a438b0a9749e3bdf5cac4befac86333024c' src-generated/Seki/Protocol/Wayland.hs protocol/wayland/wayland.xml
cabal run seki-scanner -- generate Seki.Protocol.RiverWindowManagementV1 protocol/river/river-window-management-v1.xml 'river v0.4.5, commit f6d961773711dc29d3eb5b60bd14744a6a33894f' src-generated/Seki/Protocol/RiverWindowManagementV1.hs protocol/river/river-window-management-v1.xml protocol/wayland/wayland.xml protocol/river/river-xkb-bindings-v1.xml
cabal run seki-scanner -- generate Seki.Protocol.RiverXkbBindingsV1 protocol/river/river-xkb-bindings-v1.xml 'river v0.4.5, commit f6d961773711dc29d3eb5b60bd14744a6a33894f' src-generated/Seki/Protocol/RiverXkbBindingsV1.hs protocol/river/river-xkb-bindings-v1.xml protocol/wayland/wayland.xml protocol/river/river-window-management-v1.xml
cabal run seki-scanner -- generate Seki.Protocol.RiverLayerShellV1 protocol/river/river-layer-shell-v1.xml 'river v0.4.5, commit f6d961773711dc29d3eb5b60bd14744a6a33894f' src-generated/Seki/Protocol/RiverLayerShellV1.hs protocol/river/river-layer-shell-v1.xml protocol/wayland/wayland.xml protocol/river/river-window-management-v1.xml
```

## Scope

Delegated to river: rendering and compositing, DRM/KMS, input device plumbing, Xwayland, screen capture, session locking, and color management.

Non-goals for seki itself: animations and window effects, multi-seat (v1 is single-seat), decorations beyond borders.

## Documentation

- [`docs/spec.md`](docs/spec.md) — the design specification: pure core, protocol mapping, configuration model, risks, and milestones.
- [`docs/m1-plan.md`](docs/m1-plan.md) — the M1 core milestone's scope,
  design decisions, work packages, exit criteria, and regression record.
- [`docs/m2-plan.md`](docs/m2-plan.md) — the M2 practical milestone's scope,
  decided design issues, work packages, and exit criteria.
- [`docs/wire-protocol.md`](docs/wire-protocol.md) — the native Wayland transport, XML binding generator, and current runtime.
- [`docs/reconciliation.md`](docs/reconciliation.md) — how layout selections
  become shown, sized, and focused windows across river's manage and render
  sequences.
- [`docs/integration-testing.md`](docs/integration-testing.md) — how to run,
  diagnose, and maintain the isolated real-river integration suite.
- [`docs/latency.md`](docs/latency.md) — how to measure sequence latency and
  GC pauses, and how to read the
  [reference report](docs/latency-baseline/report.md).

## Related projects

- [xmonad](https://xmonad.org/), whose StackSet and layout algebra this is a port of.
- [river](https://codeberg.org/river/river) and its window-management protocol, without which this project would be a compositor instead.

## License

MIT. See [`LICENSE`](LICENSE).

Vendored protocol definitions retain their upstream MIT notices; see
[`THIRD_PARTY_NOTICES.md`](THIRD_PARTY_NOTICES.md).
