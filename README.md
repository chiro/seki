# seki

A tiling window manager for [river](https://codeberg.org/river/river), written in Haskell — with xmonad's semantics and xmonad's configuration model.

> **Status: M0 validated; M1 core development is next.**
> This is not ready for daily use; see [Roadmap](#roadmap).

## What this is

River 0.4 split the compositor from the window manager. The compositor handles DRM/KMS, rendering, input devices, and Xwayland, while delegating window-management policy to a separate process over the `river-window-management-v1` protocol. That process is an ordinary Wayland client and can be written in any language with suitable protocol bindings.

seki is a Haskell implementation of that process, intended for xmonad users moving from X11:

- **Your configuration is the window manager.** As in xmonad, there is no config file. You write a Haskell program that imports `Seki`; `def` is intended to provide a usable default configuration.
- **The core is pure.** `StackSet`, the layout algebra (`|||`, `Mirror`, layout modifiers), and `ManageHook` are based on xmonad, with its semantics and QuickCheck properties retained where applicable. Layout is a pure function; only the reconciler touches IO.
- **Workspaces use xmonad-style semantics.** They form a shared pool across outputs, with `greedyView` as the default. River itself has no workspace abstraction, so seki implements this policy in the window manager.

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

## Consequences of this architecture

Splitting the WM from the compositor changes the behavior of restarts, failures, and input processing:

- **The WM can restart without ending the session.** River keeps the session and windows alive during a restart. The version 4 window identifier lets seki restore state for surviving windows across a WM reconnect; windows created during the handover use normal initial-placement rules.
- **A WM crash does not terminate the session.** Existing windows remain in river, and starting a window manager again restores active management.
- **Ordinary client input remains compositor-side.** Keybinding matching also happens in the compositor, and pointer motion alone does not wake the WM. Actions handled by the WM can still be delayed by a GC pause, so pause duration remains an implementation concern.
- **Types represent the protocol phases.** River separates window-management state from rendering state and rejects requests made in the wrong phase. `ManageM` and `RenderM` make many invalid phase-specific calls unrepresentable in the higher-level API.

## Requirements

- **river 0.4.5 or later** (0.4.5 added the modifier-watching requests we need)
- GHC 9.6.6 and cabal-install 3.10 or later
- No `libwayland` dependency: the Wayland wire protocol is implemented in Haskell, with a code generator for the protocol XML

## Development

The initial development toolchain is GHC 9.6.6, cabal-install 3.10 or later,
and Haskell Language Server. Install those with your system package manager or
[GHCup](https://www.haskell.org/ghcup/).

Build and test the project with:

```console
cabal build all
cabal test all
```

Format and lint Haskell sources with:

```console
fourmolu --mode inplace src test tools
hlint src test tools
```

With a Wayland compositor running, print the synchronized global registry with:

```console
cabal run seki-registry-smoke
```

Run the current Full-layout policy against the display selected by the standard
`WAYLAND_DISPLAY`/`XDG_RUNTIME_DIR` environment variables with:

```console
cabal run seki
```

The current defaults use Mod4-Shift-Return to launch `foot` and Mod4-Shift-c to
close the active window.

Validate the pinned protocol set and regenerate the committed bindings with:

```console
cabal run seki-scanner -- validate protocol/wayland/wayland.xml protocol/river/river-window-management-v1.xml protocol/river/river-xkb-bindings-v1.xml
cabal run seki-scanner -- generate Seki.Protocol.Wayland protocol/wayland/wayland.xml 'Wayland 1.25.0, commit 3e673a438b0a9749e3bdf5cac4befac86333024c' src-generated/Seki/Protocol/Wayland.hs protocol/wayland/wayland.xml
cabal run seki-scanner -- generate Seki.Protocol.RiverWindowManagementV1 protocol/river/river-window-management-v1.xml 'river v0.4.5, commit f6d961773711dc29d3eb5b60bd14744a6a33894f' src-generated/Seki/Protocol/RiverWindowManagementV1.hs protocol/river/river-window-management-v1.xml protocol/wayland/wayland.xml protocol/river/river-xkb-bindings-v1.xml
cabal run seki-scanner -- generate Seki.Protocol.RiverXkbBindingsV1 protocol/river/river-xkb-bindings-v1.xml 'river v0.4.5, commit f6d961773711dc29d3eb5b60bd14744a6a33894f' src-generated/Seki/Protocol/RiverXkbBindingsV1.hs protocol/river/river-xkb-bindings-v1.xml protocol/wayland/wayland.xml protocol/river/river-window-management-v1.xml
```

## Scope

Delegated to river: rendering and compositing, DRM/KMS, input device plumbing, Xwayland, screen capture, session locking, and colour management.

Non-goals for seki itself: animations and window effects, multi-seat (v1 is single-seat), decorations beyond borders.

## Documentation

- [`docs/spec.md`](docs/spec.md) — the design specification: pure core, protocol mapping, configuration model, risks, and milestones.
- [`docs/wire-protocol.md`](docs/wire-protocol.md) — the M0 design for the native Wayland transport and XML binding generator.

## Related projects

- [xmonad](https://xmonad.org/), whose StackSet and layout algebra this is a port of.
- [river](https://codeberg.org/river/river) and its window-management protocol, without which this project would be a compositor instead.

## License

MIT. See [`LICENSE`](LICENSE).

Vendored protocol definitions retain their upstream MIT notices; see
[`THIRD_PARTY_NOTICES.md`](THIRD_PARTY_NOTICES.md).
