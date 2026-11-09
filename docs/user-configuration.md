# User configurations

Install the flake's `config-delivery` output and retain its Nix root while using
compiled configurations. `bin/seki` is the product launcher. `bin/seki-default`
is the distinct shipped default runtime; `seki-config-env` provisions build tools.
`bin/sekictl` is the IPC client that `sekictl recompile` and the Waybar module use.
First acquisition requires Nix network access and trusted substitutes or a build.
User executables depend dynamically on that retained closure.

Run `seki --check` to create missing files in `$XDG_CONFIG_HOME/seki` (default
`~/.config/seki`), build the project offline, resolve `exe:seki-user` with Cabal
`list-bin`, copy it to a private generation, and invoke that exact copy with
`--check`. Existing files, including partial files, are preserved. Correct partial
files manually using the shipped `examples/config` templates. The generated
project quotes the absolute archive path, binds the absolute compiler path, and
uses installed dependencies.
It does not search for a source checkout or download Hackage packages.

Check exits zero only when candidate policy validation succeeds. It never selects
a startup profile, opens Wayland, runs library startup/manage hooks, consumes
restore state, or activates a session. Haskell configuration is trusted code;
this check is not a sandbox. Compilation has no deadline. Candidate checks have
a ten-second deadline and a 1 MiB output threshold, sampled every 10 ms. Cabal
`list-bin` has a thirty-second deadline and a 64 KiB output threshold. Child
process groups are killed after completion or refusal. Output goes directly to
private log files; failure diagnostics read at most the last 8 KiB. Build logs
are retained in `$XDG_STATE_HOME/seki` (default `~/.local/state/seki`). Failed
builds and unused checked copies are retained for diagnosis; remove unused files
only when no session or profile references them.

Run `seki` for initial startup. Only a successfully built and checked generation
can replace the stable `profile` symlink in the state directory. The launcher
execs that link, preserving its path as runtime argv0 for M2 restart. Cabal cannot
overwrite the copied generation. An immediate exec failure restores the previous
profile. Initial configuration, compiler, or state failures print a diagnostic
and attempt `seki-default`. `--check` fails without starting fallback. A recorded
`seki --restore FILE` invocation executes the existing profile with exactly
`--restore FILE` and does not generate, compile, or check anything.

Run `seki --recompile`, `sekictl recompile`, or the configured restart action
(default Mod-q) to rebuild a running installed session. The CLI sends a request
to the existing IPC owner in its runtime directory. Focus and layout actions
continue while the session builds and checks a private candidate. One build runs
at a time, with one queued rebuild. Further requests coalesce and invalidate the
earlier result. Failed builds leave the process and profile in place.

IPC snapshots report build state, error, and log path. The command receipt means
the owner applied the request, not that compilation or activation finished. A
changed valid candidate uses the existing stop/finished handover. State is saved
before the profile changes. Immediate exec failure rolls back the profile and
reconnects with the old in-memory configuration and saved window state. A fresh
managed IPC session identifies the new image. See [IPC](ipc.md) for the status
contract. Configuration and hooks are trusted code; checking does not provide
recovery from arbitrary crashes after exec.

Direct development runtimes without `SEKI_CONFIG_ARCHIVE` retain M2's recorded
target restart behavior. Their IPC owner refuses recompile requests. Do not use
a second launcher invocation to change a running session.

For direct development runtime use `cabal run exe:seki-default`; public user
programs continue to call `Seki.seki config`. The direct launcher requires explicit
absolute `SEKI_CONFIG_ARCHIVE`, `SEKI_CONFIG_GHC`, `SEKI_CONFIG_CABAL`,
`SEKI_CONFIG_ENV`, `SEKI_CONFIG_TEMPLATES`, and `SEKI_CONFIG_DEFAULT` resources.
The installed Nix wrapper supplies these immutable paths. The launcher supplies a private Cabal config, cache and store under its state
directory. Direct developer runs can explicitly select an absolute
`SEKI_CONFIG_CABAL_CONFIG`; the installed wrapper clears this override. Developer
cached Cabal environments are separate evidence from the installed provisioned path.

The `Installed configuration smoke` CI job builds `.#config-delivery` and runs
`ci/check-product.py` against it without a development shell. The script uses the
installed `seki` from a fresh home and state directory. It builds the shipped
skeleton offline, repeats the unchanged build, checks a valid and an invalid
configuration, checks the compiler, state, and restore failure paths, and starts
both the default runtime fallback and the built configuration against a sentinel
Wayland socket.

## Start from the daily-use configuration

`dailyConfig` in `test-public/PortedConfig.hs` is the maintainer's daily-use
configuration. It extends the M2 port `portedConfig` with Waybar, swaybg,
swayidle and swaylock, fcitx5, an alacritty scratchpad on Mod-grave, and input
rules for keyboards and one touchpad, which the rule matches as a pointer.
`test-public/DailyConfigMain.hs` runs it
with the keymap at `$XDG_CONFIG_HOME/seki/keymap.xkb`. When `XDG_CONFIG_HOME`
is unset or relative, the keymap is `~/.config/seki/keymap.xkb`.

The configuration runs `waybar`, `swaybg`, `swayidle`, `swaylock`, `fcitx5`,
`alacritty`, `amixer`, and `brightnessctl`. It also names the maintainer's
wallpaper and touchpad. Install those programs, or change the commands, the
wallpaper path, and the touchpad name in your copy. Automatic client urgency is
unavailable, so the configuration has no urgent border or bar flag.

To start from it, run these steps from a seki checkout:

1. Build the installed product and put its `bin` directory on `PATH`:

   ```sh
   nix build --no-write-lock-file .#config-delivery -o ~/.local/share/seki-delivery
   export PATH="$HOME/.local/share/seki-delivery/bin:$PATH"
   ```

2. Compile the keymap from your XKB settings. These are the maintainer's
   settings:

   ```sh
   mkdir -p ~/.config/seki
   xkbcli compile-keymap --rules evdev --model pc105 --layout us --options ctrl:nocaps > ~/.config/seki/keymap.xkb
   ```

3. Install the Waybar module. It runs `sekictl subscribe` from `PATH`.

   ```sh
   mkdir -p ~/.config/waybar
   cp examples/waybar/config.jsonc examples/waybar/seki.py ~/.config/waybar/
   ```

4. Run `seki --check` once. It creates `Main.hs`, `seki-user.cabal`, and
   `cabal.project` in `~/.config/seki`.
5. Copy `test-public/PortedConfig.hs` into `~/.config/seki/`, and copy
   `test-public/DailyConfigMain.hs` over `~/.config/seki/Main.hs`.
6. Add `other-modules: PortedConfig` to the `executable seki-user` stanza of
   `~/.config/seki/seki-user.cabal`.
7. Run `seki --check` again. It exits 0 when the configuration builds and
   passes its policy check.
8. Start `seki` from river's init script, as the [README](../README.md)
   describes.

To change the configuration later, edit `~/.config/seki/PortedConfig.hs` and
run `seki --recompile`, or press Mod-q.
