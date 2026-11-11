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
`--check`. Existing files, including partial files, are preserved. Correct
partial files manually using the shipped `examples/config` templates. The
generated project quotes the absolute archive path, binds the absolute compiler
path, and uses installed dependencies. Each build points an existing
`cabal.project` at the installed archive and compiler, so a rebuilt delivery
reaches the configuration. It replaces a quoted path to a
`seki-<version>.tar.gz` file and a `with-compiler` path with the installed
compiler's file name, and keeps every other byte. It does not search for a source checkout or download Hackage
packages.

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

The installed wrapper exports `SEKI_CONFIG_LAUNCHER`, the path that `seki` was
started through. That path is usually a link, and a rebuilt delivery replaces
the target of the link. A session runs that path with `--resources` when the
session starts and before each build. `seki --resources` prints the archive,
the compiler, Cabal, the build environment, and the templates of its delivery,
one `SEKI_CONFIG_<NAME>=<path>` line each. The build uses the answer, so a
recompile after a delivery rebuild builds against the new seki library. The
first line of each build log names the archive of that build. The query has a
thirty-second deadline and a 64 KiB output threshold.

When the launcher does not answer, the build uses the resources that the
session runs. The `warning` in the IPC build status, seki's output, and the
build log give the reason, and the output of the failed query stays in a
`resources*.log` file in the state directory. The `warning` also appears while
the installed delivery ships other resources than the session runs, for
example after a recompile that failed against a new delivery. A restart passes
the resources of the new image to it as its `SEKI_CONFIG_*` variables, so those
variables name the seki library that the session runs.

A session that started from a delivery without `SEKI_CONFIG_LAUNCHER` runs a
seki library that does not ask the launcher. Start seki again once after you
install the first delivery that exports the variable. A launcher from an older
delivery answers `--resources` with its usage line. A newer session then keeps
its own resources and warns.

A recompile that follows a rebuilt delivery hands the saved session state to
the new seki library. If the new library cannot read the version of that
state, it logs the reason and starts without the state. The windows stay open,
and seki manages them as new windows.

Direct development runtimes without `SEKI_CONFIG_ARCHIVE` retain M2's recorded
target restart behavior. Their IPC owner refuses recompile requests. Do not use
a second launcher invocation to change a running session.

For direct development runtime use `cabal run exe:seki-default`; public user
programs continue to call `Seki.seki config`. The direct launcher requires explicit
absolute `SEKI_CONFIG_ARCHIVE`, `SEKI_CONFIG_GHC`, `SEKI_CONFIG_CABAL`,
`SEKI_CONFIG_ENV`, `SEKI_CONFIG_TEMPLATES`, and `SEKI_CONFIG_DEFAULT` resources.
The installed Nix wrapper supplies these immutable paths. A direct run without
`SEKI_CONFIG_LAUNCHER` builds with the resources in its environment. The launcher supplies a private Cabal config, cache and store under its state
directory. Direct developer runs can explicitly select an absolute
`SEKI_CONFIG_CABAL_CONFIG`; the installed wrapper clears this override. Developer
cached Cabal environments are separate evidence from the installed provisioned path.

The `Installed configuration smoke` CI job builds `.#config-delivery` and runs
`ci/check-product.py` against it without a development shell. The script uses the
installed `seki` from a fresh home and state directory. It builds the shipped
skeleton offline, repeats the unchanged build, checks a valid and an invalid
configuration, checks the compiler, state, and restore failure paths, and starts
both the default runtime fallback and the built configuration against a sentinel
Wayland socket. It also checks that the wrapper exports the path it was
started through, and that `seki --resources` through that path names the
archive of the delivery.

CI does not run `ci/check-delivery-upgrade.py`, because it needs river and two
`config-delivery` outputs that ship different archives. To check an upgrade in
a real session, build the two outputs and run
`python3 ci/check-delivery-upgrade.py FIRST SECOND`. The script starts a
session from the first output behind a link, points the link at the second
output, and runs `seki --recompile`. It passes when the new profile loads the
seki library that Cabal built from the second archive.

## Start from the daily-use configuration

`dailyConfig` in `test-public/PortedConfig.hs` is the maintainer's daily-use
configuration. It extends the M2 port `portedConfig` with Waybar, swaybg,
swayidle and swaylock, fcitx5, an alacritty scratchpad on Mod-grave, and input
rules for keyboards and one touchpad, which the rule matches as a pointer. It
also binds xmonad defaults that `defaultKeys` lacks: Mod-p runs a launcher,
Mod-Shift-j and Mod-Shift-k swap windows, and Mod-Tab and Mod-Shift-Tab cycle
focus.
`test-public/DailyConfigMain.hs` runs it
with the keymap at `$XDG_CONFIG_HOME/seki/keymap.xkb`. When `XDG_CONFIG_HOME`
is unset or relative, the keymap is `~/.config/seki/keymap.xkb`.

The configuration runs `waybar`, `swaybg`, `swayidle`, `swaylock`, `fcitx5`,
`alacritty`, `wmenu-run` (or `dmenu_run`), `amixer`, and `brightnessctl`. It
also names the maintainer's wallpaper, touchpad, and terminal font size. Install
those programs, or change the commands, the wallpaper path, and the touchpad
name in your copy. Automatic client urgency is unavailable, so the configuration
has no urgent border or bar flag.

To start from it, run these steps from a seki checkout:

1. Build the installed product and put its `bin` directory on `PATH`. The
   Nix daemon must be running. If your `nix.conf` does not enable flakes, add
   `--extra-experimental-features 'nix-command flakes'` to `nix build`.

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
8. Start `seki` from river's init script, `~/.config/river/init`, in a loop,
   as the [README](../README.md) describes. A display manager starts river
   without your shell's environment, so the script sets `PATH` itself. This is
   the maintainer's script. The input method variables are for fcitx5, and the
   log file keeps seki's output across restarts:

   ```sh
   #!/bin/sh
   export PATH="$HOME/.local/share/seki-delivery/bin:$PATH"
   export GTK_IM_MODULE=fcitx QT_IM_MODULE=fcitx XMODIFIERS=@im=fcitx
   log="${XDG_STATE_HOME:-$HOME/.local/state}/seki/session.log"
   mkdir -p "${log%/*}"
   while true; do seki >> "$log" 2>&1; sleep 1; done &
   ```

   Make the script executable. A display manager such as LightDM lists river's
   session file, `/usr/share/wayland-sessions/river.desktop`. Choose that
   session to start river, and river runs the script.

To change the configuration later, edit `~/.config/seki/PortedConfig.hs` and
run `seki --recompile`, or press Mod-q.

After you rebuild `config-delivery`, press Mod-q or run `seki --recompile`.
The session builds the configuration against the new delivery and restarts
into it. To see the archive that the next build uses, run `seki --resources`.
If the session started from a delivery that does not export
`SEKI_CONFIG_LAUNCHER`, start seki again once, for example by logging out and
back in.

To log out, press Mod-Shift-q. river exits and stops the init script, loop
included.
