# M4 First public release implementation plan

Status is **In progress**. Written 2026-11-20 against `9ae334d` and revised 2026-11-22 against `d1dc31d` after two reviews. The operator gave the go on 2026-11-22.
M4 makes seki ready for a first public release, version 0.1.0, for people other than the maintainer.
The operator set the goal on 2026-11-20 and the release scope on 2026-11-22. 0.1.0 ships through the Nix flake and a GitHub release, under the name `seki`.
M4 adds a managed first start (#90), binding repeat, submaps, a configurable focus model, and a first contrib-equivalent set.
Per-output workspaces, the prompt, Hackage, a plain-cabal install, and automatic client urgency stay out of M4.
M4 is complete when the operator publishes the `v0.1.0` GitHub release, and a clean account installs it from the flake and runs it on real river using only the README.
The packages run in order from M4-01 to M4-08, one after another.

## How to read this

One box is one unit of work. Every box names the evidence that checks it. A nested box is a sub-step of the box above it.
Check a box only when its evidence exists, such as a file, a log line, a screenshot, a test run, or a SHA. The body is a how-to. The appendices explain and record.
Package IDs name work packages, not GitHub PR numbers. Design gates are named M4-D01 to M4-D07 and are listed in [Appendix A](#appendix-a-prototype-evidence).
Read current behavior in [the specification](spec.md), [the M3 plan](m3-plan.md), and [integration-testing.md](integration-testing.md).

The program runs `skills/poteto-mode/playbooks/autopilot-stack.md` from the installed plugin.
The operator merges every PR. Every owner stops at merge-ready. Work runs serially in one linear stack, because almost every package edits `src-public/Seki.hs`, `src-core/Seki/Internal/Config.hs`, `src/Seki/Run/Policy.hs`, and `seki.cabal`.
The repository has `delete_branch_on_merge` off. Before the operator merges a parent, retarget its child to `main` after the restack, and check every child's `baseRefName`.

In this plan, "trunk" and "parent" both mean the head of the package before the one under test. For M4-01 that is `main`. Every regression lane and every perf baseline runs at the parent head, because `main` may not yet hold the packages below it in the stack.

Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

Every new budget below is a proposed acceptance target, not a measurement. The existing budgets from [latency.md](latency.md) stay in force. They are pure p99 below 2 ms, manage and render sequence p99 below 10 ms, maximum stop-the-world pause below 10 ms, and M3 wake, IPC, and toggle p99 below 50 ms.
Every package uses one regression margin. Take the median, across rounds, of each revision's manage and render sequence p99. The head fails when its median exceeds the parent's by more than 0.2 ms. A ratio would flake, because five m3 nonmoving trials of one revision already span 0.2255 ms to 0.2961 ms.
Compare revisions with the same compiler, compositor, workload, collector, and host.

## Program checklist

### Arm the program

- [ ] State the protocol and this plan to the operator, then stop. Start execution only on the operator's explicit go.
- [ ] Present the M4-D05 default with the plan. The go does not settle an open design gate for the package that depends on it.
- [ ] On the operator's go, write this exact text into the standing orders and restate it in your todolist. "Plan `docs/m4-plan.md`. Packages in order M4-01 to M4-08, one linear stack. Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked. The operator merges every PR and publishes the release. Done when the operator publishes the `v0.1.0` GitHub release and a clean-account install from the README runs on real river."
- [ ] Read these from the installed plugin at program start. Re-read them at every tick.
  - [ ] `skills/poteto-mode/playbooks/autopilot-stack.md`
  - [ ] `skills/swarm/SKILL.md`
  - [ ] `skills/poteto-mode/playbooks/opening-a-pr.md`
  - [ ] `skills/architect/SKILL.md`, `skills/how/SKILL.md`, `skills/tdd/SKILL.md`, `skills/deslop/SKILL.md`, and `skills/no-comments/SKILL.md`
- [ ] Read [integration-testing.md](integration-testing.md) at program start. It is the driver for every live lane, because the repository has no project driver skill.
- [ ] Arm the hourly audit tick as a real `/loop` in dynamic mode, which schedules its own wake-up rather than blocking on a sleep. Never leave the cadence to memory.
- [ ] Use this tick prompt, verbatim. "Re-read the execution playbook from the installed plugin and the standing objective. Audit the operation against both and fix drift in this tick. Probe every active lane and judge progress by side effects only. Stand down a stuck lane and dispatch its replacement now. Then post a short status message to the operator in chat only when the audit found a tracked change that no earlier status message reported, such as a PR opened, a code-ready head, a round launched or closed, a verdict, a merge, a stuck agent and the action taken, a blocker added or cleared, or a decision only the operator can make. Name every such change and nothing else. Do not repeat a table, the merged list, or an unchanged blocker. If the audit found none, end the turn with no reply text. Either way, log this tick's row in your decision trail. The row names the items reported, or none."
- [ ] On the operator's hold or stand-down, send every owner a zero-writes order at once.

### Spawn owners

- [ ] Spawn one owner per PR with the full lifecycle the execution playbook names. Run one owner at a time. Two concurrent high-effort agents have hit rate limits in this repository.
- [ ] Follow this dependency graph. Each package branches from the head of the package before it.
  - [ ] M4-01 is first and branches from `main`.
  - [ ] M4-02 after M4-01. M4-03 after M4-02. M4-04 after M4-03.
  - [ ] M4-05 after M4-04. M4-06 after M4-05. M4-07 after M4-06.
  - [ ] M4-08 after M4-07.
- [ ] Hold the file boundaries. M4-01 touches only `ci/`, `.github/workflows/`, `README.md`, `docs/latency-*/`, Haddock comments in `src-public/` and in the `src-core/` definitions that `src-public/` re-exports, and an `OPTIONS_HADDOCK hide` pragma on `Seki.Internal.Input`. M4-08 touches only documentation, the `copyright` field of `seki.cabal`, `CHANGELOG.md`, and `examples/`. Each package from M4-02 to M4-07 documents every export it adds, and the M4-01 coverage check fails it otherwise. Record any other scope change before editing another package's files.
- [ ] Hold the review gate. M4-02, M4-03, M4-04, M4-05, M4-06, and M4-07 change an interaction. They wait for the operator's review in chat with screenshots and a video before merge.

### PR mechanics, for every PR

- [ ] Resolve the forge once. Default to `gh`; if `command -v origin` succeeds and Origin can resolve the repository, use `origin pr` for every PR operation. Record any fallback to `gh`. Never require `gt`.
- [ ] Open the PR ready, never draft, per **Opening a PR**. Use the run's built-in PR tool when it has one, else `origin pr create --status open --base <base-branch>` or `gh pr create --base <base-branch>` according to the resolved forge. A stack child targets its parent branch.
- [ ] Run `cabal build all`, `cabal test all`, `bash ci/check-phase-types.sh`, `ci/check-package.sh`, and the formatter once before the PR-facing push. Build inside `nix develop .#unit`, which gives GHC 9.10.3 and cabal 3.16.1 as CI does. Push with hooks on.
- [ ] Run `/deslop` before each commit and `/no-comments` before review.
- [ ] Triage every review-bot and security-reviewer comment per `../references/bugbot-triage.md`.
- [ ] Before the code-ready report and babysit, and again after merge prep, record the base and head SHAs prepared by the topology owner under the execution playbook.

### Verdict and merge, for every PR

- [ ] At the code-ready head SHA and at each later push that changes the patch, run the swarm per `skills/swarm/SKILL.md`. One gates lane. The ten live lanes from the PR's **Verify, live** block. The perf lane from its **Verify, perf** block. Two or more audit lanes, each with its own focus, that read the diff and the receipts and distrust the PR body. The root audits the receipts in the merge-ready report before the verdict.
- [ ] Run the live lanes one at a time. Only one compositor fixture runs at a time on this host, and a busy host reproduces timing flakes.
- [ ] Clean only when every lane is `PASS`. Findings go back to the owner, including a defect that a lane filed as a note. A new head gets a fresh swarm and a fresh verdict, except for results that stay valid under the patch-id rule in `playbooks/shipping.md`.
- [ ] Append on a clean verdict. No owner merges. The root appends the PR to the linear stack, and the operator lands it bottom-up. A verdict carries over a rebase only when the patch-id rule in `playbooks/shipping.md` holds for its SHA and its base.

### Boot recipe, for every live lane

Each live lane runs in its own worktree at the PR head. Drive through the skill path or exact commands recorded in this boot recipe.

- [ ] `git fetch origin <head-branch> && git checkout <head SHA>`. Check out the parent head in a second worktree for lanes and probes that compare revisions.
- [ ] Unset every `SEKI_CONFIG_*` variable and use temporary XDG directories. Never drive the maintainer's live desktop.
- [ ] Run `nix develop --no-write-lock-file .#ci --command ci/run-integration.sh` with `SEKI_INTEGRATION_SCENARIOS` set to the lane's scenario name and `WLR_RENDERER=pixman`. Without Nix, run `SEKI_RUN_INTEGRATION=1` with the built test binary from the repository root.
- [ ] Add each new scenario to `integration/Seki/Integration/M4.hs`, `integration/Main.hs`, `seki.cabal` `other-modules`, and `ci/expected-integration-manifest.txt` in the same PR.
- [ ] Deliver input only through `sekictl`, `wtype` (`-P` and `-p` hold and release a key), the native virtual-pointer probe, and the fixture client. Read the river inventory, the reconcile trace, and IPC snapshots only as diagnostics.
- [ ] Save every screenshot to `/tmp/swarm-<pr-id>/worker-<n>/<slug>.png` and return the paths with the report. Attach the command transcript next to each PNG, because a screenshot alone does not prove a key or focus predicate.

## Add the release checks (M4-01)

**Depends on.** None.

**Files.**

- [ ] Edit `.github/workflows/ci.yml` and `ci/check-package.sh`.
- [ ] Edit `README.md` for the river version lines.
- [ ] Edit `docs/latency-baseline/`, `docs/latency-m2/`, and `docs/latency-m3/` manifests and summaries to remove local paths.
- [ ] Edit the Haddock comments that `src-public/` exports lack. Haddock cannot attach a comment to a re-export, so most of them go on the definitions in `src-core/`. Hide `Seki.Internal.Input` with `{-# OPTIONS_HADDOCK hide #-}` so the `Seki.Input` page lists its exports. Change no code.

**Build.**

- [ ] Measure the Haddock gap first. Record the count of undocumented exports per public module in the PR body. If the coverage output prints no usable count, make the check parse `cabal haddock`'s per-module coverage lines, and record that parser's output instead.
- [ ] Make `ci/check-package.sh` fail when any public module has an undocumented export. Document the existing gaps in this PR.
- [ ] Add a CI step that regenerates `src-generated/` and `tools-generated/` with `seki-scanner` and fails on any diff.
- [ ] Replace `/home/...` worktree paths in the retained latency manifests with repository-relative paths.
- [ ] Keep river 0.4.5 as the runtime minimum in `README.md:260`, because `protocol/SOURCES.md` records that the three v0.5.0 pins do not raise it. Add river 0.5.0 as the release-tested compositor. A later package that needs a newer protocol version raises the minimum in its own PR.

**You see.**

- [ ] CI fails on a hand-edited generated file and on an undocumented public export, and passes at the head.

**Verify, unit.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Run `cabal build all`, `cabal test all`, and `ci/check-package.sh`. Retain the output at the head.
- [ ] Plant a one-byte change in a generated file and an undocumented public export on a scratch commit. Show that each CI step fails, then drop the commit.

**Verify, live.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked. Ten lanes on the configured `swarm workers` model at the PR head, per the boot recipe.

- [ ] Lane 1. Regression lane against trunk. Run the full release manifest at `main` and at the head. `main` has neither new check. Gate both checks passing at the head. Save `release-checks.png`. Pass when the manifest passes at both revisions and both new checks pass at the head.
- [ ] Lane 2. Regenerate the bindings at the head. Save `regen-clean.png`. Pass when the regeneration step reports no diff.
- [ ] Lane 3. Plant a generated-file edit on a scratch branch. Save `regen-planted.png`. Pass when the regeneration step fails and names the file.
- [ ] Lane 4. Plant an undocumented export in `src-public/Seki.hs` on a scratch branch. Save `haddock-planted.png`. Pass when `ci/check-package.sh` fails and names the export's module.
- [ ] Lane 5. Open the Haddock index of the `seki` library. Save `haddock-index.png`. Pass when every public module lists a comment for each export.
- [ ] Lane 6. Grep the retained latency directories and the sdist tarball for `/home/`. Save `no-home-paths.png`. Pass when no match is a local path. The made-up `/home/u` arguments in `test/Seki/Run/RestartTest.hs` are test data, and the lane lists them.
- [ ] Lane 7. Load each edited latency JSON file at `main` and at the head and compare them. `--report-only` reads only schema 5, and the retained directories are schemas 1, 2, and 4 with no trials, so it cannot regenerate them at either revision. Save `latency-paths-only.png`. Pass when every file parses and the only changed values are build paths, now relative to the repository.
- [ ] Lane 8. Run `m3-daily-build` and `m3-daily-delivery`. Save `daily-delivery.png`. Pass when both pass.
- [ ] Lane 9. Run `python3 ci/check-product.py` against `.#config-delivery`. Save `product.png`. Pass when it exits zero.
- [ ] Lane 10. Start seki from the head build in nested river with two outputs and three windows. Save `nested-smoke.png`. Pass when the capture shows the Tall layout and `sekictl` reports the focused window.

**Verify, perf.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Metric. The m2 and m3 workloads' pure p99, sequence p99, maximum pause, and m3 endpoint p99.
- [ ] Probe. Run `python3 tools/latency/run.py artifacts/latency/m4-01 --workload m2 --workload m3 --revision parent=<parent worktree> --revision head=<head worktree>`, which interleaves the revisions.
- [ ] Baseline. Record the parent values first.
- [ ] Rule. Head meets every existing budget and the regression margin in **How to read this**. The report's source comparison calls the runtime group different, because it reads `src-public/`. The PR lists each difference the report names. The gate fails if the report names a difference other than Haddock comments in `src-public/` and `src-core/` and the `OPTIONS_HADDOCK` pragma.

**Review gate.** None. M4-01 is not review-gated.

**Merge.**

- [ ] Root's clean verdict at the exact head SHA.
- [ ] Bugbot triage done.
- [ ] Base and verdict are current under the execution playbook and the patch-id rule in `playbooks/shipping.md`.
- [ ] The root appends M4-01 to the base-branch stack, and the operator lands it bottom-up.

## Keep the screen managed during the first build (M4-02)

**Depends on.** M4-01.

**Files.**

- [ ] Edit `tools/seki/app/Main.hs`. `tools/seki-default/app/Main.hs` needs no change, because `seki def` already reads its arguments through `runConfig`.
- [ ] Edit `src/Seki/Run/Config.hs`, `src/Seki/Run/ConfigBuild.hs`, `src/Seki/Run/BuildSession.hs`, and `src/Seki/Run/Policy.hs`.
- [ ] Edit `test/Seki/Run/BuildSessionTest.hs`, `test/Seki/Run/ConfigTest.hs`, `test/Seki/Run/SessionsTest.hs`, `integration/Seki/Integration/Restart.hs`, and `ci/check-product.py`.
- [ ] Edit `docs/user-configuration.md`, `docs/ipc.md`, and `docs/spec.md` section 5.2. Close #90 from the PR body for the blank screen, and link a follow-up issue for the seat half of its open question.

**Build.**

- [ ] Run `skills/how/SKILL.md` on the launcher, `BuildSession.hs`, and the `--recompile` exec path before code. Then run `skills/architect/SKILL.md` and settle M4-D07. The design round on 2026-11-22 did both, and an interrogate round with three models amended it. Build from that design and its verdict.
- [ ] Stop building before the first exec. `seki` with no arguments execs the last validated profile at once. When no profile exists, it execs the shipped `seki-default`.
- [ ] Make the started image run one build of the user configuration through the build session that `sekictl recompile` already uses. A validated candidate that differs from the running image becomes the profile, and seki execs it with the M2 restore handshake, as a recompile does today.
- [ ] Keep `seki --check` synchronous, and keep `seki --restore FILE` unchanged.
- [ ] Report the build in the IPC snapshot's existing `build` field while it runs and after it fails. A failed first build leaves the running image in place.
- [ ] Answer #90's open question for outputs. If river removes every output during the build, the built profile still receives outputs and devices when they return, and its startup hook still runs. A nested river cannot disable the seat, so file the seat half as a follow-up issue that names what stays inferred.
- [ ] Never exec a profile again that crashed before it proved healthy. The launcher starts `seki-default` instead, which builds and hands over, so a fixed configuration can replace a profile that crashes at start. A profile whose start ended cleanly is not held back. One whose start left no record, as after a power loss, gets one more trial behind `seki-default`'s validated build.
- [ ] Serialize builds. `seki --check` and the session's build take one exclusive lock in the state directory, because they share the Cabal project directory and the private store.

**You see.**

- [ ] On a first start with no built profile, windows are managed by `seki-default` within one second, and the user configuration takes over when its build finishes.

**Verify, unit.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Extend `ConfigTest.hs` for the launcher choice between profile, `seki-default`, and `--check`. Extend `BuildSessionTest.hs` for a startup build that coalesces with a `recompile` request, a failed startup build, and a build that yields the running image. Run `cabal test seki-test`.

**Verify, live.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked. Ten lanes on the configured `swarm workers` model at the PR head, per the boot recipe.

- [ ] Lane 1. Regression lane against trunk. Start seki with a fresh state directory and a configuration that needs a build, at the parent and at the head, and map two fixture windows at once. Save `first-start.png` one second after start. Pass when the parent's capture shows unmanaged output, the head's capture shows both windows tiled, and the head ends on the built profile.
- [ ] Lane 2. Start with a configuration that fails to compile. Save `first-start-failed.png`. Pass when `seki-default` keeps managing windows and `sekictl snapshot` reports the failed build.
- [ ] Lane 3. Start with a current profile, then again after `touch` on a configuration source file. Save `current-profile.png`. Pass when the profile execs directly, the time to the first managed window is no longer than at the parent, and each start ends with `build.state` `unchanged`, the same pid and IPC session, and one run of the startup hook.
- [ ] Lane 4. Open three windows during the first build, once with a configuration that keeps tags `1` to `9` and once with other tags. Save `handover-windows.png`. Pass when all three keep their workspaces after the exec in the first case, and in the second case every window is still managed and placed by the user's manage hook, as the documentation says.
- [ ] Lane 5. Start from a current profile whose hook has two `spawnOnce` programs, edit the configuration while logged out, keep the same two `spawnOnce` programs in the new hook, then start. Save `handover-spawnonce.png`. Pass when the session hands over to the new build and each program starts exactly once.
- [ ] Lane 6. Rebuild the stub delivery, as `m3-daily-delivery` does, then start fresh. Save `upgrade-start.png`. Pass when the screen is managed during the rebuild and the session ends on the rebuilt profile.
- [ ] Lane 7. Remove the only output during the first build and add it back after the exec. Save `output-lost.png`. Pass when the built profile manages the returned output and its startup hook has run.
- [ ] Lane 8. Send `sekictl recompile` during the first build. Save `recompile-coalesce.png`. Pass when the first build's result is discarded, one more build runs after it with no two compilers at once, and seki execs once into the second build.
- [ ] Lane 9. Run `seki --check` with a fresh state directory. Save `check-sync.png`. Pass when it builds, prints the validated path, and starts no window manager.
- [ ] Lane 10. Run `m3-daily-build`, `m3-daily-delivery`, and the restart scenarios. Save `daily-handover.png`. Pass when all pass.
- [ ] Lane 11. Build a profile whose startup hook throws, start it, then fix the configuration and let river's init loop start `seki` again. Save `crash-recovery.png`. Pass when the second start runs `seki-default`, the fixed configuration builds and takes over, and the broken profile is not exec'd again. Also kill a healthy profile before it proved healthy and let the init loop start `seki` again. Save `kill-recovery.png`. Pass when that start runs `seki-default`, hands over to the reproduced profile, and the start after it execs the profile directly.

**Verify, perf.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Metric. The time from starting `seki` to the first managed window on a first start, at the parent and at the head. The m2 and m3 metrics at both.
- [ ] Probe. Run lane 1's scenario five times per revision, alternating revisions, and read the first `focus_window` time from the reconcile trace. Run the M4-01 latency command in `artifacts/latency/m4-02`.
- [ ] Baseline. Record the parent's first-start values first. The parent's value is the build time, about 32 seconds in #90.
- [ ] Rule. The head's slowest first start to a managed window fails above 1 second. The head meets every existing budget and the regression margin.

**Review gate.** The operator reviews before merge.

- [ ] Copy lane 1 and lane 2 screenshots into `artifacts/m4-review/M4-02-review-<slug>.png`.
- [ ] Record a 30 to 60 second video of a first start in a lane worktree. Save it as `artifacts/m4-review/M4-02-review.mp4`.
- [ ] Post the screenshots and the video in chat to the operator. Stop at merge-ready. Wait for the operator's click.

**Merge.**

- [ ] Root's clean verdict at the exact head SHA.
- [ ] Bugbot triage done.
- [ ] Base and verdict are current under the execution playbook and the patch-id rule in `playbooks/shipping.md`.
- [ ] The root appends M4-02 to the base-branch stack, and the operator lands it bottom-up.

## Repeat held bindings on core-owned timers (M4-03)

**Depends on.** M4-02.

**Files.**

- [ ] Edit `src/Seki/Run/Runtime.hs`, `src/Seki/Run/Policy.hs`, `src/Seki/Run/Reconcile.hs`, and `src/Seki/Run/External.hs`.
- [ ] Edit `src-core/Seki/Internal/Config.hs`, `src-core/Seki/Internal/Core/State.hs`, and `src-core/Seki/Internal/Core/Event.hs`.
- [ ] Create `src-core/Seki/Internal/Core/Timer.hs`.
- [ ] Edit `src-public/Seki.hs`, `docs/user-configuration.md`, and `test-public/PortedConfig.hs`.
- [ ] Edit `tools/latency/` to add an `m4` workload.

**Build.**

- [ ] Run `skills/how/SKILL.md` on the M3-01 ingress path in `External.hs` and `ManageWake.hs`, then `skills/architect/SKILL.md` for the timer shape. Settle M4-D01.
- [ ] Bind `river_xkb_bindings_seat_v1` for the single seat.
- [ ] Add `Timer.hs`. `CoreState` holds desired timers as `Map TimerKey Deadline` with a generation per key. `TimerKey` is a closed sum, so a later package adds a constructor rather than a string. `RepeatTimer` is its first constructor.
- [ ] Reconcile desired timers into one runtime timer thread. At a deadline the thread calls `signalExternal`, which wakes the owner whatever the bounded queue holds. It never calls `submitExternal`, so a full queue cannot reject a fire.
- [ ] At each `manage_start`, read the clock once. Apply the drained events first, in arrival order, then fire every timer whose deadline has passed. A due timer stays pending until it fires, is cancelled, is superseded by a new generation, or its session closes. A stale generation never fires.
- [ ] Re-arm a periodic timer from its previous deadline, not from the fire time. When a manage sequence starts after several periods, fire once and set the next deadline to the first period boundary still in the future.
- [ ] Tie timers to the session. A timer from an old session cannot enter a restored one, as M3-01 does for external work.
- [ ] Add a per-binding repeat setting to the public key map, with a default delay and rate in `SekiConfig`. A binding without it behaves exactly as today. `Pressed` on a repeating binding runs the action and arms `RepeatTimer` at the delay. Each fire runs the action again.
- [ ] Stop the repeat on `Released`, `StopRepeat`, a seat change, and session end. Each one cancels the timer key.
- [ ] Mark the brightness keys in `test-public/PortedConfig.hs` as repeating.
- [ ] Add the `m4` latency workload with two endpoints, the timer's lateness past its deadline and the interval between repeated actions.

**You see.**

- [ ] Holding `XF86MonBrightnessUp` in the ported configuration runs its action once, then again at the configured rate until release.

**Verify, unit.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Add `test/Seki/Internal/Core/TimerTest.hs` with properties for stale-generation drops, re-arm from the previous deadline, a late manage sequence that fires once, and cancel. Run `cabal test seki-test`.
- [ ] Extend `RuntimeTest.hs` for a fire during manage, during render, at shutdown, and while the queue is full. The full-queue case forces `submitExternal` to return `Full` before the deadline and asserts that the current generation fires exactly once after the drain.
- [ ] Extend `EventTest.hs` and `PolicyTest.hs` for press, hold, release, `StopRepeat`, a second press during repeat, and a binding without repeat. Run `cabal test seki-test seki-public`.

**Verify, live.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked. Ten lanes on the configured `swarm workers` model at the PR head, per the boot recipe.

- [ ] Lane 1. Regression lane against trunk. Hold a binding that spawns a counter command for two seconds with `wtype -P` and `-p`, at the parent and at the head. The parent has no repeat. Save `repeat-hold.png`. Pass when the parent runs it once and the head runs it once plus the count the delay and rate predict, within one.
- [ ] Lane 2. Tap the binding. Save `repeat-tap.png`. Pass when it runs once.
- [ ] Lane 3. Hold the binding, then press another key. Save `repeat-stop.png`. Pass when `stop_repeat` ends the repeat and the second key reaches its target.
- [ ] Lane 4. Hold a binding without repeat. Save `repeat-off.png`. Pass when it runs once.
- [ ] Lane 5. Hold a repeating binding while foot has focus. Save `repeat-client.png`. Pass when foot receives none of the bound key's events.
- [ ] Lane 6. Hold the binding and restart seki with `sekictl`. Save `repeat-restart.png`. Pass when no action runs after the restart until a new press.
- [ ] Lane 7. Hold the binding, then end the compositor. Save `repeat-shutdown.png`. Pass when the timer thread exits and the session closes cleanly.
- [ ] Lane 8. Fill the external queue to 256 with IPC commands, so the next `submitExternal` returns `Full`, and hold the repeating binding through the drain. Save `repeat-full-queue.png`. Pass when the overflow is reported, every accepted command runs once, and the repeat count matches lane 1's prediction within one.
- [ ] Lane 9. Run `m3-daily-ime` with the bindings seat bound. Save `repeat-ime.png`. Pass when the exact `日本語` commit still arrives.
- [ ] Lane 10. Run `m3-daily-load`, `m3-daily-two-output`, and `m3-daily-hotplug` while a repeating binding is held. Save `repeat-load.png`. Pass when every scenario's existing ceilings hold.

**Verify, perf.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Metric. The m2 and m3 metrics at the parent and at the head. The diff-added work is the `m4` timer lateness and repeat interval.
- [ ] Probe. Run `python3 tools/latency/run.py artifacts/latency/m4-03 --workload m2 --workload m3 --workload m4 --revision parent=<parent worktree> --revision head=<head worktree>`. The parent produces m2 and m3. The head produces all three.
- [ ] Baseline. Record the parent m2 and m3 values first.
- [ ] Rule. The head meets every existing budget and the regression margin. A timer lateness p99 above 10 ms fails. At a 25 Hz rate, a repeat interval p99 more than 10 ms away from 40 ms fails. Do not compare an m4 endpoint with any parent number.

**Review gate.** The operator reviews before merge.

- [ ] Copy lane 1 and lane 3 screenshots into `artifacts/m4-review/M4-03-review-<slug>.png`.
- [ ] Record a 30 to 60 second video of the change in a lane worktree. Save it as `artifacts/m4-review/M4-03-review.mp4`.
- [ ] Post the screenshots and the video in chat to the operator. Stop at merge-ready. Wait for the operator's click.

**Merge.**

- [ ] Root's clean verdict at the exact head SHA.
- [ ] Bugbot triage done.
- [ ] Base and verdict are current under the execution playbook and the patch-id rule in `playbooks/shipping.md`.
- [ ] The root appends M4-03 to the base-branch stack, and the operator lands it bottom-up.

## Add submaps (M4-04)

**Depends on.** M4-03.

**Files.**

- [ ] Edit `src-core/Seki/Internal/Config.hs`, `src-core/Seki/Internal/Core/State.hs`, and `src-core/Seki/Internal/Core/Timer.hs`.
- [ ] Edit `src/Seki/Run/Runtime.hs`, `src/Seki/Run/Reconcile.hs`, `src/Seki/Run/Policy.hs`, and `src/Seki/Run/Snapshot.hs`.
- [ ] Edit `src-public/Seki.hs`, `docs/ipc.md`, `docs/spec.md` sections 3.8 and 5.3, and `examples/waybar/seki.py`.

**Build.**

- [ ] Run the M4-D03 probe before code. On river 0.5.0, hold a chord, disable its binding in the next manage sequence, then release the key. Record whether river sends `released` to the binding, sends the release to the focused client, or sends neither.
- [ ] Run `skills/architect/SKILL.md` before code. Settle M4-D02.
- [ ] Model key maps as `Map SubmapName Submap` plus the root map. A `Submap` holds its key map and its exit rule. The active submap lives in `CoreState` as `Maybe SubmapName`.
- [ ] Make the exit rule a closed sum. `OneShot` leaves after the next non-modifier key, bound or not, as xmonad's `XMonad.Actions.Submap` does, and is the default. `Sticky` stays after bound keys and leaves on an explicit `exitSubmap` action, on an unbound key when `leaveOnUnbound` is set, or after its `timeout` when one is set.
- [ ] Keep one river binding object per distinct chord. The reconciler enables a chord exactly when the active map binds it and disables it otherwise. This replaces the enable-once path in `runManagePlans`, which starts at `Runtime.hs:791`.
- [ ] Apply the M4-D03 result to the entry chord. If river would send its release to the client, keep the entry chord's binding enabled until its `released` arrives, then disable it.
- [ ] Decode `ate_unbound_key` into a core event.
- [ ] Request `ensure_next_key_eaten` on entry to a `OneShot` submap and to a `Sticky` submap with `leaveOnUnbound`. In the `Sticky` case, request it again in the manage sequence after each bound press that leaves the submap active, because one request covers only the next non-modifier key.
- [ ] Handle the race the spec names. An `ate_unbound_key` that arrives after `cancel_ensure_next_key_eaten` leaves state unchanged.
- [ ] Add a `SubmapTimeout` timer key. Within one manage sequence, apply key events before due timers, as M4-03 orders them. A press that reached seki before the sequence started therefore runs in the submap even when the timeout has passed, and the press re-arms the timeout.
- [ ] Resolve `released` and `stop_repeat` against the press that started them. Record the action and its submap at press time. A submap change between press and release never selects a different action.
- [ ] Add the `submap` field to the IPC snapshot as a string or null. Follow the `build` field's precedent and keep IPC version 1. Show it in the Waybar example.

**You see.**

- [ ] Pressing the submap key shows the submap name in `sekictl subscribe`. Its keys work, the root map's other keys pass to the client, and the exit rule returns the snapshot to null.

**Verify, unit.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Extend `ReconcileTest.hs` for the enable and disable diff, the held entry chord, and the re-armed `ensure_next_key_eaten` after a bound press.
- [ ] Extend `PolicyTest.hs` for each exit rule, the race, a timeout and a press in one manage sequence, and a release after a submap change. Extend `IPCTest.hs` for the new field. Run `cabal test seki-test` and `python3 examples/waybar/test_seki.py`.

**Verify, live.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked. Ten lanes on the configured `swarm workers` model at the PR head, per the boot recipe.

- [ ] Lane 1. Regression lane against trunk. Run the release manifest's binding scenarios at the parent and at the head. The parent has no submaps. Gate the head entering a `Sticky` resize submap, resizing twice, and leaving through `exitSubmap`. Save `submap-resize.png`. Pass when the master width changes twice and the snapshot's `submap` goes from null to the name and back.
- [ ] Lane 2. Enter a `Sticky` submap with `leaveOnUnbound`, press its bound resize key once, then press a key the submap does not bind. Save `submap-rearm.png`. Pass when the client receives neither the press nor the release of the last key, and the snapshot's `submap` returns to null.
- [ ] Lane 3. Enter a `OneShot` submap and press a bound key, then press the same key again. Save `submap-oneshot.png`. Pass when the first press runs the submap action and leaves, and the second press acts as the root map says.
- [ ] Lane 4. Enter a `OneShot` submap while foot has focus through fcitx5, press an unbound key, then type `日本語`. Save `submap-oneshot-unbound.png`. Pass when the unbound key is eaten, the submap leaves, and the text commits exactly.
- [ ] Lane 5. Enter a `Sticky` submap with a timeout and wait. Save `submap-timeout.png`. Pass when it leaves within the timeout plus 50 ms with no input.
- [ ] Lane 6. Bind the same chord in the root map and in a submap to different actions. Save `submap-shadow.png`. Pass when the submap's action runs inside the submap and the root action runs outside it.
- [ ] Lane 7. Hold the entry chord, wait 200 ms, then release it while foot has focus. Save `submap-entry-release.png`. Pass when foot receives no event for the entry key and the submap stays active.
- [ ] Lane 8. Hold a repeating binding inside a `Sticky` submap, then leave the submap through its timeout while the key is held. Save `submap-repeat.png`. Pass when the repeat stops at the transition and the release runs no root action.
- [ ] Lane 9. Enter a submap and restart seki. Save `submap-restart.png`. Pass when the restored session starts in the root map and the snapshot says null.
- [ ] Lane 10. Run the Waybar module against a session that enters and leaves a submap. Save `submap-waybar.png`. Pass when the module text shows and then clears the name.

**Verify, perf.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Metric. The m2 and m3 metrics at the parent and at the head. The diff-added work is the time from the submap key press to the IPC snapshot that names the submap.
- [ ] Probe. Run the M4-03 command with `--workload m4` extended by the submap endpoint, in `artifacts/latency/m4-04`.
- [ ] Baseline. Record the parent m2 and m3 values first.
- [ ] Rule. The head meets every existing budget and the regression margin. A submap endpoint p99 above 50 ms fails.

**Review gate.** The operator reviews before merge.

- [ ] Copy lane 1, lane 3, and lane 10 screenshots into `artifacts/m4-review/M4-04-review-<slug>.png`.
- [ ] Record a 30 to 60 second video of the change in a lane worktree. Save it as `artifacts/m4-review/M4-04-review.mp4`.
- [ ] Post the screenshots and the video in chat to the operator. Stop at merge-ready. Wait for the operator's click.

**Merge.**

- [ ] Root's clean verdict at the exact head SHA.
- [ ] Bugbot triage done.
- [ ] Base and verdict are current under the execution playbook and the patch-id rule in `playbooks/shipping.md`.
- [ ] The root appends M4-04 to the base-branch stack, and the operator lands it bottom-up.

## Make the focus model configurable (M4-05)

**Depends on.** M4-04.

**Files.**

- [ ] Edit `src-core/Seki/Internal/Config.hs` and `src-core/Seki/Internal/Core/Event.hs`.
- [ ] Edit `src/Seki/Run/Runtime.hs`, `src/Seki/Run/Policy.hs`, and `src/Seki/Run/Reconcile.hs`.
- [ ] Edit `src-public/Seki.hs` and `docs/spec.md` section 3.6.

**Build.**

- [ ] Run the M4-D04 probe before code. Record whether river 0.5.0 sends `pointer_enter` when a window moves under a still pointer.
- [ ] Add `focusModel :: FocusModel` with `ClickToFocus` and `FocusFollowsMouse` to `SekiConfig`. The default stays `ClickToFocus`.
- [ ] Under `FocusFollowsMouse`, turn `pointer_enter` into a focus event. Use the M4-D04 result to decide whether to drop entries that a layout change caused, using `pointer_position`.
- [ ] Apply M4-D05. Under the default, a pointer entry does not take keyboard focus back from a non-exclusive layer surface. A click and a focus key binding still do.
- [ ] Record both rules in `spec.md` 3.6 and close the M2 plan's "no design is recorded yet" item.

**You see.**

- [ ] With `FocusFollowsMouse`, moving the pointer onto a window gives it the focused border and keyboard input.

**Verify, unit.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Extend `EventTest.hs` and `PolicyTest.hs` for both models, layer focus in each mode, and the layout-caused entry. Run `cabal test seki-test`.

**Verify, live.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked. Ten lanes on the configured `swarm workers` model at the PR head, per the boot recipe.

- [ ] Lane 1. Regression lane against trunk. Move the virtual pointer from the master window to a stack window at the parent and at the head, with `FocusFollowsMouse` set at the head. Save `ffm-move.png`. Pass when the parent keeps focus on the master and the head focuses the stack window.
- [ ] Lane 2. Run the same move with the default model at the head. Save `ctf-move.png`. Pass when focus stays on the master.
- [ ] Lane 3. Click a window under `ClickToFocus`. Save `ctf-click.png`. Pass when it gains focus exactly as at the parent.
- [ ] Lane 4. Park the pointer and switch workspace with a key. Save `ffm-switch.png`. Pass when focus follows the M4-D04 rule recorded in the spec.
- [ ] Lane 5. Open a non-exclusive layer surface, then move the pointer over a window. Save `ffm-layer-nonexclusive.png`. Pass when keyboard focus stays on the layer surface.
- [ ] Lane 6. Lock with an exclusive layer surface and move the pointer. Save `ffm-layer-exclusive.png`. Pass when no focus request is sent and every window draws the unfocused border.
- [ ] Lane 7. Move the pointer across two outputs. Save `ffm-outputs.png`. Pass when the current screen and focus follow the pointer.
- [ ] Lane 8. Start a mod-drag move under `FocusFollowsMouse`. Save `ffm-drag.png`. Pass when the dragged window keeps focus until release.
- [ ] Lane 9. Move the pointer over a floating dialog above a tiled window. Save `ffm-float.png`. Pass when the dialog gains focus.
- [ ] Lane 10. Move the pointer onto an Xwayland window. Save `ffm-xwayland.png`. Pass when the Xterm gains keyboard focus.

**Verify, perf.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Metric. The m2 and m3 metrics at the parent and at the head. The diff-added work is the time from virtual pointer motion to the IPC snapshot that names the new focused window.
- [ ] Probe. Run the M4-03 command with `--workload m4` extended by the pointer-focus endpoint, in `artifacts/latency/m4-05`.
- [ ] Baseline. Record the parent m2 and m3 values first.
- [ ] Rule. The head meets every existing budget and the regression margin. A pointer-focus endpoint p99 above 50 ms fails.

**Review gate.** The operator reviews before merge.

- [ ] Copy lane 1 and lane 5 screenshots into `artifacts/m4-review/M4-05-review-<slug>.png`.
- [ ] Record a 30 to 60 second video of the change in a lane worktree. Save it as `artifacts/m4-review/M4-05-review.mp4`.
- [ ] Post the screenshots and the video in chat to the operator. Stop at merge-ready. Wait for the operator's click.

**Merge.**

- [ ] Root's clean verdict at the exact head SHA.
- [ ] Bugbot triage done.
- [ ] Base and verdict are current under the execution playbook and the patch-id rule in `playbooks/shipping.md`.
- [ ] The root appends M4-05 to the base-branch stack, and the operator lands it bottom-up.

## Add the contrib layouts, modifiers, and per-workspace layouts (M4-06)

**Depends on.** M4-05.

**Files.**

- [ ] Edit `src-core/Seki/Internal/Core/Layout.hs`, `src/Seki/Run/Reconcile.hs`, and the existing layouts under `src-core/Seki/Internal/Layout/`.
- [ ] Create `src-core/Seki/Internal/Layout/ThreeColumns.hs`, `src-core/Seki/Internal/Layout/Grid.hs`, `src-core/Seki/Internal/Layout/Modifier.hs`, and `src-core/Seki/Internal/Layout/PerWorkspace.hs`.
- [ ] Create the matching public modules under `src-public/Seki/Layout/`, and edit `seki.cabal`.

**Build.**

- [ ] Run `skills/architect/SKILL.md` before code, for both changes to the layout class at once. `pureLayout` takes only the layout, a rectangle, and the stack, and returns only rectangles. Border modifiers need a per-window border decision in the result. `onWorkspace` needs the workspace tag as input. Change the class once, in this package.
- [ ] Add `ThreeColMid` and `Grid` layouts that answer `Resize` and `IncMasterN` where they apply.
- [ ] Add `noBorders`, `smartBorders`, `spacing` for outer and inner gaps, and `renamed` for the layout name in IPC.
- [ ] Add `onWorkspace` and `onWorkspaces` for per-workspace layouts.
- [ ] Keep `Tall`, `Full`, `Mirror`, and `|||` output identical. Every existing layout test passes unchanged.

**You see.**

- [ ] `onWorkspace "9" Full (smartBorders (spacing 8 (Tall 1 0.03 0.5 ||| ThreeColMid 1 0.03 0.5 ||| Grid)))` gives workspace 9 Full and the other workspaces three layouts with gaps, and a lone window has no border.

**Verify, unit.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Add `ThreeColumnsTest.hs`, `GridTest.hs`, `ModifierTest.hs`, and `PerWorkspaceTest.hs` under `test/Seki/Internal/Layout/`, with properties that tiles never overlap and stay inside the area. Run `cabal test seki-test seki-public`.

**Verify, live.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked. Ten lanes on the configured `swarm workers` model at the PR head, per the boot recipe.

- [ ] Lane 1. Regression lane against trunk. Run the Tall layout with four windows at the parent and at the head. The parent lacks the new layouts. Gate the head's ThreeColMid with four windows. Save `layout-tall-threecol.png`. Pass when the Tall geometry is byte-identical at both revisions, and the head's ThreeColMid puts the master in the middle.
- [ ] Lane 2. Grid with five windows. Save `layout-grid.png`. Pass when the inventory shows no overlap and every tile is inside the output.
- [ ] Lane 3. `spacing 8` on Tall. Save `layout-spacing.png`. Pass when every gap measures 8 logical pixels.
- [ ] Lane 4. `smartBorders` with one window, then two, then `noBorders` on Full. Save `layout-borders.png`. Pass when the lone window has no border, both windows have borders after the second maps, and the Full window has none.
- [ ] Lane 5. `renamed` on a layout, then read the snapshot. Save `layout-renamed.png`. Pass when the IPC layout name is the given name.
- [ ] Lane 6. `onWorkspace "9" Full` with three windows on workspace 9 and three on workspace 1. Save `perworkspace.png`. Pass when workspace 9 uses Full and workspace 1 uses the default layout.
- [ ] Lane 7. Resize and change the master count in ThreeColMid. Save `layout-threecol-resize.png`. Pass when the columns respond.
- [ ] Lane 8. Fullscreen a window under `spacing`. Save `layout-fullscreen.png`. Pass when it covers the output with no gap.
- [ ] Lane 9. Restart seki in Grid on workspace 1 and Full on workspace 9. Save `layout-restart.png`. Pass when each workspace's layout and modifiers restore.
- [ ] Lane 10. Float a window over ThreeColMid with mod-drag. Save `layout-float.png`. Pass when it moves freely and the tiles reflow.

**Verify, perf.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Metric. The m2 and m3 metrics at the parent and at the head with the default layout. The diff-added work is the pure layout p99 of the m2 workload under `smartBorders (spacing 8 ThreeColMid)`.
- [ ] Probe. Run the M4-03 command in `artifacts/latency/m4-06`, plus a head-only m2 run with the new layout configured.
- [ ] Baseline. Record the parent m2 and m3 values first.
- [ ] Rule. The head meets every existing budget and the regression margin. The new layout's pure p99 fails at or above 2 ms.

**Review gate.** The operator reviews before merge.

- [ ] Copy lane 1, lane 3, and lane 4 screenshots into `artifacts/m4-review/M4-06-review-<slug>.png`.
- [ ] Record a 30 to 60 second video of the change in a lane worktree. Save it as `artifacts/m4-review/M4-06-review.mp4`.
- [ ] Post the screenshots and the video in chat to the operator. Stop at merge-ready. Wait for the operator's click.

**Merge.**

- [ ] Root's clean verdict at the exact head SHA.
- [ ] Bugbot triage done.
- [ ] Base and verdict are current under the execution playbook and the patch-id rule in `playbooks/shipping.md`.
- [ ] The root appends M4-06 to the base-branch stack, and the operator lands it bottom-up.

## Add the contrib workspace actions and ManageHook placements (M4-07)

**Depends on.** M4-06.

**Files.**

- [ ] Edit `src-core/Seki/Internal/Core/Action.hs` and `src-core/Seki/Internal/Core/ManageHook.hs`.
- [ ] Create `src-public/Seki/Actions/CycleWS.hs`, and edit `src-public/Seki.hs` and `seki.cabal`.

**Build.**

- [ ] Add `nextWS`, `prevWS`, `shiftToNext`, `shiftToPrev`, `toggleWS`, `nextScreen`, `prevScreen`, and `moveTo` with a non-empty filter.
- [ ] Add `doCenterFloat` and `doRectFloat` to the ManageHook language.

**You see.**

- [ ] A binding on `nextWS` steps through the workspaces, and a `doCenterFloat` rule centers a new window.

**Verify, unit.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Extend `ActionTest.hs` and `ManageHookTest.hs`, and add a public-API compile test in `test-public/`. Run `cabal test seki-test seki-public`.

**Verify, live.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked. Ten lanes on the configured `swarm workers` model at the PR head, per the boot recipe.

- [ ] Lane 1. Regression lane against trunk. Run the M1 workspace scenarios at the parent and at the head. The parent lacks the new actions. Gate the head's `nextWS` from workspace 9. Save `cyclews-wrap.png`. Pass when the M1 scenarios pass at both revisions, and the head wraps to workspace 1.
- [ ] Lane 2. `prevWS` from workspace 1. Save `cyclews-prev.png`. Pass when it wraps to workspace 9.
- [ ] Lane 3. `shiftToNext` with a focused window. Save `cyclews-shift.png`. Pass when the window moves and the view stays.
- [ ] Lane 4. `shiftToPrev` from workspace 1 with a focused window. Save `cyclews-shift-prev.png`. Pass when the window lands on workspace 9 and the view stays.
- [ ] Lane 5. `toggleWS` twice. Save `cyclews-toggle.png`. Pass when the view returns to the first workspace.
- [ ] Lane 6. `moveTo` with the non-empty filter. Save `cyclews-nonempty.png`. Pass when it skips empty workspaces.
- [ ] Lane 7. `nextScreen` and `prevScreen` with two outputs. Save `cyclews-screen.png`. Pass when focus moves to the other output and back.
- [ ] Lane 8. Bind `nextWS` in a `Sticky` submap and press it three times. Save `cyclews-submap.png`. Pass when the view advances three workspaces and the submap stays active.
- [ ] Lane 9. Map a fixture window that `doCenterFloat` matches. Save `center-float.png`. Pass when its center is the output's center, within one pixel.
- [ ] Lane 10. Map a fixture window that `doRectFloat` matches with a relative rectangle. Save `rect-float.png`. Pass when its box matches the rectangle on the output.

**Verify, perf.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Metric. The m2 and m3 metrics at the parent and at the head. The diff-added work is the `nextWS` binding, timed from the key press to the IPC snapshot that shows the new workspace.
- [ ] Probe. Run the M4-03 command with `--workload m4` extended by a `nextWS` endpoint, in `artifacts/latency/m4-07`.
- [ ] Baseline. Record the parent m2 and m3 values first.
- [ ] Rule. The head meets every existing budget and the regression margin. A `nextWS` endpoint p99 above 50 ms fails.

**Review gate.** The operator reviews before merge.

- [ ] Copy lane 1, lane 8, and lane 9 screenshots into `artifacts/m4-review/M4-07-review-<slug>.png`.
- [ ] Record a 30 to 60 second video of the change in a lane worktree. Save it as `artifacts/m4-review/M4-07-review.mp4`.
- [ ] Post the screenshots and the video in chat to the operator. Stop at merge-ready. Wait for the operator's click.

**Merge.**

- [ ] Root's clean verdict at the exact head SHA.
- [ ] Bugbot triage done.
- [ ] Base and verdict are current under the execution playbook and the patch-id rule in `playbooks/shipping.md`.
- [ ] The root appends M4-07 to the base-branch stack, and the operator lands it bottom-up.

## Prepare the 0.1.0 release (M4-08)

**Depends on.** M4-07.

**Files.**

- [ ] Edit `README.md`, `docs/user-configuration.md`, `docs/spec.md` section 6.2, and `docs/ipc.md`.
- [ ] Create `CHANGELOG.md` and `docs/release-0.1.0.md`.
- [ ] Edit the `copyright` field of `seki.cabal` and `examples/config/`.

**Build.**

- [ ] Add an Installation section to the README for the flake. Cover `nix profile install` of `config-delivery`, the river init line, and the managed first start from M4-02. Write it for someone without the maintainer's setup.
- [ ] Remove the maintainer-specific wallpaper, font, and LightDM text from `docs/user-configuration.md`, or mark each as one example.
- [ ] Write `CHANGELOG.md` for 0.1.0 and the GitHub release notes in `docs/release-0.1.0.md`. Add the missing `copyright` field. `homepage`, `bug-reports`, and `category` already exist.
- [ ] Extend `examples/config/Main.hs` with a submap, a repeating binding, the focus model, and one new layout, using public modules only.
- [ ] Update the spec's M4 row. Add an "M5 onwards" row with per-output workspaces, the prompt, Hackage, a plain-cabal install, and automatic client urgency.

**You see.**

- [ ] A user in a fresh account follows the README alone and reaches a running seki session with the example configuration.

**Verify, unit.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Run `cabal haddock seki`, `ci/check-package.sh`, `cabal test all`, and `python3 ci/check-product.py` against `.#config-delivery`.

**Verify, live.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked. Ten lanes on the configured `swarm workers` model at the PR head, per the boot recipe.

- [ ] Lane 1. Regression lane against trunk. Follow the README's install steps in a fresh `HOME` at the parent and at the head, with the flake reference pointed at each checkout. The parent has no Installation section, so record that and gate the head's steps reaching a running session. Save `readme-install.png`. Pass when the head's steps start seki in nested river with no step outside the README.
- [ ] Lane 2. Run the README's first start in the fresh `HOME`. Save `readme-first-start.png`. Pass when windows are managed within one second and the example configuration takes over after its build.
- [ ] Lane 3. Run `seki --check` on the example configuration from the installed delivery. Save `example-check.png`. Pass when it validates.
- [ ] Lane 4. Exercise the example configuration's submap, a repeat, the focus model, and the new layout. Save `example-run.png`. Pass when each works as its README section says.
- [ ] Lane 5. Open every relative link in `README.md`, `CHANGELOG.md`, and `docs/`. Save `docs-links.png`. Pass when none is broken.
- [ ] Lane 6. Grep the shipped docs for maintainer-specific text. Save `docs-generic.png`. Pass when every remaining mention is marked as an example.
- [ ] Lane 7. Run `sekictl` and the Waybar module from the installed delivery. Save `release-ipc.png`. Pass when both show workspaces and the `submap` field.
- [ ] Lane 8. Run the full release manifest at the head. Save `release-manifest.png`. Pass when every scenario passes.
- [ ] Lane 9. Follow the README's migration section to build `seki-reference-config`. Save `release-port.png`. Pass when it builds and runs.
- [ ] Lane 10. Build the GitHub source archive with `git archive` at the head and build `.#config-delivery` from the unpacked tree. Save `release-archive.png`. Pass when the build succeeds and `ci/check-product.py` passes on it.

**Verify, perf.** Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

- [ ] Metric. The m2, m3, and m4 metrics at the parent and at the head.
- [ ] Probe. Run the M4-03 command with all three workloads in `artifacts/latency/m4-08`, and commit the compact report as `docs/latency-m4/`.
- [ ] Baseline. Record the parent values first. The parent is the M4-07 head, which holds the whole feature stack.
- [ ] Rule. The head meets every existing budget, every M4 endpoint budget, and the regression margin. The source comparison must call the runtime group identical. The only difference the report may name is the `copyright` field, which the runner already ignores as metadata.

**Review gate.** None. M4-08 is not review-gated.

**Merge.**

- [ ] Root's clean verdict at the exact head SHA.
- [ ] Bugbot triage done.
- [ ] Base and verdict are current under the execution playbook and the patch-id rule in `playbooks/shipping.md`.
- [ ] The root appends M4-08 to the base-branch stack, and the operator lands it bottom-up.
- [ ] After the operator lands the stack, the operator tags `v0.1.0` and publishes the GitHub release from `docs/release-0.1.0.md`. Agents never tag or publish.

## Close the program

- [ ] Every box above is checked with its evidence.
- [ ] The operator records the `v0.1.0` release and a clean-account install from the README on real river in an acceptance record in this plan.
- [ ] Mark M4 Complete in `docs/spec.md` 6.2.
- [ ] Reply to the operator with links to the stack root and tip, a one-line verdict per link, and anything parked or excluded with the reason.

## Appendix A. Prototype evidence

**The `-Werror` options are the only Hackage blocker, and Hackage is out of M4.** On 2026-11-20 at `9ae334d`, `cabal check` ended with "Hackage would reject this package" and named the three unconditional `-Werror` options. A scratch worktree with those three lines removed passed `cabal check`, and only a warning about `-O2` on `seki-latency` remained. A review on 2026-11-22 found the same at `d1dc31d`. The operator chose the flake and a GitHub release on 2026-11-22, so M4 leaves those options alone.

**Neither candidate name is taken on Hackage.** On 2026-11-22, `https://hackage.haskell.org/package/seki` and `https://hackage.haskell.org/package/seki-wm` both returned 404. The search page rendered no results without JavaScript, so it proves nothing about similar names. 0.1.0 keeps the name `seki`.

**The generated bindings already contain every request and event M4 needs.** `src-generated/Seki/Protocol/RiverXkbBindingsV1.hs` decodes `Pressed`, `Released`, `StopRepeat`, and `ate_unbound_key`, and encodes `ensure_next_key_eaten` and `cancel_ensure_next_key_eaten`. Nothing in `src/` uses them, and no code binds `river_xkb_bindings_seat_v1`. This finding comes from reading the code. No run has exercised these messages.

**The timer and margin budgets have headroom over measured values.** `docs/latency-m3/report.md` shows `external_wake` p99 between 2.05 ms and 2.27 ms across its m3 trials, under the 10 ms timer lateness budget. The same report shows m3 head nonmoving manage p99 between 0.2255 ms and 0.2961 ms across five trials, a 0.07 ms spread under the 0.2 ms regression margin.

**The first start builds before it execs.** `tools/seki/app/Main.hs:40-46` runs `generate` and `build` and only then calls `activate`, which execs the profile. Nothing manages the screen during the build, which matches #90. `seki-default` runs only when the build throws.

**The design gates stay open.** Each one names its default and the package that settles it.

| Gate | Question | Default | Settled by |
|---|---|---|---|
| M4-D01 | Where timers live | Desired timers in `CoreState` with generations. One runtime thread wakes the owner with `signalExternal`, and `manage_start` fires every due timer after the drained events. Fires never enter the bounded queue | M4-03 unit tests and lanes 6 to 8 |
| M4-D02 | How submaps map to river binding objects | One object per distinct chord, enabled when the active map binds it. This avoids relying on river's unspecified choice between duplicate bindings | M4-04 lane 6 |
| M4-D03 | What river does with a held chord whose binding is disabled | Unknown. Probe before M4-04 code. If the release would reach the client, keep the entry binding enabled until its release | M4-04 probe and lane 7 |
| M4-D04 | Whether river sends `pointer_enter` when a window moves under a still pointer | Unknown. Probe before M4-05 code | M4-05 probe and lane 4 |
| M4-D05 | Whether a pointer entry takes focus back from a non-exclusive layer surface | No. Only a click or a focus key binding does, as M1-D01 already rules | Operator preference, M4-05 lane 5 |
| M4-D06 | Release channels and package name | Settled on 2026-11-22. The Nix flake and a GitHub release, under the name `seki` | Operator |
| M4-D07 | How the first start hands over to the built configuration | The launcher execs the last profile or `seki-default` without building. That image runs the `recompile` build session once and execs a new validated candidate with the M2 restore handshake | M4-02 architect pass and lanes 1 to 8 |

## Appendix B. Alternatives rejected

The operator chose the flake and a GitHub release over Hackage and a plain-cabal install on 2026-11-22. The launcher assumes a Nix-provided archive and compiler, and `examples/config/cabal.project` is a template with `@ARCHIVE@` and `@COMPILER@`. A non-Nix channel would need new launcher code. M4 therefore drops the `werror` flag, the flag for the development executables, and GHC 9.12 support. GHC 9.10.3 stays the only supported compiler, because the flake pins it.

The operator moved per-output workspaces out of M4 on 2026-11-22. They need a unique workspace identity, a rule for bare tags in `view` and `doShift`, an IPC change, and an unplug rule, and none of that is needed for a first release.

A built-in prompt is outside M4. It needs a shell surface, shm buffers, and text rendering, a stack seki does not have. External launchers through layer-shell already work.

A separate timer package with a test-only timer loses to delivering timers with binding repeat. Repeat is the first real timer, so M4-03 tests real behavior and adds no test-only code to the runtime.

Sending timer fires through `submitExternal` loses to a wake through `signalExternal`. The bounded queue returns `Full` at 256 entries, and a dropped fire would leave a timeout submap active with no later wake. Due timers already live in `CoreState`, so the wake only needs to make `manage_start` look at the clock. The build session's result uses the same path today.

Registering one binding object per submap and chord loses to one object per chord. River's choice between duplicate bindings is compositor policy, and the M1 configuration already rejects duplicates. One object per chord keeps that rule and makes the active map decide in the core.

A runtime-owned timer table loses to desired timers in `CoreState`. Every other effect in seki is desired state that the reconciler realizes. Timers follow the same path, so restart, session ties, and races reuse the M3-01 ingress proofs.

`OneShot` is the default exit rule because xmonad's `XMonad.Actions.Submap` behaves that way, and M4's users port xmonad configurations. `Sticky` covers modal submaps such as a resize mode.

A 20% regression ratio on sequence p99 loses to an absolute margin on the median. Five trials of one revision already span 31% from lowest to highest, so a ratio would fail correct packages.

Two changes to the layout class in consecutive packages lose to one change in M4-06. Per-window borders and per-workspace layouts both change what `pureLayout` sees, and one architect pass designs both.

Running packages in parallel loses to one linear stack. M4-02 through M4-07 all edit `Config.hs`, `Seki.hs`, `Policy.hs`, or `seki.cabal`, so parallel owners would wait on the same files.

The open latency issues #87 and #88 and the question of running `tools/latency/test_report.py` in CI stay outside M4. They concern measurement, not the release. M4's perf gates use the same report, so a fix that lands meanwhile only makes them stricter.

The contrib set is a first cut chosen for a first release. It is two layouts, four layout modifiers, PerWorkspace, the CycleWS family, and two ManageHook placements. Tabbed layouts need drawing, and GridSelect needs a prompt, so both wait. The operator can change the list before M4-06 starts.

## Appendix C. Risks

| Risk | PR | What the owner watches |
|---|---|---|
| The first-start handover runs the startup hook twice or loses windows | M4-02 | Lanes 4 and 5 |
| A seat or output lost during the first build leaves the built profile without outputs | M4-02 | Lane 7 |
| Binding the bindings seat changes how river routes keys to fcitx5 | M4-03 | Lane 9 runs the exact Japanese commit |
| Disabling bindings mid-sequence races a key press, or leaks the entry chord's release | M4-04 | M4-D03 runs before code, and lanes 6 and 7 |
| Pointer entries from layout changes steal focus | M4-05 | M4-D04 runs before any code depends on it |
| The layout class change breaks existing layouts | M4-06 | Lane 1 compares Tall geometry byte for byte |
| A user's first install needs Nix knowledge the README does not give | M4-08 | Lanes 1 and 2 run in a fresh `HOME` |
| Timing lanes flake on a busy host | Every PR | Run lanes one at a time and record host load next to each perf run |

## Appendix D. Links and reading list

Read [spec.md](spec.md) sections 3.6 and 3.8 before M4-03 through M4-05, and section 5.2 before M4-02. Read [ipc.md](ipc.md) before M4-02 and M4-04. Read [latency.md](latency.md) before any perf gate. Read issue #90 before M4-02.

M4-02, M4-03, M4-04, and M4-06 change core types or the launcher and get `skills/architect/SKILL.md` before code. M4-02 and M4-03 also get `skills/how/SKILL.md` first. Run `skills/interrogate/SKILL.md` on M4-02 and M4-03, where a wrong shape is costly to undo.

Each owner keeps a decision trail per `skills/show-me-your-work/SKILL.md` and returns it with the report. The root's trail lives in `artifacts/m4/` and stays uncommitted.
