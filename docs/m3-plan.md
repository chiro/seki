# M3 Daily use implementation plan

Status is **Complete**. Updated 2026-11-10 against `8f7c75c`, the merge of #78.
M3 makes seki usable for daily work in place of xmonad.
It builds external event ingress, bar IPC and `sekictl`, input rules, scratchpads,
and configuration compilation in M3-01 through M3-07.
M2 is complete. Preserve its window management, restoration, and latency guarantees.
M3-01 landed through [PR #41](https://github.com/chiro/seki/pull/41) on 2026-10-31 as `ce800256f99e843cbfa84fc5488f43d7b90d8b66`. Its tree matches the reviewed head `5dc5da19098b28dd81d1db5d25113ba05ab8716a`.
M3-02 landed through PR #43 on 2026-11-02 at `dbab756a27bcf4566e10789a4ef5c79a64166124`. Its tree exactly matches reviewed `9e60f13`; operator-review fixes are included.
M3-03, M3-04, and M3-06 landed together through PR #44 on 2026-11-03 as `6d349f5729b62522dbdd6ab6dc435da55b50c762`. PR #45 (M3-04) and PR #46 (M3-06) merged into the #44 stack before it landed.
The operator deferred M3-05, automatic client urgency, from M3 on 2026-11-03. River has no supported client signal for it (M3-D01), and M3 ships no manual substitute.
M3-07 is three stacked PRs, which the operator merges in this order.
[PR #68](https://github.com/chiro/seki/pull/68) adds the daily-use configuration and its real-river lanes. It merged on 2026-11-09 as `1545eee900b14e380d7b153e1e0890ec6b4fbc4c`, and its tree matches the reviewed head `b72b2b1`.
[PR #72](https://github.com/chiro/seki/pull/72) adds the m3 latency workload and the strict campaign. It merged on 2026-11-09.
[PR #75](https://github.com/chiro/seki/pull/75) commits this plan and brings the documentation up to date. It merged into #72's stale branch, and [PR #76](https://github.com/chiro/seki/pull/76) re-landed it on main the same day.
[PR #70](https://github.com/chiro/seki/pull/70) made the installed `config-delivery` product ship `sekictl`. It landed on 2026-11-09 as `3c3037369eed28e5dc59528046afa4b7a1309cae`.
M3 is complete. The operator reviewed the M3-07 media, passed lanes 9 and 10 on 2026-11-10, and recorded full migration from xmonad in the [operator acceptance record](#operator-acceptance-record). [PR #77](https://github.com/chiro/seki/pull/77) and [PR #78](https://github.com/chiro/seki/pull/78) fixed the two seki gaps the lanes found.
Execution was authorized on 2026-10-31. Unresolved design gates still apply. The operator retains merge authority.

## How to read this

One box is one unit of work. Every box names the evidence that checks it.
Check a box only when its evidence exists in a test log, capture, report, or commit.
Package IDs name work packages, not GitHub PR numbers. The [coverage ledger](#record-package-coverage) maps each package to its PRs.
Module paths and scenario names below were proposals when the plan was written. The code and [integration-testing.md](integration-testing.md) have the shipped names.
The coverage ledger is the authoritative status for M3-01 to M3-06. Their per-box checklists, and the recurring boxes in the program checklist, were not back-filled after those packages merged.
Read current behavior in [the specification](spec.md) and [the M2 plan](m2-plan.md).

Execution follows `skills/poteto-mode/playbooks/autopilot-stack.md` from the installed plugin.
The operator merges every PR. Owners stop at merge-ready. Start implementation only on an explicit go.
Work runs serially. Deliver one linear stack in this order: M3-01, M3-02, M3-03, M3-04, M3-06, M3-05, M3-07.
Each package branches from the head of the package before it. Start the next package only after the previous one reaches merge-ready.
Run the M3-D01 and M3-D04 probes before asking for the go, and present their results with this draft.
The go does not waive an unresolved design gate for dependent implementation.

Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.

Treat all new limits and response budgets below as proposed acceptance targets, not measurements.
Each 100 ms command target below is a ceiling. M3-D02 measures the idle `manage_dirty` round trip.
Set each package's target from that measurement before its perf gate runs, and record the chosen value with the reason.
Existing budgets remain pure p99 below 2 ms, manage and render sequence p99 below 10 ms,
and maximum observed stop-the-world pause below 10 ms, as defined in [latency.md](latency.md).
Use the same compiler, compositor, workload, collector, and host for revision comparisons.

## Program checklist

### Arm the program

- [x] Run the M3-D01 and M3-D04 probes and record their results in Appendix A. The probes ran locally on 2026-10-31. D01 remains blocked and D04 is partially proved.
- [x] Present this draft, the probe results, and its unresolved decisions before execution. The operator authorized work on 2026-10-31 at `f9b742380333884d8d3191f6d4601f110e911ec5`. Unresolved gates remain closed for dependent work.
- [ ] Put the plan path, package order, verification rule, operator merge authority, and M3 daily-use exit criterion in standing orders.
- [ ] Read the installed plugin's execution playbook, `skills/poteto-mode/playbooks/opening-a-pr.md`, and the leaf skills used by each package.
- [ ] If execution becomes unattended, establish an hourly audit through a supported scheduling tool. Record its handle. If none is available, report that limit and use attended execution without claiming an armed audit.
- [ ] Post a status message only for new evidence, a changed blocker, or a decision. Stop writes when the operator places the run on hold.

### Spawn owners

- [ ] Run one owner at a time in stack order. Start M3-05 only after M3-06 reaches merge-ready and M3-D01 is resolved.
- [ ] Give the owner an isolated checkout that starts at the previous package's head.
- [ ] Use the scopes in each package. Record any scope change before editing another package's files.
- [ ] Keep M3-02 through M3-07 review-gated. The operator inspects captures and a video before merging each interaction change.

### PR mechanics, for every PR

- [ ] Resolve the forge and base through the execution playbook. Open each PR with its dependency and verification evidence. Link every created or pushed PR in the host.
- [ ] Run build, tests, formatter, package, phase-type, and affected integration checks before the PR-facing push. Apply `deslop` before commit and `no-comments` before review.
- [ ] Triage automated review comments with evidence. Record the exact base and head SHAs for the verdict.

### Verdict and merge, for every PR

- [ ] Run the package's ten live lanes one at a time at the code-ready head, then its performance gate. Run two independent audits, one for correctness and one for maintainability, one after the other. Check their receipts before handing back.
- [ ] Record trunk failures separately. Missing required independent reviewers block merge-ready. Retain evidence still valid under the execution playbook's revision rules.
- [ ] Stop at merge-ready for the operator. Revalidate changed behavior after fixes and resolve every blocking finding before the operator merges.

### Boot recipe, for every live lane

- [ ] Check out the exact recorded head in an isolated worktree. Use [integration-testing.md](integration-testing.md) as the current CLI driver. No project driver skill is installed.
- [ ] Run `nix develop --no-write-lock-file .#ci --command ci/run-integration.sh` for the release gate. For a focused future scenario, set `SEKI_INTEGRATION_SCENARIOS` to the package's registered scenario name first.
- [ ] Add each new scenario to `integration/Seki/Integration/M3.hs`, `integration/Main.hs`, and `ci/expected-integration-manifest.txt` in the same PR. The full release manifest must include all existing scenarios.
- [ ] Drive commands through the built `sekictl`, bindings through `wtype`, pointer actions through the existing native probe, and client changes through the fixture. Read inventory and traces only as diagnostics.
- [ ] Extend the fixture to save successful captures as PNG as well as failure diagnostics. Current failure captures are PPM. Save each lane in its own scratch directory outside the repository and attach the command transcript alongside its named PNG.
- [ ] Run one compositor fixture at a time. Never drive the maintainer's live desktop during automated tests.

## Wake the runtime for external work (M3-01)

**Depends on.** None.

**Files.**

- [ ] Edit `src/Seki/Run/Runtime.hs`, `src/Seki/Run/Policy.hs`, and their tests.
- [ ] Create `src/Seki/Run/External.hs` for bounded ingress and session lifecycle ownership.
- [ ] Edit `seki.cabal` and the integration fixture for new modules and evidence capture.

**Build.**

- [ ] Settle M3-D02 before freezing the wake path. The current transport is nonblocking and waits with `threadWaitRead` and `threadWaitWrite`.
- [ ] Keep every worker free of blocking foreign calls. `executable seki` links the non-threaded RTS, where a safe blocking call such as `waitForProcess` stops every Haskell thread. `launchDetached` in `Policy.hs` polls for this reason. Any RTS change belongs to M3-D04, because the generated skeleton's Cabal file sets the RTS of the user binary.
- [ ] Represent external inputs as commands or completion events. Enqueue from workers, then execute actions through the existing reducer at `manage_start`.
- [ ] Keep one Wayland dispatcher and one owner of core state. A worker enqueues its command, then submits `manage_dirty`. The protocol allows `manage_dirty` at any time, including during a sequence. It is the only request a worker sends. Workers never mutate policy.
- [ ] Bound pending commands at 256 and drain at most 32 per manage sequence as initial targets. Continue pending work through a later wake. Reject overload explicitly.
- [ ] Prove no lost wake across idle, drain, render, and shutdown races. Tie workers and queues to the session so completions from an old session cannot enter a restored session.

**You see.**

- [ ] An external action completes while Wayland is idle, without a mouse movement or key press. Save the scenario transcript and capture.

**Verify, unit.**

- [ ] Extend `test/Seki/Run/RuntimeTest.hs`, `PolicyTest.hs`, and `MockRiver.hs` for wake coalescing, FIFO order, bounded draining, and shutdown. Run `cabal test seki-test`. Retain test output at the recorded head.

**Verify, live.** Ten lanes, run one at a time at the PR head, using the boot recipe and proposed `m3-external-wake` scenario family.

- [ ] Lane 1. Regression lane against trunk. Inject an external workspace action on idle river at trunk and head. Trunk lacks external ingress. Gate head completion without physical input. Save `external-wake.png`. Pass when head changes workspace and the trace contains one legal manage/render sequence.
- [ ] Lane 2. Queue commands during an active manage sequence. Save `manage-queue.png`. Pass when arrival order survives and all actions run inside manage.
- [ ] Lane 3. Queue work during render. Save `render-queue.png`. Pass when it runs in the following manage sequence.
- [ ] Lane 4. Fill the queue beyond its limit. Save `queue-limit.png`. Pass when overflow is reported and normal key focus still works.
- [ ] Lane 5. Race enqueue with the final drain. Save `drain-race.png`. Pass when every accepted command completes once.
- [ ] Lane 6. Send a sustained producer burst. Save `burst.png`. Pass when bounded batches allow probe windows to resize and receive focus.
- [ ] Lane 7. End the compositor with queued work. Save `shutdown.png`. Pass when workers exit and the session closes cleanly.
- [ ] Lane 8. Restart with a delayed completion. Save `old-session.png`. Pass when the old completion cannot change restored state.
- [ ] Lane 9. Remove all outputs and deliver an action. Save `headless.png`. Pass when no deadlock occurs and output reattach restores normal work.
- [ ] Lane 10. Run layer focus and pointer-drag regressions. Save `m2-regression.png`. Pass when existing focus protection and drag behavior pass.

**Verify, perf.**

- [ ] Metric. Idle external-action completion latency and the unchanged M2 sequence distributions.
- [ ] Probe. Add an ingress probe to the M3 fixture. Timestamp enqueue through observed workspace state, outside screenshots. Interleave identical M2 latency trials on trunk and head, then run the new ingress probe at head.
- [ ] Baseline. Record trunk metrics first with SHA, toolchain, host, workload, collector, and raw samples. Record unsupported trunk features explicitly.
- [ ] Rule. Require all accepted finite-burst actions to complete. Target idle-action p99 below the value set from M3-D02, at most 100 ms, over at least 1000 actions. Keep every existing latency budget. Trunk has no ingress, so report no ingress speedup.

**Review gate.** None. M3-01 is not review-gated. It adds runtime ingress without a public interaction.

**Merge.**

- [ ] Record a clean independent verdict at the exact head, complete review-bot triage, and confirm current dependencies. Stop at merge-ready for the operator.

## Publish bar state and accept sekictl commands (M3-02)

**Depends on.** M3-01.

**Files.**

- [ ] Create `src/Seki/Run/IPC.hs`, a pure snapshot module, and `tools/sekictl/app/Main.hs`.
- [ ] Edit `src/Seki/Run/Policy.hs`, `Inventory.hs`, `seki.cabal`, and integration registration.
- [ ] Create `docs/ipc.md` and a minimal Waybar custom-module example.
- [ ] Record each library dependency the IPC code adds to `seki-wire`. User configurations link that library, and it does not depend on `aeson` today. M3-06 repeats the external build with these dependencies.

**Build.**

- [ ] Define versioned JSON Lines request, reply, and snapshot envelopes in `docs/ipc.md` before implementing them. Include request IDs and a monotonic snapshot revision.
- [ ] Serve `$XDG_RUNTIME_DIR/seki/ipc.sock` from a user-owned runtime directory. Check socket ownership and liveness before replacing a stale endpoint. A second live manager must not unlink the first socket.
- [ ] Publish an initial snapshot on subscribe and a new revision for core or relevant inventory changes. Include outputs, configured workspace order, occupied/visible/focused flags, layout description, and nullable focused title.
- [ ] Represent unavailable urgent and XKB state explicitly. Use null for deferred submap state. Fill urgency in M3-05 and keyboard layout in M3-03. Never imply unsupported means false.
- [ ] Implement `sekictl` snapshot, subscribe, workspace, spawn, and layout commands through M3-01. Add named scratchpad commands in M3-04. Acknowledge actions after reducer application and identify the revision; do not claim a frame is displayed yet.
- [ ] Limit a line to 64 KiB, command backlog to M3-01 limits, connections to 32, and subscriber output to one replaceable pending snapshot. Disconnect malformed or stalled clients without blocking the WM.
- [ ] Document reconnect after restart, command ambiguity on disconnect, and no automatic replay of spawn or toggles. Escape JSON and bar markup as separate boundaries.
- [ ] Keep client cancellation separate from session-owned wake submission. M3-01 closes ingress if the thread owning a wake fails or is cancelled. Disconnecting a client must not cancel that thread or disable other clients. Document and test this lifecycle boundary.

**You see.**

- [ ] Waybar shows the active workspace, layout, and title. `sekictl workspace 3` changes the workspace without keyboard input. Save the scenario transcript and capture.

**Verify, unit.**

- [ ] Add IPC framing, schema, pure projection, and CLI tests with fragmented lines, bad UTF-8, missing titles, and invalid commands. Run `cabal test all`. Retain test output at the recorded head.

**Verify, live.** Ten lanes, run one at a time at the PR head, using the boot recipe and proposed `m3-ipc` scenario family.

- [ ] Lane 1. Regression lane against trunk. Run the existing workspace-switch workload at trunk and head, then invoke the new command at head. Save `ipc-regression.png`. Pass when both retain workspace behavior and head changes it through sekictl.
- [ ] Lane 2. Subscribe before creating a window. Save `subscribe.png`. Pass when one initial snapshot and later occupied/title updates arrive.
- [ ] Lane 3. Change only a window title. Save `title.png`. Pass when snapshot revision and escaped title change without a core action.
- [ ] Lane 4. Leave a subscriber unread during 1000 changes. Save `slow-reader.png`. Pass when other commands and keyboard focus continue.
- [ ] Lane 5. Send fragmented and oversized lines. Save `framing.png`. Pass when valid fragments parse once and oversized input disconnects only that client.
- [ ] Lane 6. Start a second server against a live socket. Save `socket-owner.png`. Pass when the first server remains reachable.
- [ ] Lane 7. Reconnect after a real restart. Save `reconnect.png`. Pass when subscriber receives a fresh initial snapshot without duplicate spawn.
- [ ] Lane 8. Switch two outputs and workspaces. Save `multi-output.png`. Pass when workspace flags and output names match displayed windows.
- [ ] Lane 9. Map an exclusive layer surface. Save `layer-focus.png`. Pass when snapshot distinguishes workspace focus from keyboard availability.
- [ ] Lane 10. Run the Waybar example with adversarial title text. Save `waybar.png`. Pass when title cannot alter markup or commands and displayed state is correct.

**Verify, perf.**

- [ ] Metric. Command-to-observed-state and change-to-subscriber p99, plus M2 sequence distributions with a stalled subscriber.
- [ ] Probe. Add timed IPC probes and record 1000 acknowledged updates. Interleave the same M2 workload at trunk and head. Run head with zero, one, and 32 clients, including an unread subscriber.
- [ ] Baseline. Record trunk metrics first with SHA, toolchain, host, workload, collector, and raw samples. Record unsupported trunk features explicitly.
- [ ] Rule. Target both IPC p99 values below the value set from M3-D02, at most 100 ms, on the controlled fixture. Keep the existing sequence and GC budgets at 32 clients. No ratio compares missing trunk IPC with head IPC.

**Review gate.** The operator reviews before merge.

- [ ] Post the lane screenshots and a 30 to 60 second video of M3-02 behavior in chat. Save them under `artifacts/m3/M3-02/review/`. Wait for the operator's review.

**Merge.**

- [ ] Record a clean independent verdict at the exact head, complete review-bot triage, and confirm current dependencies. Stop at merge-ready for the operator.

## Apply device rules and report keyboard layout (M3-03)

**Depends on.** M3-02. Resolve M3-D03 before freezing the public rule API.

**Files.**

- [ ] Vendor the three input XMLs under `protocol/river/` and update hashes, source records, notices, and generated bindings.
- [ ] Create input inventory and adapters under `src/Seki/Run/` and a pure rule model under `src-core/Seki/Internal/`.
- [ ] Edit public configuration, `src-public/Seki.hs`, scanner freshness tests, `seki.cabal`, and input integration scenarios.

**Build.**

- [ ] Pin `river-input-management-v1`, `river-libinput-config-v1`, and `river-xkb-config-v1` to river `v0.4.5`, the tag of the vendored river protocols. Record whether each file is byte-identical in river 0.5.0, which the release integration environment runs. If any file differs, choose the tag in M3-D03 before generating bindings. Generate bindings through the existing scanner. Add freshness and source-distribution coverage.
- [ ] Propose ordered rules using an explicit device-name matcher and optional device type. Merge matching settings in declaration order, with the last specified value winning. Validate the supported pattern syntax before connecting.
- [ ] Apply generic repeat, scroll, and touch/tablet output mapping through input management. Keep seat assignment out under the existing single-seat policy.
- [ ] Wait for support discovery before libinput requests. Model absent device support, unsupported settings, invalid settings, successful application, and removal separately. Publish structured results through logs and IPC.
- [ ] Cover the full configuration surface named by spec section 3.9, including send-events, tap, acceleration profiles, and custom acceleration arrays. Scope native-endian IEEE-754 Double conversion to the libinput adapter. Reject malformed lengths and non-finite values.
- [ ] Set per-device XKB keymaps through owned FDs and expose active layout, caps lock, and num lock in snapshots. Close descriptors after the protocol-defined lifetime.
- [ ] Treat missing optional globals as unavailable capability. If a configured rule needs them, report that rule as unapplied without silently succeeding or preventing the core WM from running.
- [ ] Reapply on device add and restart. Avoid feedback loops from current-value events. Map outputs by stable name and define behavior while a requested output is absent.

**You see.**

- [ ] Configured keyboard settings apply after hotplug, IPC reports applied or unsupported settings, and the bar updates its keyboard-layout indicator. Save the scenario transcript and capture.

**Verify, unit.**

- [ ] Extend scanner freshness tests. Add rule precedence, support/result lifecycle, array encoding, FD ownership, malformed values, and removed-device tests. Run `sha256sum --check protocol/SHA256SUMS` and `cabal test all`. Retain test output at the recorded head.

**Verify, live.** Ten lanes, run one at a time at the PR head, using the boot recipe and proposed `m3-input` scenario family.

- [ ] Lane 1. Regression lane against trunk. Run keyboard bindings at trunk and head, then apply a rule at head. Save `input-regression.png`. Pass when bindings still work and the requested supported setting is observed.
- [ ] Lane 2. Add a virtual keyboard after WM startup. Save `input-hotplug.png`. Pass when repeat and XKB rules apply once the device is ready.
- [ ] Lane 3. Switch between two XKB layouts. Save `layout.png`. Pass when bar state follows the reported active layout and fixed-layout bindings remain usable.
- [ ] Lane 4. Submit a rule for a non-libinput device. Save `non-libinput.png`. Pass when generic settings work and libinput settings are reported unavailable.
- [ ] Lane 5. Return unsupported and invalid result objects in the protocol fixture. Save `input-result.png`. Pass when IPC and logs name the device, setting, and failure.
- [ ] Lane 6. Remove a device during configuration. Save `input-remove.png`. Pass when outstanding objects are retired without stale requests.
- [ ] Lane 7. Run without optional input globals in the protocol fixture. Save `missing-global.png`. Pass when the core WM works and requested input rules report unavailability.
- [ ] Lane 8. Change mapping while the target output disappears. Save `mapping.png`. Pass when mapping follows the recorded absence/readd policy.
- [ ] Lane 9. Use a physical touchpad on the maintainer-approved test session. Save `touchpad.png`. Pass when tap, scroll, and acceleration settings match observed behavior and device readback.
- [ ] Lane 10. Restart and hotplug the physical keyboard or pointing device. Save `physical-restart.png`. Pass when rules reapply and normal input remains usable.

**Verify, perf.**

- [ ] Metric. Device-ready-to-rule-result latency, layout-event-to-snapshot latency, and unchanged M2 sequence distributions.
- [ ] Probe. Add lifecycle timestamps to the input fixture. Interleave the M2 workload on trunk and head with the same virtual devices. Separately record physical-device results with device model and compositor revision.
- [ ] Baseline. Record trunk metrics first with SHA, toolchain, host, workload, collector, and raw samples. Record unsupported trunk features explicitly.
- [ ] Rule. Target supported virtual-device rule completion and layout publication p99 below the value set from M3-D02, at most 100 ms, after the protocol discovery barrier. Exclude device discovery and keymap compilation from that interval and report them separately. Keep existing budgets. Physical checks remain required evidence without an invented timing baseline.

**Review gate.** The operator reviews before merge.

- [ ] Post the lane screenshots and a 30 to 60 second video of M3-03 behavior in chat. Save them under `artifacts/m3/M3-03/review/`. Wait for the operator's review.

**Merge.**

- [ ] Record a clean independent verdict at the exact head, complete review-bot triage, and confirm current dependencies. Stop at merge-ready for the operator.

## Toggle named scratchpads without exposing storage workspaces (M3-04)

**Depends on.** M3-02.

**Files.**

- [ ] Edit `src-core/Seki/Internal/Core/Action.hs`, `Event.hs`, `State.hs`, `Scene.hs`, and `Restore.hs`.
- [ ] Edit internal/public configuration, `src-public/Seki.hs`, runtime policy, and IPC command lowering.
- [ ] Create scratchpad properties, public-API examples, and real-river scenarios.

**Build.**

- [ ] Define each scratchpad by unique name, launch command, explicit window query, and output-relative float geometry. Reject duplicate names. Match with the existing appId/title queries rather than PID heuristics.
- [ ] Track each definition as absent, launch-pending, hidden, or shown with a stable window identity. Choose the first matching managed window deterministically. Leave other matches as ordinary windows and report the ambiguity.
- [ ] On toggle, launch an absent pad once, show a hidden pad on the current workspace, or hide a shown pad there. If shown elsewhere, bring it to the current workspace. Coalesce repeated toggles while launch is pending.
- [ ] Use dedicated hidden storage outside configured workspace navigation and bar occupancy. Keep hidden pads out of layout and focus. On hide, select the prior eligible workspace focus.
- [ ] Clamp geometry using existing float rules. Exit fullscreen before hide or move. Handle output removal, window close, and definition removal without losing an ordinary window.
- [ ] Persist scratchpad ownership and shown/hidden state by identifier through an explicit restore-schema revision. Do not restore pending process launches as live windows. Reuse geometry and restoration rules from M2.
- [ ] Document precedence between scratchpad matching and user manageHook, launch failure/timeout recovery, and changed definitions at restore. Settle these examples before freezing the API.

**You see.**

- [ ] A named terminal opens once, hides, and returns on the current output. It survives restart without appearing in ordinary workspace navigation. Save the scenario transcript and capture.

**Verify, unit.**

- [ ] Add scratchpad state-machine properties and traces in core tests, restore-version tests, and a public-only configuration compile test. Run `cabal test all`. Retain test output at the recorded head.

**Verify, live.** Ten lanes, run one at a time at the PR head, using the boot recipe and proposed `m3-scratchpad` scenario family.

- [ ] Lane 1. Regression lane against trunk. Run ordinary float/show/hide workflows at trunk and head, then toggle a pad at head. Save `scratch-regression.png`. Pass when ordinary floats remain unchanged and the pad returns without relaunch.
- [ ] Lane 2. Toggle repeatedly before the process maps. Save `pending.png`. Pass when only one launch occurs.
- [ ] Lane 3. Hide while another tiled window can receive focus. Save `hide-focus.png`. Pass when the tiled window receives input and the hidden pad is not rendered.
- [ ] Lane 4. Bring a pad from the other visible output. Save `bring-pad.png`. Pass when one window moves to the current workspace with clamped geometry.
- [ ] Lane 5. Switch Tall, Mirror, and Full with a visible pad. Save `pad-layout.png`. Pass when the pad remains a float above tiles.
- [ ] Lane 6. Hide a fullscreen pad. Save `pad-fullscreen.png`. Pass when fullscreen exits and other workspace windows become usable.
- [ ] Lane 7. Restart with one shown and one hidden pad. Save `pad-restart.png`. Pass when identities, geometry, and visibility survive without new processes.
- [ ] Lane 8. Close and toggle a pad again. Save `pad-close.png`. Pass when close clears ownership and the next toggle launches once.
- [ ] Lane 9. Create two windows that match one definition. Save `pad-duplicate.png`. Pass when ownership is deterministic and the extra window stays usable.
- [ ] Lane 10. Remove a definition and an output before restoring. Save `pad-config-change.png`. Pass when no saved window is lost in inaccessible storage.

**Verify, perf.**

- [ ] Metric. Existing-pad toggle-to-visible-or-hidden p99, plus M2 sequence distributions.
- [ ] Probe. Time 1000 alternating toggles through observed visibility and input ownership. Interleave the same M2 float workload on trunk and head. Report process launch-to-map time separately.
- [ ] Baseline. Record trunk metrics first with SHA, toolchain, host, workload, collector, and raw samples. Record unsupported trunk features explicitly.
- [ ] Rule. Target existing-pad toggle p99 below the value set from M3-D02, at most 100 ms. Do not apply that target to an arbitrary external application startup. Keep existing latency budgets and prove one launch during pending toggles.

**Review gate.** The operator reviews before merge.

- [ ] Post the lane screenshots and a 30 to 60 second video of M3-04 behavior in chat. Save them under `artifacts/m3/M3-04/review/`. Wait for the operator's review.

**Merge.**

- [ ] Record a clean independent verdict at the exact head, complete review-bot triage, and confirm current dependencies. Stop at merge-ready for the operator.

## Report urgency from a demonstrated signal source (M3-05)

**Deferred from M3 by the operator on 2026-11-03.** No automatic client urgency or manual IPC substitute is delivered. The requirements below remain future work, not current M3 exit criteria.

**Depends on.** M3-02 and a verified M3-D01 resolution. This package is blocked until then.

**Files.**

- [ ] Edit input inventory only where the verified signal enters, then core urgency state, scene border selection, public configuration, and IPC snapshots.
- [ ] Add source-specific client probes and urgency tests.
- [ ] Update `docs/spec.md` with the verified source, minimum versions, and native/Xwayland coverage.

**Build.**

- [ ] Do not invent an urgency event in river-window-management-v1 v4. Complete M3-D01 with a source that can identify the correct managed window and a real client that triggers it.
- [ ] If a newer compositor/protocol is necessary, propose the minimum-version change and compatibility behavior before implementation. If no source exists, retain this blocker and request an explicit milestone scope decision.
- [ ] An explicit IPC mark-urgent command may support user scripts, but it does not substitute for client urgency without the operator accepting that scope change.
- [ ] After the source is proved, define urgent-window state beside StackSet, clear it on the documented focus acknowledgement, and remove it on close. Urgency alone must not steal focus.
- [ ] Select border precedence as focused, then urgent, then unfocused. Preserve exclusive layer-focus protection. Publish aggregate urgent flags for hidden and visible workspaces.
- [ ] Define whether transient urgency survives restart. Demonstrate re-enumeration or persist stable identifiers with schema migration rather than guessing from app IDs or PIDs.

**You see.**

- [ ] A supported client requests attention on a hidden workspace. Its bar indicator becomes urgent without changing focus, then clears on acknowledgement. Save the scenario transcript and capture.

**Verify, unit.**

- [ ] Add source decoding, urgent-set cleanup, border precedence, workspace aggregation, and restore tests. Run `cabal test all`. Synthetic events verify policy only, not compositor support. Retain test output at the recorded head.

**Verify, live.** Ten lanes, run one at a time at the PR head, using the boot recipe and proposed `m3-urgent` scenario family.

- [ ] Lane 1. Regression lane against trunk. Run ordinary focus and border behavior at trunk and head, then trigger supported urgency at head. Save `urgent-regression.png`. Pass when ordinary focus is unchanged and real urgency is observed without focus theft.
- [ ] Lane 2. Request attention from a native client on a hidden workspace. Save `urgent-native.png`. Pass when the correct workspace flag changes if native coverage is part of the approved contract.
- [ ] Lane 3. Request attention from an Xwayland client. Save `urgent-xwayland.png`. Pass when the approved Xwayland contract is observed and recorded.
- [ ] Lane 4. Focus an urgent window. Save `urgent-clear.png`. Pass when the documented acknowledgement clears the flag.
- [ ] Lane 5. Close an urgent window before focusing it. Save `urgent-close.png`. Pass when no stale workspace flag survives.
- [ ] Lane 6. Map an exclusive layer surface during urgency. Save `urgent-layer.png`. Pass when urgency does not take keyboard focus away.
- [ ] Lane 7. Make an unfocused visible float urgent. Save `urgent-float.png`. Pass when its urgent border follows the chosen precedence.
- [ ] Lane 8. Restart while a window is urgent. Save `urgent-restart.png`. Pass when the approved restore rule is observed.
- [ ] Lane 9. Change title and app ID while attention is pending. Save `urgent-identity.png`. Pass when the stable window remains the urgency target.
- [ ] Lane 10. Run with the signal source unavailable. Save `urgent-unavailable.png`. Pass when capability is reported unavailable rather than falsely reporting working client urgency.

**Verify, perf.**

- [ ] Metric. Real attention-signal-to-snapshot p99 and unchanged M2 sequence distributions.
- [ ] Probe. Timestamp only after the verified source receives a real attention signal. Run repeated attention/acknowledgement trials. Interleave the M2 workload at trunk and head and record native and Xwayland coverage separately.
- [ ] Baseline. Record trunk metrics first with SHA, toolchain, host, workload, collector, and raw samples. Record unsupported trunk features explicitly.
- [ ] Rule. Target supported-source notification p99 below the value set from M3-D02, at most 100 ms, after source delivery. Keep existing latency budgets. An unavailable source fails this package rather than producing a synthetic performance claim.

**Review gate.** The operator reviews before merge.

- [ ] Post the lane screenshots and a 30 to 60 second video of M3-05 behavior in chat. Save them under `artifacts/m3/M3-05/review/`. Wait for the operator's review.

**Merge.**

- [ ] Record a clean independent verdict at the exact head, complete review-bot triage, and confirm current dependencies. Stop at merge-ready for the operator.

## Build and check user configurations without stopping the WM (M3-06)

**Depends on.** M3-02. Resolve M3-D04 before choosing package distribution and launcher ownership.

**Files.**

- [ ] Edit `tools/seki/app/Main.hs`, `src/Seki/Run/Config.hs`, and `Restart.hs`.
- [ ] Create config-build lifecycle and skeleton modules, packaged example files, and CLI tests.
- [ ] Edit `seki.cabal`, package checks, public-API tests, and real-river restart scenarios.

**Build.**

- [ ] Prove a generated user project builds outside this checkout against the packaged public seki API. Repeat the M3-D04 proof with the dependencies that M3-02 added. Record compiler/package provenance, dependency source, first-build network requirements, and warm-cache offline behavior.
- [ ] Generate the skeleton's Cabal file with the RTS that M3-D04 chooses for the user binary.
- [ ] Use `$XDG_CONFIG_HOME/seki/`, falling back to `~/.config/seki/`, for `Main.hs` and its Cabal project. Generate missing skeleton files without overwriting existing files. Ship the skeleton in the source distribution.
- [ ] Distinguish the installed launcher, user configuration binary, and last successful build. Parse command options once so a user binary cannot recursively invoke the launcher or rebuild on `--restore`.
- [ ] Provide `--check` without opening Wayland, running startupHook, spawning commands, or restarting. Document whether the implementation type-checks or builds extra artifacts and return nonzero on failure.
- [ ] Provide `--recompile` and the configured restart action through an asynchronous build lifecycle. Keep the WM responsive, coalesce duplicate build requests, bound subprocess output in memory, and retain the full build log in the user state directory.
- [ ] Wait for the build subprocess without a blocking foreign call. Under the non-threaded RTS, poll its exit as `launchDetached` does and read its output through nonblocking handles.
- [ ] On failure, retain the current process and state and report the error/log path through IPC and stderr. Handle compiler absence, missing package, disk failure, and an unwritable log directory.
- [ ] On success, resolve the built executable through Cabal and validate it. Make it the restart target by the method M3-D04 chooses. Prefer an atomic repoint of the symlink that the recorded argv[0] names, so the existing exec path reaches the new build unchanged. Reuse M2 stop/finished/save/exec/reconnect handover. Do not serialize before build success or stop for a failed build.
- [ ] Before changing the restart target or sending `stop`, run the candidate's policy validation without opening Wayland or running hooks. A successful compile is insufficient. Reject duplicate workspace tags, conflicting bindings, and invalid configuration values while the current WM keeps running.
- [ ] On initial unbuilt or invalid configuration, attempt the shipped default WM fallback and report why. Keep fallback separate from an already running configuration, which must continue on build failure.
- [ ] After exec failure, restore the running session in-process. Recreate IPC ownership and input adapters without inheriting stale build workers or connections. Preserve M2 spawnOnce semantics.

**You see.**

- [ ] A configuration builds while focus and layout keys remain usable. Success activates the new configuration and restores windows. Failure leaves the running WM in place with a readable log. Save the scenario transcript and capture.

**Verify, unit.**

- [ ] Extend ConfigTest, RestartTest, SessionsTest, and packaged public-API compilation tests. Use temporary projects and a controllable compiler process for CLI status and error cases. Run `cabal test all` and `ci/check-package.sh`. Retain test output at the recorded head.

**Verify, live.** Ten lanes, run one at a time at the PR head, using the boot recipe and proposed `m3-recompile` scenario family.

- [ ] Lane 1. Regression lane against trunk. Run M2 restart on trunk and head, then compile a changed configuration at head. Save `build-regression.png`. Pass when existing restart works and new settings appear with preserved window state.
- [ ] Lane 2. Start with no configuration in a temporary config directory. Save `skeleton.png`. Pass when the skeleton builds and existing unrelated files remain intact.
- [ ] Lane 3. Run --check without a compositor. Save `check-only.png`. Pass when valid input exits successfully and no WM or startup command starts.
- [ ] Lane 4. Compile a type error and a candidate that compiles but fails policy validation, such as duplicate workspace tags. Save `build-error.png` and both error logs. Pass when the old WM retains input, IPC reports the failure, and the restart target stays unchanged.
- [ ] Lane 5. Hold a long-running build and use focus/layout keys. Save `build-responsive.png`. Pass when all input actions complete before the build is released.
- [ ] Lane 6. Send duplicate recompile requests. Save `build-coalesce.png`. Pass when only the documented single build or queued rebuild runs.
- [ ] Lane 7. Build outside the checkout with a warm offline cache. Save `build-offline.png`. Pass when the packaged public API resolves and the artifact runs.
- [ ] Lane 8. Start with missing compiler/package or unwritable state directory. Save `build-fallback.png`. Pass when startup attempts the default WM and running sessions stay alive on failure.
- [ ] Lane 9. Force exec failure after successful compilation. Save `build-exec-fail.png`. Pass when in-process reconnect restores windows and recreates IPC.
- [ ] Lane 10. Recompile with removed workspace/layout/scratchpad definitions. Save `build-restore.png`. Pass when documented fallback placement preserves surviving windows and spawnOnce does not repeat.

**Verify, perf.**

- [ ] Metric. WM response latency during a held build and build-completion-to-restored-session latency, with compilation duration reported separately.
- [ ] Probe. Hold a deterministic compiler stub while running the same M2 actions on trunk and head. Separately compile a real external user project and record process launch, build completion, handover completion, and first observed new state.
- [ ] Baseline. Record trunk metrics first with SHA, toolchain, host, workload, collector, and raw samples. Record unsupported trunk features explicitly.
- [ ] Rule. Keep existing sequence and GC budgets during the held build. Target post-build handover below 1 second in the controlled fixture and no input stall of 100 ms or more caused by the build worker. Do not impose a deadline on arbitrary user compilation.

**Review gate.** The operator reviews before merge.

- [ ] Post the lane screenshots and a 30 to 60 second video of M3-06 behavior in chat. Save them under `artifacts/m3/M3-06/review/`. Wait for the operator's review.

**Merge.**

- [ ] Record a clean independent verdict at the exact head, complete review-bot triage, and confirm current dependencies. Stop at merge-ready for the operator.

## Prove the daily-use configuration and close M3 (M3-07)

**Depends on.** Accepted M3-01, M3-02, M3-03, M3-04 and M3-06. The operator explicitly deferred automatic native/Xwayland client urgency from M3 on 2026-11-03. M3-05 is outside this milestone; its technical source gate remains unsupported. Physical-device and operator daily-work acceptance remain mandatory unless separately changed by the operator.

**Files.**

- [x] Extend `test-public/PortedConfig.hs` and the reference executable with the accepted daily-use configuration. #68 adds `dailyConfig` and a separate `seki-daily-config` executable. The M2 reference executable stays unchanged, because lane 1 runs it on trunk and head.
- [x] Edit README, spec, IPC, wire-protocol, integration, reconciliation, and user-configuration documentation. Include the plan and user-facing examples in `seki.cabal` source-distribution documentation. #75 edits these documents and adds this plan, the M2 plan, and both latency reports to `extra-doc-files`. The Waybar and configuration examples were already in the source distribution.
- [x] Extend `tools/latency/` for the M3 workload and create `docs/latency-m3/` evidence and this plan's coverage ledger. #72 adds the m3 workload and `docs/latency-m3/`. #75 adds the M3-07 ledger row.

**Build.**

- [x] Port the remaining daily-use setup with Waybar, input rules, named scratchpads, and the configuration build flow. Document automatic client urgency as unavailable and explicitly deferred. Keep the example public-API-only and build with `-Werror`. #68 builds `seki-daily-config` with `-Werror` from public modules alone, and its module documentation says that automatic client urgency is unavailable. #75 records the deferral in the README, spec, and IPC documentation.
- [x] Run combined scenarios for bar reconnection, device hotplug, scratchpads, and build/restart while two outputs and floats are active. Include Japanese input and Xwayland regressions. #68 adds the seven `m3-daily-*` scenarios to the release manifest.
- [ ] Retain an unchanged M2 workload for valid trunk/head comparison. Add a separately named M3 workload that proves external wake, IPC update, scratchpad toggle, input-result reporting, and recompile completion through observable outcomes. Partly done. #72 keeps the m2 workload and times four of the five m3 outcomes. The latency fixture cannot report input results, so `m3-input-perf` measures that path instead.
- [x] Record one operator-confirmed daily-work session with compositor/toolchain versions, device models, exercised tasks, and any blockers. A green headless suite alone does not meet full migration from xmonad. This is an operator gate. See the [operator acceptance record](#operator-acceptance-record). The operator recorded full migration on 2026-11-10.
- [x] Update spec and README status only after every in-scope package, physical-device check, and exit criterion is verified. Link exact commits and reports in the coverage ledger. #75 marked M3 as software complete with daily-work acceptance pending. After lanes 9 and 10, the media review, #77, and #78, this change marks M3 Complete in the spec and README.

**You see.**

- [x] The maintainer can use the ported configuration for daily work, rebuild it safely, and rely on bar and input state across restarts. Save the scenario transcript and capture. This is an operator gate, which lane 10 records. Lane 10 passed on 2026-11-10. Rebuilding safely after a delivery upgrade needed #78.

**Verify, unit.**

- [ ] Run `cabal build all`, `cabal test all`, `fourmolu --mode check` on tracked Haskell files, `ci/check-package.sh`, `ci/check-phase-types.sh`, and `python3 tools/latency/test_report.py` in the pinned environment. Retain test output at the recorded head. Partly done. CI runs the build, the tests, the phase-type check, and the package check in the pinned Nix shell, and they pass at #72's head `5aaf32f`. `fourmolu` and `test_report.py` passed with GHC 9.10.3 outside the pinned environment, as #68 and #72 report.

**Verify, live.** Ten lanes, run one at a time at the PR head, using the boot recipe and proposed `m3-reference-config` scenario family.
The shipped lanes are `m2-reference-config` for lane 1 and the `m3-daily-*` scenarios for lanes 2 to 8. The root ran lanes 1 to 8 one at a time at `b52cc23`, which has the same patch as #68's round-2 head. Lanes 1 and 8 passed again at `b72b2b1`, and the root verdict on #68 records the runs. The lane PNGs and transcripts are on the maintainer's host under `artifacts/m3/M3-07/review/root-b52cc23/`, outside the repository.

- [x] Lane 1. Regression lane against trunk. Run the unchanged M2 reference configuration on trunk and head. Save `daily-regression.png`. Pass when every existing behavior and release scenario still passes. `m2-reference-config` passed at trunk `3c30373` and at head. This lane kept transcripts and manifests but no PNG. CI passes the full release manifest at `b72b2b1` and `5aaf32f`.
- [x] Lane 2. Use the public daily-use configuration on two outputs. Save `daily-two-output.png`. Pass when workspace, title, layout, and scratchpad bar state match the windows. `m3-daily-two-output` passed.
- [x] Lane 3. Recompile while a scratchpad and fullscreen client exist. Save `daily-build.png`. Pass when new configuration starts and all surviving state restores correctly. `m3-daily-build` passed. The lane stubs the build, so it proves the handover and an exact restore for a copy of the running image. M3-06 proved a changed image (M3-D04).
- [x] Lane 4. Reconnect Waybar after multiple restarts. Save `daily-bar.png`. Pass when fresh snapshots restore the bar without stale state or replayed actions. `m3-daily-bar` passed with a `sekictl subscribe` stream, which is what the Waybar example reads. Waybar itself is a logging stub in this lane, and `m3-ipc-waybar` covers real Waybar.
- [x] Lane 5. Hotplug input and outputs during daily configuration use. Save `daily-hotplug.png`. Pass when input results and output placement remain correct. `m3-daily-hotplug` passed. The virtual touchpad has no libinput, so its tap, click method, and natural scrolling results are `unavailable`. Lane 9 proves them.
- [x] Lane 6. Use Japanese input in foot after configuration handover. Save `daily-ime.png`. Pass when the expected Mozc text commits and no startup daemon duplicates. `m3-daily-ime` passed.
- [x] Lane 7. Use the Xwayland application for ordinary window-management regression. Save `daily-xwayland.png`. Pass when management remains correct and documentation clearly identifies automatic attention/urgency as deferred and unavailable; no urgency proof is credited. `m3-daily-xwayland` passed, and #75 documents the deferral.
- [x] Lane 8. Flood IPC while moving and resizing a floating window. Save `daily-load.png`. Pass when input remains responsive and subscribers cannot block rendering. `m3-daily-load` passed. Key-to-focus took 14.4 ms idle and 14.5 ms during the flood.
- [x] Lane 9. Exercise physical keyboard/touchpad configuration in an approved session. Save `daily-hardware.png`. Pass when the hardware acceptance record confirms intended settings and recovery. This is an operator gate. #68's description has the runbook. Passed on 2026-11-10, after #77 fixed keyboard focus after an unlock.
- [x] Lane 10. Complete an operator-confirmed daily-work session. Save `daily-session.png`. Pass when the maintainer records full migration or names a blocking gap. This is an operator gate. #68's description has the runbook. The maintainer recorded full migration on 2026-11-10.

**Verify, perf.**

- [x] Metric. Unchanged M2 latency distributions and a separately identified M3 command-to-outcome distribution. [The M3 report](latency-m3/report.md) gives the m2 distributions at trunk and head and the m3 endpoint distributions in a separate table.
- [x] Probe. Use `python3 tools/latency/run.py` with workload m2 at both revisions and the new M3 workload only where supported. Record five interleaved trunk/head trial pairs per collector on the same host. Validate observed work and keep raw manifests. #72 ran 30 trials, five for each workload, revision, and collector, interleaved on one host. The report checks the observed work, and its manifest and summary are committed. The raw trial directories stayed on the run host, as [the campaign notes](latency-m3/notes.md) say.
- [x] Baseline. Record trunk metrics first with SHA, toolchain, host, workload, collector, and raw samples. Record unsupported trunk features explicitly. Trunk `3c30373` ran the first trial. The report records the compiler, host, workload, and collector, and it records that trunk does not run m3.
- [x] Rule. Every measured trial must meet pure p99 below 2 ms, manage/render p99 below 10 ms, and maximum observed stop-the-world pause below 10 ms. Keep M3 command targets from earlier packages. Report no ratio between M2 and M3 workloads. Record any host/toolchain change instead of comparing it to the old retained reports. All 30 trials met every budget, and the worst stop-the-world pause was 6.3 ms. Every m3 endpoint with a target stayed below its 50 ms p99 target. The report gives no ratio between m2 and m3 and draws no comparison with the M2 report.

**Review gate.** The operator reviews before merge.

- [x] Post the lane screenshots and a 30 to 60 second video of M3-07 behavior in chat. Save them under `artifacts/m3/M3-07/review/`. Wait for the operator's review. This is an operator gate. A 45-second video, a contact sheet, and the lane PNGs are on the maintainer's host under `artifacts/m3/M3-07/review/root-b52cc23/`. The operator reviewed them on 2026-11-10.

**Merge.**

- [x] Record a clean independent verdict at the exact head, complete review-bot triage, and confirm current dependencies. Stop at merge-ready for the operator. The root verdicts on #68 and #72 are posted on those PRs. #68 and #72 merged on 2026-11-09. #75 merged into #72's stale branch, and #76 re-landed its content on main the same day.

## Close the program

- [x] Link unit, live, and performance evidence for every package to the final head SHA. Leave incomplete boxes unchecked. The [coverage ledger](#record-package-coverage) links each package's evidence to its own merge or PR head. Lanes 9 and 10 ran on the delivery built from `27ca255`, the merge of #77.
- [x] Resolve every M3-D entry below and link the decision and regression evidence. Do not mark missing client urgency as implemented. Lane 9 gave M3-D03 its physical acceptance on 2026-11-10.
- [x] Pass the complete pinned release command and exact manifest, including every existing M0, M1, and M2 scenario. CI's pinned real-river job passes the full 67-scenario manifest at `5aaf32f`, a head of #72.
- [x] Confirm the public daily-use configuration builds outside the repository and works on the recorded physical devices. This is an operator gate, which lane 9 and the runbook in #68's description cover. The maintainer built it from `~/.config/seki` with the installed delivery and ran it on the recorded laptop.
- [x] Obtain the operator's daily-work acceptance record. Update the milestone table to Complete only after that record and all gates exist. This is an operator gate. The record below exists, and the spec and README say Complete.
- [x] The operator reviews the M3-07 media under `artifacts/m3/M3-07/review/root-b52cc23/`. Reviewed on 2026-11-10.
- [x] The operator merges #68, #72, and #75, in that order. #68 and #72 merged on 2026-11-09. #75 merged into #72's stale branch, and #76 re-landed it on main the same day.
- [x] The operator runs lanes 9 and 10 and fills in the [operator acceptance record](#operator-acceptance-record).
- [x] Report final PR links, measured results, coverage gaps, and the operator's remaining actions. This change's description reports them.

## Operator acceptance record

The operator ran lanes 9 and 10 with the runbook in [#68's description](https://github.com/chiro/seki/pull/68) on 2026-11-09 and 2026-11-10, and passed both. The captures and `sekictl snapshot` readbacks are on the maintainer's host under `artifacts/m3/M3-07/`, outside the repository.

### Lane 9. Physical keyboard and touchpad

| Field | Result |
|---|---|
| Date | 2026-11-09, with the suspend and resume check on 2026-11-10 |
| river version (`river -version`) | 0.5.0 +xwayland |
| seki commit | `b10e734`; the suspend and resume recheck ran `27ca255`, the merge of #77 |
| GHC version (`ghc --version`) | 9.10.3, from the `config-delivery` closure |
| Keyboard model | Built-in keyboard of a Lenovo ThinkPad E14 Gen 6 (21M3CTO1WW), reported as "AT Translated Set 2 keyboard" |
| Touchpad model (`libinput list-devices`) | ELAN06C7:00 04F3:3193 Touchpad, from seki's device list, since the `libinput` tool is not installed |
| Keyboard `keymap` and `repeat_info` results in `daily-hardware.json` | `keymap` sent and `repeat_info` sent at 25 Hz after 660 ms, before and after Mod-q and after resume. The readbacks are `lane9-snapshot-*.json` |
| Touchpad `tap`, `click_method`, and `natural_scroll` results in `daily-hardware.json` | `tap` applied, `click_method` applied as `click_finger`, and `natural_scroll` applied, before and after Mod-q and after resume |
| Caps Lock acts as Control | Yes |
| Repeat delay and rate | As intended |
| Tap, two-finger click, and two-finger natural scrolling | All work |
| Settings after Mod-q, suspend and resume, and a keyboard replug | Mod-q reapplied every setting in a new session of the same process, with one fcitx5 and one Waybar. After a lid suspend and resume, the settings stayed and the device generations did not change. After the swaylock unlock, no window got keys until focus moved; #77 fixes that. The operator confirmed the fix once #78's cause, a `cabal.project` still naming the first delivery's archive, was corrected. Replug does not apply, because the laptop has only the built-in keyboard |
| Capture | `daily-hardware.png` |
| Blockers | None. #77 and #78 fixed the two gaps the lane found |
| Lane 9 result | Pass |

### Lane 10. Daily-work session

| Field | Result |
|---|---|
| Date | 2026-11-09 to 2026-11-10 |
| river version (`river -version`) | 0.5.0 +xwayland |
| seki commit | `b10e734`, then `27ca255` from 2026-11-10 |
| GHC version (`ghc --version`) | 9.10.3, from the `config-delivery` closure |
| Device models | Lenovo ThinkPad E14 Gen 6 (21M3CTO1WW) with its built-in keyboard, the ELAN06C7:00 04F3:3193 touchpad, and a 1920x1200 panel |
| Tasks exercised | Terminal and editor work in alacritty, the Mod-grave scratchpad, and Emacs; a web browser and desktop applications; Japanese input through fcitx5 and Mozc; workspace moves, the Mod-p launcher, Mod-q restarts, swaylock, and Waybar; logging in from LightDM's river session |
| Blockers | None. The session found five gaps, and each is closed: alacritty's font was too small (the daily configuration sets 13.33), no launcher and missing xmonad keys (the daily configuration adds them), lost keyboard focus after an unlock (#77), and a rebuilt delivery that never reached the configuration (#78) |
| Capture | `daily-session.png` |
| Full-migration verdict (full migration from xmonad, or the gap that blocks it) | Full migration from xmonad |

## Appendix A. Prototype evidence

The initial D01 and D04 probes ran locally on 2026-10-31 at baseline
`f9b742380333884d8d3191f6d4601f110e911ec5`, in a local probe branch.
No production files changed. The probe report, with its scripts, logs, source hashes, and
reproduction commands, stayed on the maintainer's host and is not in the repository.
The design-gate table below records the evidence that each package produced in its own PR.

D01 remains blocked. River v0.5.0's `Server.zig` discards native activation for managed
windows, and its Xwayland adapter forwards no incoming urgency signal. The WM protocol
increased from version 4 to 6 without adding urgency. No supported client signal source
was demonstrated. All three input XMLs differ between river v0.4.5 and v0.5.0, so M3-D03
must decide versions and event batching before generation.

D04 has partial local evidence. External public-library builds from an sdist and warm
offline repeats passed with GHC 9.10.3. Atomic profile-symlink handover reached a changed
configuration under the same PID on isolated river 0.5.0. The existing `m2-restart`
scenario separately passed pixel and focus checks for a same-image restart.
These two results do not prove pixels and focus across changed images.

The internal check probe rejects duplicate tags without connecting or running hooks,
but the packaged public API has no exact-candidate check mode yet. That mode, cold
provisioning, changed-image pixel/focus checks, and the pinned Nix release environment
remain unproved. Nix is absent on this host. The probe recommends a threaded generated
skeleton without default multicore RTS flags and the existing stable profile symlink.
M3-01 must still work safely under the current non-threaded executable.

Keep future throwaway probes separate from production code and record each branch, SHA,
command, observation, and artifact path. Do not turn an unrun check into a passed box.

### Resolve the design gates

| ID | Status | Required evidence and decision | Package |
|---|---|---|---|
| M3-D01 | Technically unsupported; automatic client urgency explicitly deferred from M3 by operator 2026-11-03 | Source refresh checked v0.5.0 and upstream `65a90843` with no supported native/Xwayland attention delivery. No M3-05 implementation or manual substitute. M3-07 may proceed under revised scope; physical/daily-work gates are not waived. | Future urgency work |
| M3-D02 | Verified for M3-01 | Direct worker `manage_dirty` wakes idle river under the non-threaded RTS. The queue retains one dispatcher and policy owner, with capacity 256 and batch size 32. The initial round-trip probe used coarse polling and established feasibility, not precise wire latency. M3-01 selected a 50 ms enqueue-to-observed-policy p99 ceiling before production measurement. Independent checks at `5dc5da1` cover waiter cancellation, owned-wake failure, saturation with focus/resize, shutdown, and stale sessions. The root verdict stayed on the maintainer's host and is not in the repository. | M3-01 |
| M3-D03 | Verified; lane 9 passed the physical acceptance on 2026-11-10 | At `44c24e9`, pinned river v0.5.0 input XMLs, source-level v1 compatibility, capped v1/v2/v3 discovery, facet/generation retirement and real XKB/generic controls pass independent checks. Custom curves with 64 points pass native transient configuration checks. Actual river 0.4.5 execution and physical libinput application are not claimed. | M3-03 |
| M3-D04 | Verified for the declared source-archive plus pinned Nix dependency path | Exact public candidate --check, fresh Cabal cache/store, warm repeat, installed archive/compiler/default/product delivery, threaded skeleton and stable profile handover proved. Changed-image state, pixels, input, PID, and argv0, and recovery from a failed exec and from serialization, were independently verified. First closure acquisition requires Nix network/trust; no secure-index repair or cold-Nix claim. | M3-06 |
| M3-D05 | Decided in M3-04 | Exercise scratchpad launch-pending, hide/show, duplicate matches, workspace moves, restart, and changed definitions. Confirm storage and manageHook precedence with executable proof before freezing the API. Decision: a definition's first matching window becomes its owner and bypasses the user `manageHook`, and hidden pads stay outside workspace navigation. [Scratchpads](scratchpads.md) documents the shipped storage, precedence, and restore rules. The `m3-scratchpad-*` release scenarios exercise pending launches, hide and show, duplicates, workspace moves, restart, and changed definitions. | M3-04 |
| M3-D06 | Decided in M3-02 | Exercise IPC command acceptance versus application and displayed state, slow readers, reconnect, and title-only updates. Confirm revision and error semantics from observed traces. Decision: a receipt means the reducer applied the command and carries that revision, not a displayed frame, and a subscriber may skip intermediate revisions. [IPC](ipc.md) documents the shipped receipt, revision, and error contract. The `m3-ipc-*` release scenarios exercise receipts, slow readers, reconnect, and title-only updates. | M3-02 |
| M3-D07 | Decided by operator 2026-11-01 | Retain one active seki IPC owner per runtime directory for M3. A second seki sharing `$XDG_RUNTIME_DIR/seki/ipc.sock` exits at IPC startup; `sekictl` targets the existing endpoint. Concurrent-session discovery is outside M3. Existing isolated test sessions use private runtime directories. | M3-02 |

The proposed IPC limits, rule precedence, scratchpad behavior, and response targets in the
packages are defaults for review. They are not previously accepted specification decisions.
Do not silently reduce M3-D01 to script-generated urgency to make the milestone pass.

## Appendix B. Alternatives rejected

The plan puts external ingress before IPC and builds because both require progress while
Wayland is idle. Starting either at the socket or compiler layer leaves that shared requirement unresolved.

A custom bar is outside M3. The spec already chooses a Waybar custom module over the JSON Lines socket.
Submaps, binding repeat, focus-follows-mouse, prompts, and general contrib parity remain M4 onwards.

Reusing the M2 restore handshake avoids introducing another handover mechanism.
Restart already records argv[0] without following symlinks, so a symlink repointed at a new build reaches it (`Restart.hs`).
Add a separate restart-target selector only if M3-D04 shows that the launcher and the user binary must stay distinct.

Packages run serially in one linear stack. M3-03, M3-04, and M3-06 all edit `src-public/Seki.hs`,
`src-core/Seki/Internal/Config.hs`, `Policy.hs`, and `seki.cabal`, so parallel owners would wait on the same files.

Window identity uses river identifiers for restoration. PID, app ID, and title matching are not substitutes for persistent identity.
Scratchpad queries select newly managed windows only. Restored ownership uses identifiers.

A passing headless input test does not establish physical touchpad behavior.
Synthetic urgency events prove reducer policy only. They do not establish a client signal source.

## Appendix C. Risks

| Risk | Owner and check |
|---|---|
| No supported client urgency signal | The operator deferred M3-05 from M3 on 2026-11-03, and M3-D01 stays unsupported. The full-migration claim does not include automatic client urgency. |
| Idle work never wakes, or crosses phase boundaries | M3-01 proves progress on an idle compositor and races around sequence completion and shutdown. |
| Slow subscribers or command bursts starve the WM | M3-02 isolates output and bounds work. Keep timed input and M2 latency evidence under load. |
| Stale socket cleanup breaks a live instance | M3-02 checks ownership and liveness. Test two simultaneous startups and restore handover. |
| Virtual devices lack libinput capabilities | M3-03 separates protocol fixtures, real virtual devices, and physical acceptance. Do not skip hardware boxes. |
| Installed package cannot compile user configuration | M3-06 proves an external-directory build and documents dependency acquisition before promising first-run behavior. |
| Restored scratchpads become inaccessible | M3-04 tests removed definitions, missing windows, hidden storage, and restore schema compatibility. |
| Build or log failures stop a working configuration | M3-06 keeps compilation outside the WM loop and never stops before successful validation. |
| Fixture captures do not cover hardware settings or CLI errors | Each lane saves a compositor PNG for context and a transcript/readback for the actual predicate. Screenshots alone are insufficient. |
| No project driver skill or scheduler | Use the exact CLI boot recipe. Add a project driver if repeated execution needs one. Report absent scheduling support rather than simulate a background audit. |
| Toolchain differs from retained latency reports | M3-07 records current trunk and head on the same toolchain. Historical reports are references, not current baselines. |
| Seven-package integration grows shared files | Each package builds on the previous package's head in the linear stack. Re-run affected integration scenarios at each head. |
| A worker blocks the non-threaded RTS | M3-01 and M3-06 keep workers free of blocking foreign calls. M3-D04 decides whether the user binary's RTS changes. |

## Appendix D. Links and reading list

Start with [spec sections 3.9, 5.2, 5.3, and 6.2](spec.md),
[the M2 closeout](m2-plan.md), [the real-river harness](integration-testing.md),
[wire-protocol conventions](wire-protocol.md), [reconciliation](reconciliation.md),
and [latency methodology](latency.md).

Read runtime ownership in [Runtime.hs](../src/Seki/Run/Runtime.hs),
[the nonblocking transport](../src/Seki/Wire/Transport.hs),
[policy drain and reconciliation](../src/Seki/Run/Policy.hs),
[configuration parsing](../src/Seki/Run/Config.hs), and
[restart execution](../src/Seki/Run/Restart.hs).
Read [core actions](../src-core/Seki/Internal/Core/Action.hs),
[core state](../src-core/Seki/Internal/Core/State.hs),
[restore state](../src-core/Seki/Internal/Core/Restore.hs), and
[public configuration](../src-core/Seki/Internal/Config.hs) before proposing new public signatures.

Use `skills/how/SKILL.md` for each affected subsystem and `skills/architect/SKILL.md`
before implementation changes cross public or runtime boundaries.
Use `skills/interrogate/SKILL.md` for contested phase, identity, or lifecycle designs.
Use `skills/benchmark-checklist/SKILL.md` before reporting new timings.
Use `skills/show-me-your-work/SKILL.md` for execution decisions and link each local trail from its package evidence.

The source-only planning surveys found no IPC server, external ingress, scratchpad state,
input adapters, or build manager at the recorded baseline. Only three required river
protocols are currently bound. Input XML acquisition, regeneration, and freshness checks
are implementation work, not evidence of support already present.

### Record package coverage

| Package | Implementation | Unit | Live | Perf | Review and acceptance |
|---|---|---|---|---|---|
| M3-01 | PR #41 merged as `ce80025` | 744 unit tests and public suite pass | Ten lanes independently verified; pinned CI manifest passes | Existing budgets pass; reviewed tree unchanged by merge | Two independent audits pass; operator merged 2026-10-31 |
| M3-02 | PR #43 merged as `dbab756`; tree matches reviewed `9e60f13` | 766 unit and public tests pass; 772 with independent extra probes | 34/34 native scenarios, phase and package checks, and exact-head pinned CI pass | Exact-head native IPC perf gates pass; publication retained deliberately | All 10 operator items dispositioned; fresh independent PASS; preserve branch until child retarget |
| M3-03 | Consolidated PR #44 merged to main as `6d349f5`; tree exactly matches reviewed `031bb19` | 805 tests and public suite pass independently | Root 45/45 and independent 16 native scenarios, phase and package checks, and `deb96be` exact-head CI pass; hardware unverified | Exact-head native IPC and input finite performance gates pass; historical campaign retains original heads | Parent fixes independently preserved; operator media approval retained; no physical acceptance claimed |
| M3-04 | PR #45 included in PR #44, now main `6d349f5` | Root and independent 840 tests and public, package, phase, and format checks pass | Full 59-scenario exact manifest passes; C1/M1 independent native controls and keyboard and geometry receipts pass | Historical finite campaign applies because the timed paths are unchanged; full 59-scenario stock gate passes; no fresh campaign claimed | Pinned `95cddf9` passes all 4 checks; fresh media approved by operator; both review fixes consumed; merge remains operator-owned |
| M3-05 | Deferred from M3 by explicit operator scope decision | Not implemented | No client urgency proof claimed | Not measured | Deferral covers automatic native/Xwayland client urgency only; no manual substitute or broader waiver |
| M3-06 | PR #46 included in PR #44, now main `6d349f5`; approved tree `031bb19` preserved | 856 tests, public, package, and phase checks, and eight external candidate checks pass locally | Full 60-scenario native run and ten named functional lanes pass; changed definitions, a real changed image, and recovery proved | 30 finite trials retained; one baseline nonmoving trial at 11.2 ms fails the strict < 10 ms budget; the other 29 pass, including all 20 head trials; the original campaign failed; PR #46-only baseline exception approved, head budgets unchanged | `c946f43` bootstrap receipts valid; `c1f9578` false-green proof rejected; `53120f5` all 8 checks and four complete proof artifacts verified; `971c2bb` installed receipts valid; `cfbe7d4` all 8 CI checks and all 4 artifact proofs verified; ten lanes and media validated; baseline performance BLOCKED; `031bb19` CI and all 4 artifacts verified; second audit and README recheck PASS; operator approved the PR #46-only baseline exception and media; merge operator-owned |
| M3-07 | #68 adds the daily-use configuration and lanes, #72 the m3 workload and campaign, and #75 this plan and the documentation; #70 merged as `3c30373` and ships `sekictl` in `config-delivery` | CI's build, test, phase-type, and package checks pass in the pinned shell at `b72b2b1` and `5aaf32f`; `fourmolu` and `test_report.py` pass outside the pinned shell | Lanes 1 to 8 pass at `b52cc23`, and lane 1 also at trunk `3c30373`; the full manifest passes in CI at `b72b2b1` and `5aaf32f`; the operator passed lanes 9 and 10 on 2026-11-10, after #77 and #78 | 30 of 30 strict trials at trunk `3c30373` and head `82a31cc` meet every budget, with a worst pause of 6.3 ms; m3 endpoints stay below their 50 ms targets; the latency fixture cannot time input results or the recompile handover | #68 had three audit rounds with nothing blocking, and its root verdict is merge-ready at `b72b2b1`; #72's round-2 audits found nothing blocking, and their fixes end at `5aaf32f`; #71, #73, and #74 track follow-ups; media review, merges, and the acceptance record belong to the operator |
