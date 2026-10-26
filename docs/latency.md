# Latency and collector measurements

The opt-in `seki-latency` diagnostic and the `tools/latency/run.py` runner
measure manage/render sequence latency and GC behavior in repeated trials and
generate a report. They do not change production configuration or the
executable's RTS defaults. Two reviewed reports keep compact evidence of five
alternating trials per collector: the M1 [reference report](latency-baseline/report.md)
of 2026-10-18 and the M2 [workload report](latency-m2/report.md) of
2026-10-25. Ordinary unit CI does not enforce timing budgets.

## Run a comparison

Use the pinned `ci` environment for river, wtype, wlr-randr, and GHC 9.14.1.
Python 3 is an additional prerequisite; `flake.nix` does not declare it in that
shell. Make `python3` available when running these commands:

```sh
nix develop --no-write-lock-file .#ci --command python3 tools/latency/test_report.py
nix develop --no-write-lock-file .#ci --command python3 tools/latency/run.py artifacts/latency/m2 --workload m2 --compiler ghc
nix develop --no-write-lock-file .#ci --command python3 tools/latency/run.py artifacts/latency/m2 --report-only
```

`--workload` is required to collect trials. `m2` is the M2 gate. `m1` is the
M1 workload, still runnable on current code for comparison. Report-only mode
reads the workload from `manifest.json`; a schema 1 manifest, written before
workloads existed, reads as `m1`. It needs the full trial directories, so it
cannot regenerate a retained compact report.

Both retained reports used local GHC 9.6.7 and Cabal 3.10. The M2 report ran
`WLR_RENDERER=pixman python3 tools/latency/run.py artifacts/latency/m2-2026-10-25 --workload m2 --compiler ghc-9.6.7`
with system-installed runtime dependencies. This is a local command, not the
pinned release toolchain.
For collection and profile runs, pass `--compiler` explicitly; the runner
defaults to `ghc-9.14.1`. Report-only mode does not use a compiler.

The runner builds once with `--enable-optimization=2` into
`dist-newstyle/latency`. It alternates normal/nonmoving trials five times using
the identical binary and `+RTS -N1 -T -s`; only the nonmoving side adds
`--nonmoving-gc`. Unlike the production executable, the diagnostic is
threaded and accepts RTS options. GHC 9.14 needs no `-eventlog` link option for
eventlog output. Do not compile concurrently with trials.

For a short feasibility check, use a new directory and
`--trials 1 --live-steps 3 --pure-steps 512` with `--workload m1`, or
`--live-steps 120` with `--workload m2`, since an M2 trial is valid only when it
exercised every M2 feature. A supplied `--binary PATH` skips
building, records its hash and embedded compiler family, and marks its
compiler patch version and dependency optimization flags as unverified. A
short check is exploratory, not the five-trial report gate. Existing artifact directories are never replaced.

The runner starts only private headless river instances with the pixman renderer.
It owns the separate WM, native client/driver, keyboard keeper, and river process
groups. Their PID/start-time ownership records support cleanup after a driver
failure. It never attaches to an existing compositor session.

## Workloads and endpoints

Both live workloads run on one 800 by 600 output under Tall with the default
`Crop` policy, seed 1909, 24 warmup actions, and 1023 measured actions. The
pure model in `tools/latency/Seki/Latency/Workload.hs` names the windows, the
seeded plan, what the probe observes after each action, and the expected
picture; `test/Seki/Run/LatencyWorkloadTest.hs` checks it without river.

- **m1** opens eight solid-color tiles. Each group selects a seeded focus
  direction, views the empty workspace, then restores the populated workspace.
- **m2** opens six tiles and then two parent-hinted dialogs of the first tile,
  which the WM's `isDialog --> doFloat` hook floats at their fixed 300 by 200
  and 200 by 300 sizes, overlapping each other. Each group changes focus, which
  raises a focused dialog and exits fullscreen; sends Shrink or Expand, so every
  tile gets a new size and a new content clip box; may request client
  fullscreen on a focused tile; views the empty workspace and back, which hides
  and restores a fullscreen window; and may request the fullscreen exit. Over
  the measured actions it makes 225 resizes, 90 fullscreen entries, 58 exits by
  focus, 32 exits by request, 90 hidden-fullscreen views, and 24 dialog raises.

Every action waits until the probe observes the expected keyboard owner and the
size every window last committed. The plan's tests prove that each action
changes that observation, so no barrier passes without waiting. The barrier is
the client's commit, not a fresh frame. Pictures are polled and checked against
every pixel before the measured interval, after it, and after the restoring
actions: content shows its window's exact color, and borders and background
show no probe color. Final keyboard delivery must reach only the focused
client. No capture runs inside the interval, so captures add no render
sequences to the quantiles. An explicit WM checkpoint waits for the
completed-render idle phase after the last action, before the driver closes
the measurement interval. Checkpoint signal/file overhead lies inside this
broad driver interval, outside individual sequence timers.

The fixture acknowledges every configure and commits it. Unchanged dimensions
reuse current surface content. Genuine resizes allocate a new buffer, and a
buffer river releases is freed, so the fixture's limit of 128 counts live
buffers. The workloads do not measure more than eight native windows, pointer
operations, or a restart.

The runtime optionally keeps at most 100,000 strict sequence samples. Disabled
instrumentation reads no clocks and stores no samples. A sample starts at
`runSequence` entry before sequence-state validation and ends immediately after
a successful finish-request submission returns. It excludes the later state
write, `manage_dirty`, recording insertion, compositor acknowledgment and visible
presentation. Returned errors and handler exceptions count as failures; exception
propagation is preserved. Capacity overflow is visible and invalidates a trial.

Driver and WM timestamps use Linux's shared monotonic clock. Raw WM samples
include startup, warmup, measured actions and cleanup. Quantiles include only
samples wholly inside the driver interval. The validator requires both manage
and render counts to cover every acknowledged action, checks sample ordering,
and requires the entire interval to lie inside the WM sample history.

Pure mode uses 32 windows, 20,000 events, and 128 warmup events with the same seed.
It exercises focus, swaps, workspace changes, shifts, Tall sizing/master counts,
Tall/Mirror/Full selection, floating bookkeeping and intents. The m2 workload
starts with the two newest windows as client-sized dialog floats of window 1,
planned with those parent links, and adds a fullscreen toggle on the focus, a
client fullscreen request, a click on a dialog, and floating window 3. Concrete random
operation codes and initial state are forced before measurement. Every timed
iteration runs production `drainCore` and `planScene`, then a strict structural
checksum fold over all state, layout, tag, stack, floating, fullscreen,
pointer-op, intent and scene fields, including each tile's size policy. The next iteration therefore receives an already forced state. Fold
overhead is included. Membership, tag, focus, stacking and geometry invariants run after
each timer; a separate replay must produce the same retained checksum.

## Artifacts and interpretation

Each trial retains `wm.json`, `wm.rts`, `driver.json`, `pure.json`, `pure.rts`,
process identities, two WM RTS checkpoints, and process logs. `manifest.json`
records machine/CPU, binary hash, compiler, river version, actual build command,
RTS flags, seed, workload, trial order and host load. `summary.json` contains
per-trial quantiles, counts, collector statistics, exercise counts and budget
outcomes; `report.md` gives medians and ranges across trials, a generated
budgets section that names each miss with its value, manage and render
sequences per measured action, and the m2 exercise counts. Keep the compact summary
with the reviewed report. Bulk raw artifacts may remain under ignored
`artifacts/latency/`.

WM allocation and maximum residency come from that WM process's lifetime RTS
summary, including startup/shutdown. Driver/native-client and river allocations
are separate. The `-s` generation `colls` maxima and nonmoving `syncs` maxima are
stop-the-world observations with 0.1 ms printed resolution. Nonmoving concurrent
cycle duration is not a pause. No forced final GC is used. Boundary signal
checkpoints provide `GHC.Stats` counter deltas around the measured workload;
these counters update at collection boundaries and are not exact allocation
per operation. A trial without a major collection during this interval gives
limited collector evidence. Lifetime maxima cannot be subtracted to produce
interval maxima.

Budgets are pure p99 below 2 ms, complete-sequence p99 below 10 ms and maximum
observed stop-the-world GC pause below 10 ms. They do not establish a river
response-time guarantee or a compositor timeout margin. The report keeps
per-trial distributions rather than pooling away noisy trials. It does not
select a collector winner without repeatable evidence and an identified limiter.

## Separate profile run

```sh
nix develop --no-write-lock-file .#ci --command python3 tools/latency/run.py artifacts/latency/profile --workload m2 --compiler ghc --profile
```

Profile mode runs one normal-collector live workload with eventlog output and
periodic `/proc/PID/stat` CPU observations for WM, river, driver and keyboard
keeper. `wm.eventlog`, `cpu.json`, wall timestamps, and RTS data can distinguish
CPU consumption, collection, and waiting/scheduling classes. The profile is
excluded from the comparison. Source-level hot spots require additional
profiling evidence; process CPU totals alone do not identify an algorithm.
No eventlog parser is required for ordinary trial collection or reporting.
