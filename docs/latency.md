# Latency and collector measurements

WP9 provides an opt-in diagnostic, a repeated-trial runner, and reproducible report
creation. Production configuration and executable RTS defaults remain unchanged.
The reviewed [reference report](latency-baseline/report.md) contains five
alternating trials per collector and the retained compact evidence.
Ordinary unit CI does not enforce timing budgets.

## Run a comparison

Use the pinned `ci` environment for river, wtype, wlr-randr, and GHC 9.6.6.
Python 3 is an additional prerequisite; `flake.nix` does not declare it in that
shell. Make `python3` available when running these commands:

```sh
nix develop --no-write-lock-file .#ci --command python3 tools/latency/test_report.py
nix develop --no-write-lock-file .#ci --command python3 tools/latency/run.py artifacts/latency/reference --compiler ghc
nix develop --no-write-lock-file .#ci --command python3 tools/latency/run.py artifacts/latency/reference --report-only
```

The 2026-10-18 reference report used local GHC 9.6.7 and Cabal 3.10. To
reproduce that compiler choice with system-installed runtime dependencies, use
`python3 tools/latency/run.py artifacts/latency/local-9.6.7 --compiler ghc-9.6.7`.
This is a historical baseline command, not the pinned release toolchain.
For collection and profile runs, pass `--compiler` explicitly; the runner
defaults to `ghc-9.6.7`. Report-only mode does not use a compiler.

The runner builds once with `--enable-optimization=2` into
`dist-newstyle/latency`. It alternates normal/nonmoving trials five times using
the identical binary and `+RTS -N1 -T -s`; only the nonmoving side adds
`--nonmoving-gc`. The diagnostic is threaded and supports RTS options, unlike
the production executable. GHC 9.6 already supports eventlogs without the
deprecated `-eventlog` link option. Do not compile concurrently with trials.

For a short feasibility check, use a new directory and
`--trials 1 --live-steps 3 --pure-steps 512`. A supplied `--binary PATH` skips
building, records its hash and embedded compiler family, and explicitly marks
its compiler patch version and dependency optimization flags as unverified. A short check is exploratory,
not the five-trial report gate. Existing artifact directories are never replaced.

The runner starts only private headless river instances with the pixman renderer.
It owns the separate WM, native client/driver, keyboard keeper, and river process
groups. Their PID/start-time ownership records support cleanup after a driver
failure. It never attaches to an existing compositor session.

## Workloads and endpoints

The live reference workload has eight solid-color native clients under Tall,
seed 1909, 24 warmup actions and 1023 measured actions. Each group selects a
seeded focus direction, views the empty workspace, then restores the populated
workspace. Every operation waits for the expected native keyboard-owner change.
Initial and restored captures check every client-content pixel against exact
Tall rectangles, including all eight windows and omitted background regions.
Final keyboard delivery must reach only the focused client. Captures verify pixels and can add render sequences; actual counts are retained.
An explicit WM checkpoint waits for the completed-render idle phase after the
last action and capture, before the driver closes the measurement interval.
Checkpoint signal/file overhead lies inside this broad driver interval, outside
individual sequence timers. Capture readiness alone is not a fresh-frame barrier.

The fixture acknowledges every configure and commits it. Unchanged dimensions
reuse current surface content. Genuine resizes allocate a new buffer. The
existing 128 lifetime-buffer limit remains in force; this workload does not
claim broad resize throughput or a 32-window native stress case.

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
Tall/Mirror/Full selection, floating bookkeeping and intents. Concrete random
operation codes and initial state are forced before measurement. Every timed
iteration runs production `drainCore` and `planScene`, then a strict structural
checksum fold over all state, layout, tag, stack, floating, intent and scene
fields. The next iteration therefore receives an already forced state. No
`show`, WHNF-only result or map-size proxy stands in for work. Fold overhead is
included. Membership, tag, focus, stacking and geometry invariants run after
each timer; a separate replay must produce the same retained checksum.

## Artifacts and interpretation

Each trial retains `wm.json`, `wm.rts`, `driver.json`, `pure.json`, `pure.rts`,
process identities, two WM RTS checkpoints, and process logs. `manifest.json`
records machine/CPU, binary hash, compiler, river version, actual build command,
RTS flags, seed, workload, trial order and host load. `summary.json` contains
per-trial quantiles, counts, collector statistics and budget outcomes;
`report.md` gives medians and ranges across trials. Keep the compact summary
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
nix develop --no-write-lock-file .#ci --command python3 tools/latency/run.py artifacts/latency/profile --compiler ghc --profile
```

Profile mode runs one normal-collector live workload with eventlog output and
periodic `/proc/PID/stat` CPU observations for WM, river, driver and keyboard
keeper. `wm.eventlog`, `cpu.json`, wall timestamps, and RTS data can distinguish
CPU consumption, collection, and waiting/scheduling classes. The profile is
excluded from the comparison. Source-level hot spots require additional
profiling evidence; process CPU totals alone do not identify an algorithm.
No eventlog parser is required for ordinary trial collection or reporting.
