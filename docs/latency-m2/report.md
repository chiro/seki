# Latency and collector evidence

Collector selection is inconclusive. Production defaults remain unchanged.

The diagnostic uses the same optimized threaded build at `-N1`; only `--nonmoving-gc` differs. WM sequence latency is request submission time, not compositor acknowledgment or display latency.

Trials per collector: 5. Fewer than five is exploratory evidence only.
Machine: Linux-7.2.9-arch1-1-x86_64-with-glibc2.44. Compiler: 9.6.7. Binary SHA-256: `4b4d08e65ff5add2b2001087586562fcc5b7e4a4fefef95f0ba3cdc47a681be7`.
Seed 1909; workload m2: six native tiled windows and two parent-hinted dialog floats under Tall with Crop; focus, view, Shrink/Expand, and client fullscreen actions; 1023 acknowledged actions after 24 warmup actions. Pure mode uses 32 windows, 20000 events, and 128 warmup events.

| Collector | Metric | Trials | Median | Min | Max |
|---|---|---:|---:|---:|---:|
| normal | pure p50_ms | 5 | 0.0168 | 0.0163 | 0.0172 |
| normal | pure p95_ms | 5 | 0.0457 | 0.0444 | 0.0464 |
| normal | pure p99_ms | 5 | 0.0554 | 0.0548 | 0.0684 |
| normal | pure max_ms | 5 | 1.5481 | 1.5076 | 1.7360 |
| normal | manage p50_ms | 5 | 0.0577 | 0.0558 | 0.0601 |
| normal | manage p95_ms | 5 | 0.1349 | 0.1258 | 0.1384 |
| normal | manage p99_ms | 5 | 0.1713 | 0.1622 | 0.1773 |
| normal | manage max_ms | 5 | 0.3086 | 0.2910 | 1.1356 |
| normal | render p50_ms | 5 | 0.0429 | 0.0415 | 0.0449 |
| normal | render p95_ms | 5 | 0.1842 | 0.1783 | 0.1881 |
| normal | render p99_ms | 5 | 0.2379 | 0.2271 | 0.2539 |
| normal | render max_ms | 5 | 0.3931 | 0.3576 | 0.9262 |
| normal | WM allocated_bytes | 5 | 1372285288.0000 | 1365366896.0000 | 1374529216.0000 |
| normal | WM max_residency_bytes | 5 | 4067312.0000 | 2118928.0000 | 4126096.0000 |
| normal | WM max_stw_pause_ms | 5 | 2.2000 | 1.7000 | 2.4000 |
| normal | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.0000 |
| nonmoving | pure p50_ms | 5 | 0.0168 | 0.0161 | 0.0168 |
| nonmoving | pure p95_ms | 5 | 0.0457 | 0.0444 | 0.0463 |
| nonmoving | pure p99_ms | 5 | 0.0553 | 0.0544 | 0.0562 |
| nonmoving | pure max_ms | 5 | 0.1476 | 0.1204 | 0.2708 |
| nonmoving | manage p50_ms | 5 | 0.0586 | 0.0567 | 0.0587 |
| nonmoving | manage p95_ms | 5 | 0.1337 | 0.1295 | 0.1358 |
| nonmoving | manage p99_ms | 5 | 0.1683 | 0.1597 | 0.1777 |
| nonmoving | manage max_ms | 5 | 0.3716 | 0.3182 | 0.4379 |
| nonmoving | render p50_ms | 5 | 0.0433 | 0.0418 | 0.0439 |
| nonmoving | render p95_ms | 5 | 0.1812 | 0.1793 | 0.1841 |
| nonmoving | render p99_ms | 5 | 0.2276 | 0.2261 | 0.2449 |
| nonmoving | render max_ms | 5 | 0.4230 | 0.3716 | 0.4346 |
| nonmoving | WM allocated_bytes | 5 | 1372005016.0000 | 1366298656.0000 | 1373198488.0000 |
| nonmoving | WM max_residency_bytes | 5 | 4828696.0000 | 4770560.0000 | 4930016.0000 |
| nonmoving | WM max_stw_pause_ms | 5 | 0.7000 | 0.7000 | 0.8000 |
| nonmoving | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.1000 |

## Budgets

Budgets are pure p99 < 2 ms, manage and render sequence p99 < 10 ms, and maximum observed WM stop-the-world pause < 10 ms.

All 10 trials met every budget.

## Work and validity

Sequences per action count the manage and render sequences inside the interval for each measured action. A rise against the same workload points at repeated proposals.

| Trial | Manage | Render | Manage per action | Render per action | Pure | Major GC in workload | Load before / after |
|---|---:|---:|---:|---:|---:|---|---|
| 00-normal | 2924 | 2924 | 2.86 | 2.86 | 20000 | True | [1.68798828125, 1.322265625, 0.958984375] / [1.35302734375, 1.28466796875, 0.96142578125] |
| 01-nonmoving | 2923 | 2923 | 2.86 | 2.86 | 20000 | True | [1.35302734375, 1.28466796875, 0.96142578125] / [1.2080078125, 1.25634765625, 0.9638671875] |
| 02-normal | 2910 | 2910 | 2.84 | 2.84 | 20000 | True | [1.2080078125, 1.25634765625, 0.9638671875] / [0.9453125, 1.18017578125, 0.94921875] |
| 03-nonmoving | 2923 | 2923 | 2.86 | 2.86 | 20000 | True | [0.9453125, 1.18017578125, 0.94921875] / [0.72509765625, 1.09619140625, 0.92919921875] |
| 04-normal | 2922 | 2922 | 2.86 | 2.86 | 20000 | True | [0.72509765625, 1.09619140625, 0.92919921875] / [0.8193359375, 1.0703125, 0.92578125] |
| 05-nonmoving | 2915 | 2915 | 2.85 | 2.85 | 20000 | True | [0.8193359375, 1.0703125, 0.92578125] / [0.8974609375, 1.0625, 0.92822265625] |
| 06-normal | 2928 | 2928 | 2.86 | 2.86 | 20000 | True | [0.8974609375, 1.0625, 0.92822265625] / [1.11181640625, 1.10107421875, 0.947265625] |
| 07-nonmoving | 2919 | 2919 | 2.85 | 2.85 | 20000 | True | [1.11181640625, 1.10107421875, 0.947265625] / [0.92919921875, 1.05810546875, 0.9375] |
| 08-normal | 2904 | 2904 | 2.84 | 2.84 | 20000 | True | [0.92919921875, 1.05810546875, 0.9375] / [1.06494140625, 1.08251953125, 0.951171875] |
| 09-nonmoving | 2905 | 2905 | 2.84 | 2.84 | 20000 | True | [1.06494140625, 1.08251953125, 0.951171875] / [0.91943359375, 1.04150390625, 0.9423828125] |

## Exercised M2 features

Counts come from the model's transitions over the measured actions. Every count except float raises changed what the probe observed, so a barrier confirmed it.

| Trial | clip_resizes | fullscreen_enters | fullscreen_exits_by_focus | fullscreen_exits_by_request | hidden_fullscreen_views | float_raises |
|---|---:|---:|---:|---:|---:|---:|
| 00-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 01-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 02-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 03-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 04-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 05-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 06-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 07-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 08-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 09-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |

## Interpretation

Per-trial results and budget outcomes are retained in summary.json. Budgets are engineering budgets, not river guarantees, and no ordinary unit test enforces wall-clock budgets.

RTS allocation and residency describe the WM process lifetime, including startup and shutdown. The `-s` per-generation collection maxima have 0.1 ms printed resolution. Nonmoving synchronization maxima are reported separately and included in the observed stop-the-world maximum; concurrent cycle duration is excluded. There is no forced final GC. The earlier RTSStats snapshot reports whether major GC occurred before shutdown. A trial without major GC gives limited collector evidence.

The pure interval includes concrete structural checksum folding over every state, layout, tag, stack, floating rectangle, fullscreen window, pointer op, intent, and desired-scene field. Inputs are pre-generated and previous state fully evaluated. Independent invariants and deterministic replay occur outside the interval. Replay, serialization, and validation still contribute to pure-process lifetime RTS totals.

Each native action waits until the probe observes the expected keyboard owner and the size every window last committed. Pictures are polled and checked pixel by pixel only outside the measured interval: before it, after it, and after the restoring actions. Final input delivery checks the target window. An idle-phase WM acknowledgment after the last action ensures the last completed render precedes interval end. Signal/checkpoint overhead lies in the broad driver interval, outside sequence timers. Startup, warmup, cleanup, and samples crossing the interval boundary remain in raw wm.json but are excluded from quantiles. Driver/native-client and river costs are excluded from WM RTS statistics.

Scheduling, GC, and waiting remain possible limiters. Separate profile mode records a WM eventlog and Linux process CPU snapshots for WM, river, and driver; it is excluded from these trials. These observations do not identify source-level hotspots. No collector winner is inferred from noisy distributions or an unproven limiter.

The local compiler may differ from the repository's pinned GHC 9.6.6. Consult manifest.json for the exact compiler, build flags, platform, tools, trial order, and workload. Reproduction commands and limitations are in docs/latency.md.
