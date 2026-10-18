# WP9 latency and collector evidence

Collector selection is inconclusive. Production defaults remain unchanged.

The diagnostic uses the same optimized threaded build at `-N1`; only `--nonmoving-gc` differs. WM sequence latency is request submission time, not compositor acknowledgment or display latency.

Five alternating trials per collector.
Machine: Linux-7.2.9-arch1-1-x86_64-with-glibc2.44. Compiler: 9.6.7. Binary SHA-256: `9cc63fa9f521e2b0e29f0c1c7a18783662e7d98dc4b15431717291a4051bcc5b`.
Seed 1909; eight native windows; 1023 acknowledged focus/view actions after 24 warmup actions. Pure mode uses 32 windows, 20000 events, and 128 warmup events.

| Collector | Metric | Trials | Median | Min | Max |
|---|---|---:|---:|---:|---:|
| normal | pure p50_ms | 5 | 0.0104 | 0.0099 | 0.0108 |
| normal | pure p95_ms | 5 | 0.0277 | 0.0258 | 0.0286 |
| normal | pure p99_ms | 5 | 0.0406 | 0.0358 | 0.0418 |
| normal | pure max_ms | 5 | 0.1524 | 0.0843 | 0.6334 |
| normal | manage p50_ms | 5 | 0.0467 | 0.0441 | 0.0481 |
| normal | manage p95_ms | 5 | 0.1004 | 0.0996 | 0.1053 |
| normal | manage p99_ms | 5 | 0.1300 | 0.1211 | 0.1384 |
| normal | manage max_ms | 5 | 0.4727 | 0.3104 | 0.5105 |
| normal | render p50_ms | 5 | 0.0383 | 0.0372 | 0.0397 |
| normal | render p95_ms | 5 | 0.1761 | 0.1749 | 0.1833 |
| normal | render p99_ms | 5 | 0.2394 | 0.2287 | 0.2681 |
| normal | render max_ms | 5 | 0.4645 | 0.3617 | 0.7813 |
| normal | WM allocated_bytes | 5 | 1358643184.0000 | 1355081584.0000 | 1360190552.0000 |
| normal | WM max_residency_bytes | 5 | 3059720.0000 | 3048056.0000 | 3066376.0000 |
| normal | WM max_stw_pause_ms | 5 | 2.1000 | 1.6000 | 3.5000 |
| normal | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.0000 |
| nonmoving | pure p50_ms | 5 | 0.0105 | 0.0100 | 0.0110 |
| nonmoving | pure p95_ms | 5 | 0.0272 | 0.0267 | 0.0305 |
| nonmoving | pure p99_ms | 5 | 0.0402 | 0.0381 | 0.0468 |
| nonmoving | pure max_ms | 5 | 0.1316 | 0.0871 | 0.3339 |
| nonmoving | manage p50_ms | 5 | 0.0469 | 0.0453 | 0.0482 |
| nonmoving | manage p95_ms | 5 | 0.1009 | 0.0993 | 0.1029 |
| nonmoving | manage p99_ms | 5 | 0.1311 | 0.1265 | 0.1409 |
| nonmoving | manage max_ms | 5 | 0.2934 | 0.2667 | 0.6557 |
| nonmoving | render p50_ms | 5 | 0.0383 | 0.0377 | 0.0385 |
| nonmoving | render p95_ms | 5 | 0.1772 | 0.1738 | 0.1814 |
| nonmoving | render p99_ms | 5 | 0.2415 | 0.2266 | 0.2562 |
| nonmoving | render max_ms | 5 | 0.3857 | 0.3507 | 0.5198 |
| nonmoving | WM allocated_bytes | 5 | 1357901952.0000 | 1356890984.0000 | 1360271920.0000 |
| nonmoving | WM max_residency_bytes | 5 | 5165816.0000 | 5127776.0000 | 5167568.0000 |
| nonmoving | WM max_stw_pause_ms | 5 | 0.8000 | 0.7000 | 1.0000 |
| nonmoving | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.0000 |

## Reference result

All ten trials met the adopted local engineering budgets. Across trials, pure
p99 was below 0.047 ms, manage p99 below 0.141 ms, and render p99 below
0.269 ms. The largest reported WM stop-the-world pause was 3.5 ms. These are
observations for this reference workload and host, not response-time guarantees.

Normal and nonmoving render p99 medians were 0.2394 ms and 0.2415 ms. Their
ranges overlap. Nonmoving had lower observed lifetime pause maxima but higher
WM residency, with only two major collections during each workload versus one
for normal GC. This evidence does not establish a production collector winner.
The diagnostic is threaded at `-N1`; production retains its existing defaults.

The host has an AMD Ryzen 5 7535HS with 12 logical CPUs. Trials ran on
2026-10-18 with GHC 9.6.7 and Cabal 3.10.3.0. The pinned GHC 9.6.6/Nix release
gate was unavailable locally and is not claimed here. One-minute host load
ranged from 0.77 to 2.44 at trial boundaries. External host activity could not
be controlled, so the collectors alternated without concurrent builds or
profiling. Each trial observed all 1023 native actions with zero sequence
failures or drops. The retained quantiles cover 61,002 sequence samples and
200,000 pure steps. Sequence counts are aggregate coverage, not a one-to-one
assignment of compositor sequences to actions.

See [summary.json](summary.json), [manifest.json](manifest.json), and the
[reproduction manual](../latency.md). The summary includes raw-file SHA-256
fingerprints. Full raw samples, logs, RTS summaries, and the separate eventlog
remain in ignored `artifacts/wp9/` in the measuring checkout. Repeating the
runner recreates those artifacts; the compact summary alone cannot revalidate
individual raw timestamps. WP10 remains the M1 closeout.

## Work and validity

| Trial | Manage | Render | Pure | Major GC in workload | Load before / after |
|---|---:|---:|---:|---|---|
| 00-normal | 3046 | 3046 | 20000 | True | [2.4365234375, 1.88330078125, 7.09716796875] / [2.31591796875, 1.92236328125, 6.94384765625] |
| 01-nonmoving | 3053 | 3053 | 20000 | True | [2.31591796875, 1.92236328125, 6.94384765625] / [2.34521484375, 1.96484375, 6.7705078125] |
| 02-normal | 3049 | 3049 | 20000 | True | [2.34521484375, 1.96484375, 6.7705078125] / [1.71533203125, 1.841796875, 6.5751953125] |
| 03-nonmoving | 3046 | 3046 | 20000 | True | [1.71533203125, 1.841796875, 6.5751953125] / [1.3623046875, 1.744140625, 6.39111328125] |
| 04-normal | 3042 | 3042 | 20000 | True | [1.3623046875, 1.744140625, 6.39111328125] / [1.12841796875, 1.64453125, 6.1845703125] |
| 05-nonmoving | 3055 | 3055 | 20000 | True | [1.12841796875, 1.64453125, 6.1845703125] / [0.99072265625, 1.552734375, 6.0078125] |
| 06-normal | 3055 | 3055 | 20000 | True | [0.99072265625, 1.552734375, 6.0078125] / [0.7734375, 1.44970703125, 5.8310546875] |
| 07-nonmoving | 3049 | 3049 | 20000 | True | [0.7734375, 1.44970703125, 5.8310546875] / [1.10986328125, 1.462890625, 5.671875] |
| 08-normal | 3057 | 3057 | 20000 | True | [1.10986328125, 1.462890625, 5.671875] / [1.26171875, 1.466796875, 5.53759765625] |
| 09-nonmoving | 3049 | 3049 | 20000 | True | [1.26171875, 1.466796875, 5.53759765625] / [2.44189453125, 1.7822265625, 5.49462890625] |

## Interpretation

Budgets are pure p99 < 2 ms, complete sequence p99 < 10 ms, and maximum observed stop-the-world pause < 10 ms. They are engineering budgets, not river guarantees. Per-trial results and budget outcomes are retained in summary.json; no ordinary unit test enforces wall-clock budgets.

RTS allocation and residency describe the WM process lifetime, including startup and shutdown. The `-s` per-generation collection maxima have 0.1 ms printed resolution. Nonmoving synchronization maxima are reported separately and included in the observed stop-the-world maximum; concurrent cycle duration is excluded. There is no forced final GC. The earlier RTSStats snapshot reports whether major GC occurred before shutdown. A trial without major GC gives limited collector evidence.

The pure interval includes concrete structural checksum folding over every state, layout, tag, stack, floating rectangle, intent, and desired-scene field. Inputs are pre-generated and previous state fully evaluated. Independent invariants and deterministic replay occur outside the interval. Replay, serialization, and validation still contribute to pure-process lifetime RTS totals.

Native actions wait for exact keyboard-owner changes. Initial and restored captures verify client colors and Tall geometry; final input delivery checks the target window. Captures verify pixels and may add render sequences. An idle-phase WM acknowledgment after the final capture ensures the last render completion precedes interval end. Signal/checkpoint overhead lies in the broad driver interval, outside sequence timers. Startup, warmup, cleanup, and samples crossing the interval boundary remain in raw wm.json but are excluded from quantiles. Driver/native-client and river costs are excluded from WM RTS statistics.

Scheduling, GC, and waiting remain possible limiters. Separate profile mode records a WM eventlog and Linux process CPU snapshots for WM, river, and driver; it is excluded from these trials. These observations do not identify source-level hotspots. No collector winner is inferred from noisy distributions or an unproven limiter.

The local compiler may differ from the repository's pinned GHC 9.6.6. Consult manifest.json for the exact compiler, build flags, platform, tools, trial order, and workload. Reproduction commands and limitations are in ../latency.md. WP10 remains the M1 closeout phase.

## Separate diagnostic profile

The separate normal-collector profile used the identical binary and 1023
native actions. Its broad driver interval was 28.709 s. There were 285 process
CPU observations inside that interval, spanning 28.600 s. WM CPU was 1.06 s,
river CPU was 2.41 s, and driver CPU was 4.86 s. These correspond to 3.71%,
8.43%, and 16.99% of one core over the sampled span. CPU is the difference in
user plus system ticks divided by 100 ticks/s; tick resolution is 10 ms.

No recorded long-lived process saturated a core. The driver spawns a short-lived
wtype for each action and waits for native owner events; those child CPU totals
are not summed here. This profile therefore does not prove a source-level WM
bottleneck or a collector improvement. The 903,209-byte eventlog is retained
for scheduler/GC investigation but was not parsed for source hotspots. Profile
samples do not enter the comparison tables. Exact endpoints, CPU counters,
GC deltas, binary identity and raw fingerprints are in
[profile-summary.json](profile-summary.json).

The GC interpretation follows the [GHC 9.6.7 runtime documentation](https://downloads.haskell.org/ghc/9.6.7/docs/users_guide/runtime_control.html)
and [GHC.Stats reference](https://downloads.haskell.org/ghc/9.6.7/docs/libraries/base-4.18.3.0/GHC-Stats.html).
