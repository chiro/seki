# Latency and collector evidence

Collector selection is inconclusive. Production defaults remain unchanged.

The diagnostic uses the same optimized threaded build at `-N1`; only `--nonmoving-gc` differs. WM sequence latency is request submission time, not compositor acknowledgment or display latency.

Trials per workload, revision, and collector: 5. Fewer than five is exploratory evidence only.
Machine: Linux thrush 7.2.11-arch1-1 #1 SMP PREEMPT_DYNAMIC Fri, 16 Oct 2026 00:28:58 +0000 x86_64; AMD Ryzen 5 7535HS with Radeon Graphics, 12 CPUs. River 0.5.0 +xwayland with `WLR_RENDERER=pixman`.
Workload m2: six native tiled windows and two parent-hinted dialog floats under Tall with Crop; focus, view, Shrink/Expand, and client fullscreen actions; 1023 acknowledged actions after 24 warmup actions.
Workload m3: two native tiles and a scratchpad window adopted by app ID under Tall; IPC workspace views, scratchpad toggles, and recompiles on idle river with no physical input; 2500 acknowledged actions after 24 warmup actions.
Seed 1909. Pure mode uses 32 windows, 20000 events, and 128 warmup events.

| Workload | Revision | Collector | Metric | Trials | Median | Min | Max |
|---|---|---|---|---:|---:|---:|---:|
| m2 | trunk | normal | pure p50_ms | 5 | 0.0163 | 0.0161 | 0.0164 |
| m2 | trunk | normal | pure p95_ms | 5 | 0.0444 | 0.0443 | 0.0448 |
| m2 | trunk | normal | pure p99_ms | 5 | 0.0524 | 0.0514 | 0.0539 |
| m2 | trunk | normal | pure max_ms | 5 | 0.1459 | 0.1056 | 3.1012 |
| m2 | trunk | normal | manage p50_ms | 5 | 0.0663 | 0.0657 | 0.0686 |
| m2 | trunk | normal | manage p95_ms | 5 | 0.1483 | 0.1471 | 0.1503 |
| m2 | trunk | normal | manage p99_ms | 5 | 0.1889 | 0.1840 | 0.1918 |
| m2 | trunk | normal | manage max_ms | 5 | 0.3600 | 0.3033 | 0.4460 |
| m2 | trunk | normal | render p50_ms | 5 | 0.0438 | 0.0430 | 0.0450 |
| m2 | trunk | normal | render p95_ms | 5 | 0.1874 | 0.1777 | 0.1895 |
| m2 | trunk | normal | render p99_ms | 5 | 0.2360 | 0.2134 | 0.2463 |
| m2 | trunk | normal | render max_ms | 5 | 0.4000 | 0.3468 | 1.9507 |
| m2 | trunk | normal | WM allocated_bytes | 5 | 1371389920.0000 | 1367783984.0000 | 1376278776.0000 |
| m2 | trunk | normal | WM max_residency_bytes | 5 | 2146000.0000 | 2101008.0000 | 2156248.0000 |
| m2 | trunk | normal | WM max_stw_pause_ms | 5 | 1.7000 | 1.7000 | 1.8000 |
| m2 | trunk | normal | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.0000 |
| m2 | head | normal | pure p50_ms | 5 | 0.0163 | 0.0161 | 0.0169 |
| m2 | head | normal | pure p95_ms | 5 | 0.0443 | 0.0438 | 0.0453 |
| m2 | head | normal | pure p99_ms | 5 | 0.0520 | 0.0509 | 0.0545 |
| m2 | head | normal | pure max_ms | 5 | 0.1212 | 0.0842 | 0.2288 |
| m2 | head | normal | manage p50_ms | 5 | 0.0659 | 0.0649 | 0.0696 |
| m2 | head | normal | manage p95_ms | 5 | 0.1499 | 0.1445 | 0.1503 |
| m2 | head | normal | manage p99_ms | 5 | 0.1866 | 0.1840 | 0.1938 |
| m2 | head | normal | manage max_ms | 5 | 0.3297 | 0.3028 | 1.8188 |
| m2 | head | normal | render p50_ms | 5 | 0.0428 | 0.0418 | 0.0443 |
| m2 | head | normal | render p95_ms | 5 | 0.1863 | 0.1838 | 0.1880 |
| m2 | head | normal | render p99_ms | 5 | 0.2351 | 0.2261 | 0.2676 |
| m2 | head | normal | render max_ms | 5 | 0.3915 | 0.3278 | 2.5260 |
| m2 | head | normal | WM allocated_bytes | 5 | 1373526264.0000 | 1365393728.0000 | 1374684112.0000 |
| m2 | head | normal | WM max_residency_bytes | 5 | 2099616.0000 | 2060464.0000 | 2157008.0000 |
| m2 | head | normal | WM max_stw_pause_ms | 5 | 2.1000 | 1.7000 | 2.5000 |
| m2 | head | normal | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.0000 |
| m3 | head | normal | pure p50_ms | 5 | 0.0205 | 0.0200 | 0.0212 |
| m3 | head | normal | pure p95_ms | 5 | 0.0439 | 0.0434 | 0.0464 |
| m3 | head | normal | pure p99_ms | 5 | 0.0517 | 0.0506 | 0.0539 |
| m3 | head | normal | pure max_ms | 5 | 0.0961 | 0.0870 | 0.1718 |
| m3 | head | normal | manage p50_ms | 5 | 0.0849 | 0.0843 | 0.0849 |
| m3 | head | normal | manage p95_ms | 5 | 0.1908 | 0.1874 | 0.1928 |
| m3 | head | normal | manage p99_ms | 5 | 0.2516 | 0.2439 | 0.2704 |
| m3 | head | normal | manage max_ms | 5 | 2.7035 | 0.7074 | 3.3461 |
| m3 | head | normal | render p50_ms | 5 | 0.0534 | 0.0525 | 0.0549 |
| m3 | head | normal | render p95_ms | 5 | 0.1541 | 0.1455 | 0.1553 |
| m3 | head | normal | render p99_ms | 5 | 0.2176 | 0.2078 | 0.2430 |
| m3 | head | normal | render max_ms | 5 | 2.9026 | 2.7498 | 5.2563 |
| m3 | head | normal | WM allocated_bytes | 5 | 3110289960.0000 | 3108719816.0000 | 3115678640.0000 |
| m3 | head | normal | WM max_residency_bytes | 5 | 3673944.0000 | 3562840.0000 | 5836464.0000 |
| m3 | head | normal | WM max_stw_pause_ms | 5 | 5.3000 | 4.9000 | 6.3000 |
| m3 | head | normal | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.0000 |
| m2 | trunk | nonmoving | pure p50_ms | 5 | 0.0162 | 0.0162 | 0.0163 |
| m2 | trunk | nonmoving | pure p95_ms | 5 | 0.0443 | 0.0443 | 0.0443 |
| m2 | trunk | nonmoving | pure p99_ms | 5 | 0.0525 | 0.0510 | 0.0535 |
| m2 | trunk | nonmoving | pure max_ms | 5 | 0.2105 | 0.1894 | 0.2303 |
| m2 | trunk | nonmoving | manage p50_ms | 5 | 0.0693 | 0.0677 | 0.0702 |
| m2 | trunk | nonmoving | manage p95_ms | 5 | 0.1497 | 0.1445 | 0.1536 |
| m2 | trunk | nonmoving | manage p99_ms | 5 | 0.1837 | 0.1740 | 0.1908 |
| m2 | trunk | nonmoving | manage max_ms | 5 | 0.3308 | 0.3085 | 0.4090 |
| m2 | trunk | nonmoving | render p50_ms | 5 | 0.0443 | 0.0435 | 0.0448 |
| m2 | trunk | nonmoving | render p95_ms | 5 | 0.1865 | 0.1845 | 0.1890 |
| m2 | trunk | nonmoving | render p99_ms | 5 | 0.2353 | 0.2299 | 0.2404 |
| m2 | trunk | nonmoving | render max_ms | 5 | 0.4274 | 0.3895 | 0.4561 |
| m2 | trunk | nonmoving | WM allocated_bytes | 5 | 1371124296.0000 | 1366493808.0000 | 1373345600.0000 |
| m2 | trunk | nonmoving | WM max_residency_bytes | 5 | 4629552.0000 | 4515264.0000 | 4708424.0000 |
| m2 | trunk | nonmoving | WM max_stw_pause_ms | 5 | 0.7000 | 0.6000 | 0.7000 |
| m2 | trunk | nonmoving | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.2000 |
| m2 | head | nonmoving | pure p50_ms | 5 | 0.0162 | 0.0161 | 0.0163 |
| m2 | head | nonmoving | pure p95_ms | 5 | 0.0444 | 0.0440 | 0.0446 |
| m2 | head | nonmoving | pure p99_ms | 5 | 0.0515 | 0.0513 | 0.0538 |
| m2 | head | nonmoving | pure max_ms | 5 | 0.1254 | 0.1203 | 0.1318 |
| m2 | head | nonmoving | manage p50_ms | 5 | 0.0677 | 0.0663 | 0.0682 |
| m2 | head | nonmoving | manage p95_ms | 5 | 0.1490 | 0.1487 | 0.1496 |
| m2 | head | nonmoving | manage p99_ms | 5 | 0.1832 | 0.1803 | 0.1915 |
| m2 | head | nonmoving | manage max_ms | 5 | 0.3601 | 0.3306 | 0.4115 |
| m2 | head | nonmoving | render p50_ms | 5 | 0.0434 | 0.0432 | 0.0448 |
| m2 | head | nonmoving | render p95_ms | 5 | 0.1861 | 0.1828 | 0.1902 |
| m2 | head | nonmoving | render p99_ms | 5 | 0.2308 | 0.2278 | 0.2509 |
| m2 | head | nonmoving | render max_ms | 5 | 0.4271 | 0.3996 | 0.5321 |
| m2 | head | nonmoving | WM allocated_bytes | 5 | 1372820208.0000 | 1368156856.0000 | 1374858336.0000 |
| m2 | head | nonmoving | WM max_residency_bytes | 5 | 4564800.0000 | 4446360.0000 | 4711256.0000 |
| m2 | head | nonmoving | WM max_stw_pause_ms | 5 | 0.7000 | 0.6000 | 0.9000 |
| m2 | head | nonmoving | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.0000 |
| m3 | head | nonmoving | pure p50_ms | 5 | 0.0200 | 0.0198 | 0.0204 |
| m3 | head | nonmoving | pure p95_ms | 5 | 0.0435 | 0.0428 | 0.0443 |
| m3 | head | nonmoving | pure p99_ms | 5 | 0.0511 | 0.0499 | 0.0538 |
| m3 | head | nonmoving | pure max_ms | 5 | 0.1512 | 0.0886 | 0.1729 |
| m3 | head | nonmoving | manage p50_ms | 5 | 0.0846 | 0.0795 | 0.0854 |
| m3 | head | nonmoving | manage p95_ms | 5 | 0.1918 | 0.1668 | 0.1937 |
| m3 | head | nonmoving | manage p99_ms | 5 | 0.2685 | 0.2255 | 0.2961 |
| m3 | head | nonmoving | manage max_ms | 5 | 3.6649 | 0.7125 | 4.0979 |
| m3 | head | nonmoving | render p50_ms | 5 | 0.0534 | 0.0499 | 0.0539 |
| m3 | head | nonmoving | render p95_ms | 5 | 0.1421 | 0.1306 | 0.1541 |
| m3 | head | nonmoving | render p99_ms | 5 | 0.2274 | 0.2025 | 0.2501 |
| m3 | head | nonmoving | render max_ms | 5 | 3.0482 | 0.8806 | 3.5494 |
| m3 | head | nonmoving | WM allocated_bytes | 5 | 3111426648.0000 | 3104275144.0000 | 3117546840.0000 |
| m3 | head | nonmoving | WM max_residency_bytes | 5 | 8399712.0000 | 7921104.0000 | 8431416.0000 |
| m3 | head | nonmoving | WM max_stw_pause_ms | 5 | 1.1000 | 1.1000 | 1.8000 |
| m3 | head | nonmoving | WM nonmoving_sync_max_pause_ms | 5 | 0.7000 | 0.1000 | 0.9000 |

## Budgets

Budgets are pure p99 < 2 ms, manage and render sequence p99 < 10 ms, and maximum observed WM stop-the-world pause < 10 ms. An m3 trial must also meet each command target that names one.

| Trial | Revision | Workload | Collector | Pure p99 | Manage p99 | Render p99 | Max STW | Endpoint p99 | Pass |
|---|---|---|---|---:|---:|---:|---:|---|---|
| 00-trunk-m2-normal | trunk | m2 | normal | 0.0522 | 0.1856 | 0.2463 | 1.7 | none | pass |
| 01-head-m2-normal | head | m2 | normal | 0.0525 | 0.1938 | 0.2296 | 1.8 | none | pass |
| 02-head-m3-normal | head | m3 | normal | 0.0507 | 0.2517 | 0.2173 | 4.9 | external_wake 2.22, ipc_update 1.10, recompile_completion 46.68, scratchpad_toggle 2.03 | pass |
| 03-trunk-m2-nonmoving | trunk | m2 | nonmoving | 0.0534 | 0.1740 | 0.2299 | 0.6 | none | pass |
| 04-head-m2-nonmoving | head | m2 | nonmoving | 0.0515 | 0.1832 | 0.2307 | 0.6 | none | pass |
| 05-head-m3-nonmoving | head | m3 | nonmoving | 0.0505 | 0.2495 | 0.2274 | 1.1 | external_wake 2.09, ipc_update 1.02, recompile_completion 46.58, scratchpad_toggle 2.24 | pass |
| 06-head-m2-normal | head | m2 | normal | 0.0520 | 0.1908 | 0.2676 | 2.1 | none | pass |
| 07-trunk-m2-normal | trunk | m2 | normal | 0.0524 | 0.1918 | 0.2407 | 1.8 | none | pass |
| 08-head-m3-normal | head | m3 | normal | 0.0517 | 0.2704 | 0.2176 | 5.8 | external_wake 2.26, ipc_update 1.31, recompile_completion 48.35, scratchpad_toggle 1.94 | pass |
| 09-head-m2-nonmoving | head | m2 | nonmoving | 0.0513 | 0.1830 | 0.2509 | 0.9 | none | pass |
| 10-trunk-m2-nonmoving | trunk | m2 | nonmoving | 0.0535 | 0.1908 | 0.2375 | 0.7 | none | pass |
| 11-head-m3-nonmoving | head | m3 | nonmoving | 0.0511 | 0.2255 | 0.2312 | 1.1 | external_wake 2.05, ipc_update 1.10, recompile_completion 46.80, scratchpad_toggle 1.96 | pass |
| 12-trunk-m2-normal | trunk | m2 | normal | 0.0514 | 0.1840 | 0.2134 | 1.8 | none | pass |
| 13-head-m2-normal | head | m2 | normal | 0.0545 | 0.1840 | 0.2477 | 2.5 | none | pass |
| 14-head-m3-normal | head | m3 | normal | 0.0518 | 0.2516 | 0.2078 | 6.3 | external_wake 2.10, ipc_update 1.12, recompile_completion 47.02, scratchpad_toggle 1.95 | pass |
| 15-trunk-m2-nonmoving | trunk | m2 | nonmoving | 0.0510 | 0.1855 | 0.2353 | 0.7 | none | pass |
| 16-head-m2-nonmoving | head | m2 | nonmoving | 0.0515 | 0.1915 | 0.2278 | 0.6 | none | pass |
| 17-head-m3-nonmoving | head | m3 | nonmoving | 0.0499 | 0.2834 | 0.2200 | 1.1 | external_wake 2.12, ipc_update 1.21, recompile_completion 46.29, scratchpad_toggle 2.02 | pass |
| 18-head-m2-normal | head | m2 | normal | 0.0509 | 0.1857 | 0.2351 | 2.3 | none | pass |
| 19-trunk-m2-normal | trunk | m2 | normal | 0.0539 | 0.1908 | 0.2360 | 1.7 | none | pass |
| 20-head-m3-normal | head | m3 | normal | 0.0539 | 0.2451 | 0.2430 | 5.3 | external_wake 2.13, ipc_update 1.13, recompile_completion 46.12, scratchpad_toggle 1.99 | pass |
| 21-head-m2-nonmoving | head | m2 | nonmoving | 0.0514 | 0.1841 | 0.2308 | 0.8 | none | pass |
| 22-trunk-m2-nonmoving | trunk | m2 | nonmoving | 0.0524 | 0.1831 | 0.2345 | 0.6 | none | pass |
| 23-head-m3-nonmoving | head | m3 | nonmoving | 0.0538 | 0.2961 | 0.2501 | 1.8 | external_wake 2.21, ipc_update 1.25, recompile_completion 46.21, scratchpad_toggle 2.01 | pass |
| 24-trunk-m2-normal | trunk | m2 | normal | 0.0529 | 0.1889 | 0.2248 | 1.7 | none | pass |
| 25-head-m2-normal | head | m2 | normal | 0.0511 | 0.1866 | 0.2261 | 1.7 | none | pass |
| 26-head-m3-normal | head | m3 | normal | 0.0506 | 0.2439 | 0.2303 | 5.1 | external_wake 2.27, ipc_update 1.06, recompile_completion 47.25, scratchpad_toggle 2.16 | pass |
| 27-trunk-m2-nonmoving | trunk | m2 | nonmoving | 0.0525 | 0.1837 | 0.2404 | 0.7 | none | pass |
| 28-head-m2-nonmoving | head | m2 | nonmoving | 0.0538 | 0.1803 | 0.2387 | 0.7 | none | pass |
| 29-head-m3-nonmoving | head | m3 | nonmoving | 0.0511 | 0.2685 | 0.2025 | 1.1 | external_wake 2.10, ipc_update 1.04, recompile_completion 46.63, scratchpad_toggle 2.01 | pass |

All 30 trials met every budget.

## Revisions

| Revision | Commit | Compiler | Binary SHA-256 | Linked seki libraries (SHA-256) |
|---|---|---|---|---|
| trunk | `3c3037369eed28e5dc59528046afa4b7a1309cae` | 9.10.3 | `47c3269edab5dd4455b0e742b6764816c94d9e26aac31123795c24f5ebc5a354` | `libHSseki-0.1.0.0-inplace-layer-surface-fixture-ghc9.10.3.so` `5b4398a6e2dbe724b2e8eee821d753d9c012931fff04545e1b6691d590a87703`<br>`libHSseki-0.1.0.0-inplace-seki-wire-ghc9.10.3.so` `2d86dec6f516cd264129458591f2c935e0180bf2fe6d6ac1b731ce3743c2343c`<br>`libHSseki-0.1.0.0-inplace-seki-core-ghc9.10.3.so` `377a4cfc5d9d49f30fc13c75d5e7253c578c63046b94bd57bf8a9d447c1c6eba` |
| head | `82a31cc298f9f5200788b2898a36dd248cfefdd0` | 9.10.3 | `9df3839cd62f480d8d7cbdb503595c9b61453660f4308b73bc4194db91248fb8` | `libHSseki-0.1.0.0-inplace-layer-surface-fixture-ghc9.10.3.so` `01a441aace501b6441dd8e355a3992bdf57362c646afcd73c87c1fb5fdfec152`<br>`libHSseki-0.1.0.0-inplace-seki-wire-ghc9.10.3.so` `99ff735438a64e5bcdf1792439a464333c64967c8ee7569c799d2e380d14fba4`<br>`libHSseki-0.1.0.0-inplace-seki-core-ghc9.10.3.so` `377a4cfc5d9d49f30fc13c75d5e7253c578c63046b94bd57bf8a9d447c1c6eba` |

Runtime sources under `src`, `src-core`, `src-public`, `src-generated`, and `cbits` are identical across the revisions, so their m2 manage and render differences are a noise floor.
Latency driver sources under `tools/latency` and `integration/Seki/Integration/Harness.hs` differ across the revisions. The pure interval includes the driver's checksum fold, so matching pure checksums show the same output, not the same cost.
This report claims no speedup or regression.
m2 pure checksums agree across revisions.
trunk does not run m3: invalid workload: m3.

## M3 command endpoints

Each sample runs from the driver's send of one IPC command to the outcome named below, on idle river after a 1 ms gap, with no physical input. Probe outcomes are polled every 0.1 ms; subscriber lines are stamped on arrival. No ratio compares these with the m2 sequences.

- `external_wake`: IPC workspace command sent to the probe's keyboard owner change.
- `ipc_update`: the same command to the subscriber line that publishes the new focused workspace.
- `scratchpad_toggle`: IPC scratchpad toggle to the probe's keyboard owner change.
- `recompile_completion`: IPC recompile to the subscriber line that publishes the build generation's result.
- `input_result` is unsupported: river 0.5.0 lists no input device for the fixture's wtype virtual keyboard, so no input rule has a device to report on; integration/input/backend.so creates devices only when preloaded into river, which m3-input-perf measures.
- `recompile_handover` is unsupported: a validated build makes the WM stop managing and exec its target, which ends the process that owns the trial's samples; the stub candidate fails its check instead, so the m3-recompile integration lane covers the handover.

| Trial | Endpoint | Samples | p50 ms | p95 ms | p99 ms | Max ms | Target p99 |
|---|---|---:|---:|---:|---:|---:|---|
| 02-head-m3-normal | external_wake | 1000 | 1.4124 | 1.8469 | 2.2210 | 13.4704 | < 50 ms |
| 02-head-m3-normal | ipc_update | 1000 | 0.5103 | 0.8394 | 1.0995 | 3.1480 | < 50 ms |
| 02-head-m3-normal | recompile_completion | 499 | 43.5718 | 45.4505 | 46.6829 | 48.6031 | none |
| 02-head-m3-normal | scratchpad_toggle | 1001 | 1.4517 | 1.7829 | 2.0327 | 6.1513 | < 50 ms |
| 05-head-m3-nonmoving | external_wake | 1000 | 1.4042 | 1.8498 | 2.0873 | 13.8736 | < 50 ms |
| 05-head-m3-nonmoving | ipc_update | 1000 | 0.5024 | 0.8413 | 1.0245 | 1.7289 | < 50 ms |
| 05-head-m3-nonmoving | recompile_completion | 499 | 43.6174 | 45.3151 | 46.5816 | 48.1357 | none |
| 05-head-m3-nonmoving | scratchpad_toggle | 1001 | 1.4222 | 1.7888 | 2.2445 | 4.0007 | < 50 ms |
| 08-head-m3-normal | external_wake | 1000 | 1.4365 | 1.9049 | 2.2623 | 14.0121 | < 50 ms |
| 08-head-m3-normal | ipc_update | 1000 | 0.5178 | 0.8606 | 1.3127 | 3.5530 | < 50 ms |
| 08-head-m3-normal | recompile_completion | 499 | 43.7161 | 45.8916 | 48.3506 | 60.6234 | none |
| 08-head-m3-normal | scratchpad_toggle | 1001 | 1.4371 | 1.7603 | 1.9419 | 2.5084 | < 50 ms |
| 11-head-m3-nonmoving | external_wake | 1000 | 1.3623 | 1.7410 | 2.0548 | 13.6214 | < 50 ms |
| 11-head-m3-nonmoving | ipc_update | 1000 | 0.4392 | 0.7604 | 1.1017 | 2.9236 | < 50 ms |
| 11-head-m3-nonmoving | recompile_completion | 499 | 42.2496 | 45.1367 | 46.8040 | 47.3774 | none |
| 11-head-m3-nonmoving | scratchpad_toggle | 1001 | 1.3790 | 1.7444 | 1.9628 | 2.6433 | < 50 ms |
| 14-head-m3-normal | external_wake | 1000 | 1.3805 | 1.7962 | 2.0971 | 13.5619 | < 50 ms |
| 14-head-m3-normal | ipc_update | 1000 | 0.5024 | 0.8495 | 1.1227 | 4.3719 | < 50 ms |
| 14-head-m3-normal | recompile_completion | 499 | 43.5847 | 45.4925 | 47.0234 | 49.9692 | none |
| 14-head-m3-normal | scratchpad_toggle | 1001 | 1.4057 | 1.7624 | 1.9545 | 4.2878 | < 50 ms |
| 17-head-m3-nonmoving | external_wake | 1000 | 1.4445 | 1.8930 | 2.1163 | 13.6302 | < 50 ms |
| 17-head-m3-nonmoving | ipc_update | 1000 | 0.5145 | 0.8592 | 1.2148 | 2.7433 | < 50 ms |
| 17-head-m3-nonmoving | recompile_completion | 499 | 43.6423 | 45.4573 | 46.2871 | 58.9582 | none |
| 17-head-m3-nonmoving | scratchpad_toggle | 1001 | 1.4522 | 1.7733 | 2.0153 | 4.9476 | < 50 ms |
| 20-head-m3-normal | external_wake | 1000 | 1.3735 | 1.8863 | 2.1311 | 6.2183 | < 50 ms |
| 20-head-m3-normal | ipc_update | 1000 | 0.5147 | 0.8618 | 1.1336 | 4.8399 | < 50 ms |
| 20-head-m3-normal | recompile_completion | 499 | 43.6289 | 45.2989 | 46.1199 | 57.5480 | none |
| 20-head-m3-normal | scratchpad_toggle | 1001 | 1.4183 | 1.7615 | 1.9936 | 14.1751 | < 50 ms |
| 23-head-m3-nonmoving | external_wake | 1000 | 1.4326 | 1.8675 | 2.2079 | 10.6610 | < 50 ms |
| 23-head-m3-nonmoving | ipc_update | 1000 | 0.5043 | 0.8592 | 1.2526 | 3.5090 | < 50 ms |
| 23-head-m3-nonmoving | recompile_completion | 499 | 43.5305 | 45.3846 | 46.2079 | 48.1865 | none |
| 23-head-m3-nonmoving | scratchpad_toggle | 1001 | 1.4220 | 1.7708 | 2.0118 | 3.4904 | < 50 ms |
| 26-head-m3-normal | external_wake | 1000 | 1.4302 | 1.8934 | 2.2650 | 13.2610 | < 50 ms |
| 26-head-m3-normal | ipc_update | 1000 | 0.5235 | 0.8279 | 1.0555 | 5.8184 | < 50 ms |
| 26-head-m3-normal | recompile_completion | 499 | 43.8420 | 45.8853 | 47.2543 | 48.6949 | none |
| 26-head-m3-normal | scratchpad_toggle | 1001 | 1.4481 | 1.8081 | 2.1568 | 4.9497 | < 50 ms |
| 29-head-m3-nonmoving | external_wake | 1000 | 1.4250 | 1.8500 | 2.1045 | 13.5556 | < 50 ms |
| 29-head-m3-nonmoving | ipc_update | 1000 | 0.5147 | 0.8229 | 1.0417 | 3.3984 | < 50 ms |
| 29-head-m3-nonmoving | recompile_completion | 499 | 43.7507 | 45.5943 | 46.6308 | 58.2476 | none |
| 29-head-m3-nonmoving | scratchpad_toggle | 1001 | 1.4427 | 1.7732 | 2.0137 | 4.6060 | < 50 ms |

## Work and validity

Sequences per action count the manage and render sequences inside the interval for each measured action. A rise against the same workload points at repeated proposals.

| Trial | Manage | Render | Manage per action | Render per action | Pure | Major GC in workload | Load before / after |
|---|---:|---:|---:|---:|---:|---|---|
| 00-trunk-m2-normal | 2906 | 2906 | 2.84 | 2.84 | 20000 | True | [0.97216796875, 0.73828125, 0.55712890625] / [0.77392578125, 0.71875, 0.556640625] |
| 01-head-m2-normal | 2893 | 2893 | 2.83 | 2.83 | 20000 | True | [0.77392578125, 0.71875, 0.556640625] / [0.83203125, 0.7353515625, 0.5673828125] |
| 02-head-m3-normal | 5473 | 5473 | 2.19 | 2.19 | 20000 | True | [0.83203125, 0.7353515625, 0.5673828125] / [0.78369140625, 0.72998046875, 0.5703125] |
| 03-trunk-m2-nonmoving | 2907 | 2907 | 2.84 | 2.84 | 20000 | True | [0.78369140625, 0.72998046875, 0.5703125] / [0.69580078125, 0.712890625, 0.56982421875] |
| 04-head-m2-nonmoving | 2900 | 2900 | 2.83 | 2.83 | 20000 | True | [0.69580078125, 0.712890625, 0.56982421875] / [0.552734375, 0.67578125, 0.56103515625] |
| 05-head-m3-nonmoving | 5465 | 5465 | 2.19 | 2.19 | 20000 | True | [0.552734375, 0.67578125, 0.56103515625] / [0.552734375, 0.65966796875, 0.5576171875] |
| 06-head-m2-normal | 2910 | 2910 | 2.84 | 2.84 | 20000 | True | [0.552734375, 0.65966796875, 0.5576171875] / [0.67822265625, 0.681640625, 0.56787109375] |
| 07-trunk-m2-normal | 2917 | 2917 | 2.85 | 2.85 | 20000 | True | [0.67822265625, 0.681640625, 0.56787109375] / [0.6201171875, 0.6689453125, 0.5673828125] |
| 08-head-m3-normal | 5460 | 5460 | 2.18 | 2.18 | 20000 | True | [0.6201171875, 0.6689453125, 0.5673828125] / [0.70849609375, 0.6845703125, 0.5751953125] |
| 09-head-m2-nonmoving | 2914 | 2914 | 2.85 | 2.85 | 20000 | True | [0.70849609375, 0.6845703125, 0.5751953125] / [0.52734375, 0.64013671875, 0.56298828125] |
| 10-trunk-m2-nonmoving | 2908 | 2908 | 2.84 | 2.84 | 20000 | True | [0.52734375, 0.64013671875, 0.56298828125] / [0.6181640625, 0.6572265625, 0.57080078125] |
| 11-head-m3-nonmoving | 5452 | 5452 | 2.18 | 2.18 | 20000 | True | [0.6181640625, 0.6572265625, 0.57080078125] / [0.5068359375, 0.62548828125, 0.56201171875] |
| 12-trunk-m2-normal | 2902 | 2902 | 2.84 | 2.84 | 20000 | True | [0.5068359375, 0.62548828125, 0.56201171875] / [0.392578125, 0.58642578125, 0.5498046875] |
| 13-head-m2-normal | 2890 | 2890 | 2.83 | 2.83 | 20000 | True | [0.392578125, 0.58642578125, 0.5498046875] / [0.52685546875, 0.6005859375, 0.5546875] |
| 14-head-m3-normal | 5458 | 5458 | 2.18 | 2.18 | 20000 | True | [0.52685546875, 0.6005859375, 0.5546875] / [0.640625, 0.62353515625, 0.5634765625] |
| 15-trunk-m2-nonmoving | 2909 | 2909 | 2.84 | 2.84 | 20000 | True | [0.640625, 0.62353515625, 0.5634765625] / [1.0556640625, 0.71630859375, 0.595703125] |
| 16-head-m2-nonmoving | 2910 | 2910 | 2.84 | 2.84 | 20000 | True | [1.0556640625, 0.71630859375, 0.595703125] / [1.09521484375, 0.76025390625, 0.615234375] |
| 17-head-m3-nonmoving | 5463 | 5463 | 2.19 | 2.19 | 20000 | True | [1.09521484375, 0.76025390625, 0.615234375] / [0.9912109375, 0.76806640625, 0.62353515625] |
| 18-head-m2-normal | 2914 | 2914 | 2.85 | 2.85 | 20000 | True | [0.9912109375, 0.76806640625, 0.62353515625] / [0.8056640625, 0.74658203125, 0.623046875] |
| 19-trunk-m2-normal | 2895 | 2895 | 2.83 | 2.83 | 20000 | True | [0.8056640625, 0.74658203125, 0.623046875] / [0.76318359375, 0.74365234375, 0.62841796875] |
| 20-head-m3-normal | 5477 | 5477 | 2.19 | 2.19 | 20000 | True | [0.76318359375, 0.74365234375, 0.62841796875] / [0.7119140625, 0.7353515625, 0.6298828125] |
| 21-head-m2-nonmoving | 2909 | 2909 | 2.84 | 2.84 | 20000 | True | [0.7119140625, 0.7353515625, 0.6298828125] / [0.78076171875, 0.7490234375, 0.63916015625] |
| 22-trunk-m2-nonmoving | 2894 | 2894 | 2.83 | 2.83 | 20000 | True | [0.78076171875, 0.7490234375, 0.63916015625] / [0.810546875, 0.76220703125, 0.64697265625] |
| 23-head-m3-nonmoving | 5463 | 5463 | 2.19 | 2.19 | 20000 | True | [0.810546875, 0.76220703125, 0.64697265625] / [0.70751953125, 0.73828125, 0.64111328125] |
| 24-trunk-m2-normal | 2908 | 2908 | 2.84 | 2.84 | 20000 | True | [0.70751953125, 0.73828125, 0.64111328125] / [0.80126953125, 0.7490234375, 0.64697265625] |
| 25-head-m2-normal | 2911 | 2911 | 2.85 | 2.85 | 20000 | True | [0.80126953125, 0.7490234375, 0.64697265625] / [0.81982421875, 0.7607421875, 0.65380859375] |
| 26-head-m3-normal | 5463 | 5463 | 2.19 | 2.19 | 20000 | True | [0.81982421875, 0.7607421875, 0.65380859375] / [0.79931640625, 0.76708984375, 0.6591796875] |
| 27-trunk-m2-nonmoving | 2911 | 2911 | 2.85 | 2.85 | 20000 | True | [0.79931640625, 0.76708984375, 0.6591796875] / [0.8994140625, 0.7939453125, 0.671875] |
| 28-head-m2-nonmoving | 2899 | 2899 | 2.83 | 2.83 | 20000 | True | [0.8994140625, 0.7939453125, 0.671875] / [0.712890625, 0.767578125, 0.66748046875] |
| 29-head-m3-nonmoving | 5471 | 5471 | 2.19 | 2.19 | 20000 | True | [0.712890625, 0.767578125, 0.66748046875] / [0.6220703125, 0.7412109375, 0.66162109375] |

## Exercised M2 features

Counts come from the model's transitions over the measured actions. Every count except float raises changed what the probe observed, so a barrier confirmed it.

| Trial | clip_resizes | fullscreen_enters | fullscreen_exits_by_focus | fullscreen_exits_by_request | hidden_fullscreen_views | float_raises |
|---|---:|---:|---:|---:|---:|---:|
| 00-trunk-m2-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 01-head-m2-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 03-trunk-m2-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 04-head-m2-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 06-head-m2-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 07-trunk-m2-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 09-head-m2-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 10-trunk-m2-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 12-trunk-m2-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 13-head-m2-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 15-trunk-m2-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 16-head-m2-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 18-head-m2-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 19-trunk-m2-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 21-head-m2-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 22-trunk-m2-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 24-trunk-m2-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 25-head-m2-normal | 225 | 90 | 58 | 32 | 90 | 24 |
| 27-trunk-m2-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |
| 28-head-m2-nonmoving | 225 | 90 | 58 | 32 | 90 | 24 |

## Interpretation

Per-trial results and budget outcomes are retained in summary.json. Budgets are engineering budgets, not river guarantees, and no ordinary unit test enforces wall-clock budgets.

RTS allocation and residency describe the WM process lifetime, including startup and shutdown. The `-s` per-generation collection maxima have 0.1 ms printed resolution. Nonmoving synchronization maxima are reported separately and included in the observed stop-the-world maximum; concurrent cycle duration is excluded. There is no forced final GC. The earlier RTSStats snapshot reports whether major GC occurred before shutdown. A trial without major GC gives limited collector evidence.

The pure interval includes concrete structural checksum folding over every state, layout, tag, stack, floating rectangle, fullscreen window, pointer op, intent, and desired-scene field. Inputs are pre-generated and previous state fully evaluated. Independent invariants and deterministic replay occur outside the interval. Replay, serialization, and validation still contribute to pure-process lifetime RTS totals.

Each native action waits until the probe observes the expected keyboard owner and the size every window last committed. Pictures are polled and checked pixel by pixel only outside the measured interval: before it, after it, and after the restoring actions. Final input delivery checks the target window. An idle-phase WM acknowledgment after the last action ensures the last completed render precedes interval end. Signal/checkpoint overhead lies in the broad driver interval, outside sequence timers. Startup, warmup, cleanup, and samples crossing the interval boundary remain in raw wm.json but are excluded from quantiles. Driver/native-client and river costs are excluded from WM RTS statistics.

Scheduling, GC, and waiting remain possible limiters. Separate profile mode records a WM eventlog and Linux process CPU snapshots for WM, river, and driver; it is excluded from these trials. These observations do not identify source-level hotspots. No collector winner is inferred from noisy distributions or an unproven limiter.

The local compiler may differ from the repository's pinned GHC 9.10.3. Consult manifest.json for the exact compiler, build flags, platform, tools, trial order, and workloads. Reproduction commands and limitations are in docs/latency.md.
