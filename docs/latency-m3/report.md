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
| m2 | trunk | normal | pure p50_ms | 5 | 0.0163 | 0.0163 | 0.0163 |
| m2 | trunk | normal | pure p95_ms | 5 | 0.0443 | 0.0439 | 0.0444 |
| m2 | trunk | normal | pure p99_ms | 5 | 0.0525 | 0.0509 | 0.0530 |
| m2 | trunk | normal | pure max_ms | 5 | 0.1129 | 0.0891 | 2.7763 |
| m2 | trunk | normal | manage p50_ms | 5 | 0.0673 | 0.0663 | 0.0701 |
| m2 | trunk | normal | manage p95_ms | 5 | 0.1502 | 0.1479 | 0.1525 |
| m2 | trunk | normal | manage p99_ms | 5 | 0.1864 | 0.1819 | 0.1979 |
| m2 | trunk | normal | manage max_ms | 5 | 0.3565 | 0.3232 | 0.4369 |
| m2 | trunk | normal | render p50_ms | 5 | 0.0440 | 0.0432 | 0.0453 |
| m2 | trunk | normal | render p95_ms | 5 | 0.1864 | 0.1821 | 0.1888 |
| m2 | trunk | normal | render p99_ms | 5 | 0.2228 | 0.2182 | 0.2560 |
| m2 | trunk | normal | render max_ms | 5 | 0.4146 | 0.3598 | 0.4299 |
| m2 | trunk | normal | WM allocated_bytes | 5 | 1372841792.0000 | 1369122984.0000 | 1374367024.0000 |
| m2 | trunk | normal | WM max_residency_bytes | 5 | 2141288.0000 | 2137048.0000 | 2297832.0000 |
| m2 | trunk | normal | WM max_stw_pause_ms | 5 | 2.1000 | 1.7000 | 3.7000 |
| m2 | trunk | normal | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.0000 |
| m2 | head | normal | pure p50_ms | 5 | 0.0163 | 0.0161 | 0.0166 |
| m2 | head | normal | pure p95_ms | 5 | 0.0440 | 0.0439 | 0.0448 |
| m2 | head | normal | pure p99_ms | 5 | 0.0514 | 0.0509 | 0.0521 |
| m2 | head | normal | pure max_ms | 5 | 0.0987 | 0.0835 | 0.1080 |
| m2 | head | normal | manage p50_ms | 5 | 0.0677 | 0.0673 | 0.0695 |
| m2 | head | normal | manage p95_ms | 5 | 0.1502 | 0.1497 | 0.1519 |
| m2 | head | normal | manage p99_ms | 5 | 0.1900 | 0.1843 | 0.1944 |
| m2 | head | normal | manage max_ms | 5 | 0.3915 | 0.3491 | 1.8269 |
| m2 | head | normal | render p50_ms | 5 | 0.0447 | 0.0430 | 0.0453 |
| m2 | head | normal | render p95_ms | 5 | 0.1888 | 0.1871 | 0.1901 |
| m2 | head | normal | render p99_ms | 5 | 0.2424 | 0.2325 | 0.2604 |
| m2 | head | normal | render max_ms | 5 | 0.4296 | 0.3774 | 0.4658 |
| m2 | head | normal | WM allocated_bytes | 5 | 1373478720.0000 | 1367805736.0000 | 1375306176.0000 |
| m2 | head | normal | WM max_residency_bytes | 5 | 2120016.0000 | 2066592.0000 | 2145536.0000 |
| m2 | head | normal | WM max_stw_pause_ms | 5 | 1.7000 | 1.7000 | 1.9000 |
| m2 | head | normal | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.0000 |
| m3 | head | normal | pure p50_ms | 5 | 0.0202 | 0.0200 | 0.0204 |
| m3 | head | normal | pure p95_ms | 5 | 0.0439 | 0.0425 | 0.0441 |
| m3 | head | normal | pure p99_ms | 5 | 0.0507 | 0.0494 | 0.0523 |
| m3 | head | normal | pure max_ms | 5 | 0.1137 | 0.0800 | 0.1514 |
| m3 | head | normal | manage p50_ms | 5 | 0.0846 | 0.0810 | 0.0847 |
| m3 | head | normal | manage p95_ms | 5 | 0.1893 | 0.1773 | 0.1923 |
| m3 | head | normal | manage p99_ms | 5 | 0.2524 | 0.2468 | 0.2757 |
| m3 | head | normal | manage max_ms | 5 | 3.4147 | 2.5026 | 4.2923 |
| m3 | head | normal | render p50_ms | 5 | 0.0510 | 0.0500 | 0.0520 |
| m3 | head | normal | render p95_ms | 5 | 0.1391 | 0.1311 | 0.1397 |
| m3 | head | normal | render p99_ms | 5 | 0.2222 | 0.2098 | 0.2446 |
| m3 | head | normal | render max_ms | 5 | 3.0420 | 0.7213 | 4.9929 |
| m3 | head | normal | WM allocated_bytes | 5 | 3109502896.0000 | 3104739280.0000 | 3116370360.0000 |
| m3 | head | normal | WM max_residency_bytes | 5 | 4984672.0000 | 4359224.0000 | 6553736.0000 |
| m3 | head | normal | WM max_stw_pause_ms | 5 | 4.9000 | 4.6000 | 6.0000 |
| m3 | head | normal | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.0000 |
| m2 | trunk | nonmoving | pure p50_ms | 5 | 0.0165 | 0.0161 | 0.0173 |
| m2 | trunk | nonmoving | pure p95_ms | 5 | 0.0448 | 0.0440 | 0.0466 |
| m2 | trunk | nonmoving | pure p99_ms | 5 | 0.0550 | 0.0524 | 0.0582 |
| m2 | trunk | nonmoving | pure max_ms | 5 | 0.2161 | 0.2058 | 0.4653 |
| m2 | trunk | nonmoving | manage p50_ms | 5 | 0.0673 | 0.0662 | 0.0691 |
| m2 | trunk | nonmoving | manage p95_ms | 5 | 0.1492 | 0.1477 | 0.1516 |
| m2 | trunk | nonmoving | manage p99_ms | 5 | 0.1847 | 0.1798 | 0.1921 |
| m2 | trunk | nonmoving | manage max_ms | 5 | 0.4694 | 0.3961 | 3.0896 |
| m2 | trunk | nonmoving | render p50_ms | 5 | 0.0437 | 0.0420 | 0.0448 |
| m2 | trunk | nonmoving | render p95_ms | 5 | 0.1878 | 0.1842 | 0.1894 |
| m2 | trunk | nonmoving | render p99_ms | 5 | 0.2452 | 0.2308 | 0.2514 |
| m2 | trunk | nonmoving | render max_ms | 5 | 0.3932 | 0.3608 | 0.5910 |
| m2 | trunk | nonmoving | WM allocated_bytes | 5 | 1371206304.0000 | 1370411096.0000 | 1373940096.0000 |
| m2 | trunk | nonmoving | WM max_residency_bytes | 5 | 4610776.0000 | 4559024.0000 | 4656064.0000 |
| m2 | trunk | nonmoving | WM max_stw_pause_ms | 5 | 0.6000 | 0.6000 | 0.7000 |
| m2 | trunk | nonmoving | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.0000 |
| m2 | head | nonmoving | pure p50_ms | 5 | 0.0163 | 0.0162 | 0.0166 |
| m2 | head | nonmoving | pure p95_ms | 5 | 0.0443 | 0.0442 | 0.0451 |
| m2 | head | nonmoving | pure p99_ms | 5 | 0.0525 | 0.0515 | 0.0529 |
| m2 | head | nonmoving | pure max_ms | 5 | 0.1318 | 0.1057 | 0.1425 |
| m2 | head | nonmoving | manage p50_ms | 5 | 0.0672 | 0.0663 | 0.0705 |
| m2 | head | nonmoving | manage p95_ms | 5 | 0.1512 | 0.1481 | 0.1526 |
| m2 | head | nonmoving | manage p99_ms | 5 | 0.1860 | 0.1831 | 0.1918 |
| m2 | head | nonmoving | manage max_ms | 5 | 0.3452 | 0.3301 | 0.4392 |
| m2 | head | nonmoving | render p50_ms | 5 | 0.0434 | 0.0426 | 0.0453 |
| m2 | head | nonmoving | render p95_ms | 5 | 0.1874 | 0.1855 | 0.1883 |
| m2 | head | nonmoving | render p99_ms | 5 | 0.2305 | 0.2204 | 0.2473 |
| m2 | head | nonmoving | render max_ms | 5 | 0.3987 | 0.3663 | 3.1866 |
| m2 | head | nonmoving | WM allocated_bytes | 5 | 1372926816.0000 | 1368398352.0000 | 1374938616.0000 |
| m2 | head | nonmoving | WM max_residency_bytes | 5 | 4490728.0000 | 4434528.0000 | 4780104.0000 |
| m2 | head | nonmoving | WM max_stw_pause_ms | 5 | 0.6000 | 0.6000 | 0.9000 |
| m2 | head | nonmoving | WM nonmoving_sync_max_pause_ms | 5 | 0.0000 | 0.0000 | 0.0000 |
| m3 | head | nonmoving | pure p50_ms | 5 | 0.0200 | 0.0200 | 0.0202 |
| m3 | head | nonmoving | pure p95_ms | 5 | 0.0437 | 0.0433 | 0.0441 |
| m3 | head | nonmoving | pure p99_ms | 5 | 0.0515 | 0.0498 | 0.0539 |
| m3 | head | nonmoving | pure max_ms | 5 | 0.1187 | 0.0833 | 0.1920 |
| m3 | head | nonmoving | manage p50_ms | 5 | 0.0847 | 0.0844 | 0.0851 |
| m3 | head | nonmoving | manage p95_ms | 5 | 0.1930 | 0.1881 | 0.1956 |
| m3 | head | nonmoving | manage p99_ms | 5 | 0.2547 | 0.2386 | 0.2891 |
| m3 | head | nonmoving | manage max_ms | 5 | 0.9514 | 0.6782 | 3.0486 |
| m3 | head | nonmoving | render p50_ms | 5 | 0.0522 | 0.0506 | 0.0533 |
| m3 | head | nonmoving | render p95_ms | 5 | 0.1464 | 0.1333 | 0.1516 |
| m3 | head | nonmoving | render p99_ms | 5 | 0.2209 | 0.2181 | 0.2343 |
| m3 | head | nonmoving | render max_ms | 5 | 0.8375 | 0.7958 | 2.8761 |
| m3 | head | nonmoving | WM allocated_bytes | 5 | 3111257944.0000 | 3108062336.0000 | 3117755272.0000 |
| m3 | head | nonmoving | WM max_residency_bytes | 5 | 8187712.0000 | 7889136.0000 | 8722176.0000 |
| m3 | head | nonmoving | WM max_stw_pause_ms | 5 | 1.7000 | 1.0000 | 4.2000 |
| m3 | head | nonmoving | WM nonmoving_sync_max_pause_ms | 5 | 1.7000 | 0.1000 | 4.2000 |

## Budgets

Budgets are pure p99 < 2 ms, manage and render sequence p99 < 10 ms, and maximum observed WM stop-the-world pause < 10 ms. An m3 trial must also meet each command target that names one.

| Trial | Revision | Workload | Collector | Pure p99 | Manage p99 | Render p99 | Max STW | Endpoint p99 | Pass |
|---|---|---|---|---:|---:|---:|---:|---|---|
| 00-trunk-m2-normal | trunk | m2 | normal | 0.0525 | 0.1819 | 0.2560 | 2.3 | none | pass |
| 01-head-m2-normal | head | m2 | normal | 0.0509 | 0.1900 | 0.2325 | 1.7 | none | pass |
| 02-head-m3-normal | head | m3 | normal | 0.0494 | 0.2518 | 0.2446 | 5.3 | external_wake 2.12, ipc_update 1.28, recompile_completion 46.84, scratchpad_toggle 1.98 | pass |
| 03-trunk-m2-nonmoving | trunk | m2 | nonmoving | 0.0524 | 0.1921 | 0.2452 | 0.6 | none | pass |
| 04-head-m2-nonmoving | head | m2 | nonmoving | 0.0529 | 0.1872 | 0.2385 | 0.6 | none | pass |
| 05-head-m3-nonmoving | head | m3 | nonmoving | 0.0498 | 0.2891 | 0.2336 | 4.2 | external_wake 2.11, ipc_update 1.23, recompile_completion 46.19, scratchpad_toggle 1.99 | pass |
| 06-head-m2-normal | head | m2 | normal | 0.0514 | 0.1944 | 0.2344 | 1.7 | none | pass |
| 07-trunk-m2-normal | trunk | m2 | normal | 0.0509 | 0.1861 | 0.2228 | 2.1 | none | pass |
| 08-head-m3-normal | head | m3 | normal | 0.0507 | 0.2468 | 0.2098 | 6.0 | external_wake 2.10, ipc_update 1.12, recompile_completion 47.13, scratchpad_toggle 1.87 | pass |
| 09-head-m2-nonmoving | head | m2 | nonmoving | 0.0524 | 0.1831 | 0.2473 | 0.6 | none | pass |
| 10-trunk-m2-nonmoving | trunk | m2 | nonmoving | 0.0582 | 0.1847 | 0.2308 | 0.6 | none | pass |
| 11-head-m3-nonmoving | head | m3 | nonmoving | 0.0518 | 0.2386 | 0.2209 | 2.8 | external_wake 2.11, ipc_update 1.02, recompile_completion 46.78, scratchpad_toggle 2.27 | pass |
| 12-trunk-m2-normal | trunk | m2 | normal | 0.0519 | 0.1864 | 0.2214 | 1.7 | none | pass |
| 13-head-m2-normal | head | m2 | normal | 0.0514 | 0.1843 | 0.2446 | 1.7 | none | pass |
| 14-head-m3-normal | head | m3 | normal | 0.0523 | 0.2757 | 0.2238 | 4.7 | external_wake 2.04, ipc_update 1.26, recompile_completion 46.90, scratchpad_toggle 1.99 | pass |
| 15-trunk-m2-nonmoving | trunk | m2 | nonmoving | 0.0550 | 0.1833 | 0.2514 | 0.6 | none | pass |
| 16-head-m2-nonmoving | head | m2 | nonmoving | 0.0515 | 0.1856 | 0.2294 | 0.6 | none | pass |
| 17-head-m3-nonmoving | head | m3 | nonmoving | 0.0515 | 0.2755 | 0.2181 | 1.0 | external_wake 2.02, ipc_update 1.00, recompile_completion 46.14, scratchpad_toggle 2.08 | pass |
| 18-head-m2-normal | head | m2 | normal | 0.0513 | 0.1859 | 0.2604 | 1.9 | none | pass |
| 19-trunk-m2-normal | trunk | m2 | normal | 0.0530 | 0.1900 | 0.2182 | 3.7 | none | pass |
| 20-head-m3-normal | head | m3 | normal | 0.0520 | 0.2562 | 0.2132 | 4.6 | external_wake 2.03, ipc_update 1.11, recompile_completion 46.21, scratchpad_toggle 2.17 | pass |
| 21-head-m2-nonmoving | head | m2 | nonmoving | 0.0527 | 0.1918 | 0.2305 | 0.7 | none | pass |
| 22-trunk-m2-nonmoving | trunk | m2 | nonmoving | 0.0555 | 0.1906 | 0.2353 | 0.6 | none | pass |
| 23-head-m3-nonmoving | head | m3 | nonmoving | 0.0539 | 0.2480 | 0.2343 | 1.7 | external_wake 2.09, ipc_update 1.10, recompile_completion 46.10, scratchpad_toggle 2.00 | pass |
| 24-trunk-m2-normal | trunk | m2 | normal | 0.0529 | 0.1979 | 0.2237 | 2.0 | none | pass |
| 25-head-m2-normal | head | m2 | normal | 0.0521 | 0.1928 | 0.2424 | 1.7 | none | pass |
| 26-head-m3-normal | head | m3 | normal | 0.0506 | 0.2524 | 0.2222 | 4.9 | external_wake 2.36, ipc_update 1.16, recompile_completion 47.78, scratchpad_toggle 2.13 | pass |
| 27-trunk-m2-nonmoving | trunk | m2 | nonmoving | 0.0530 | 0.1798 | 0.2472 | 0.7 | none | pass |
| 28-head-m2-nonmoving | head | m2 | nonmoving | 0.0525 | 0.1860 | 0.2204 | 0.9 | none | pass |
| 29-head-m3-nonmoving | head | m3 | nonmoving | 0.0506 | 0.2547 | 0.2188 | 1.0 | external_wake 2.08, ipc_update 1.13, recompile_completion 46.15, scratchpad_toggle 2.17 | pass |

All 30 trials met every budget.

## Revisions

| Revision | Commit | Compiler | Binary SHA-256 | Linked seki libraries (SHA-256) |
|---|---|---|---|---|
| trunk | `c2b785132058f44242d23def90f7a242ba675da5` | 9.10.3 | `a6a1f4e56a92abd297b482ac1a6abbe1f71ee7de194f9d8c9fe60d0536f2a8aa` | `libHSseki-0.1.0.0-inplace-layer-surface-fixture-ghc9.10.3.so` `f4464b48e0cf70fdcb5bd50b7240548f26a57d6ff6b4adc3980ca6323583e964`<br>`libHSseki-0.1.0.0-inplace-seki-wire-ghc9.10.3.so` `1d8fa21bb5645a48eaefef25a024119d297eb5d3f2aabe9ea0572e0714bbd306`<br>`libHSseki-0.1.0.0-inplace-seki-core-ghc9.10.3.so` `377a4cfc5d9d49f30fc13c75d5e7253c578c63046b94bd57bf8a9d447c1c6eba` |
| head | `a34e564ab44bcba91537938065f6b0ab4c67e378` | 9.10.3 | `eee52dfc47adbd3664bd1038dfc2816b22ead5f2a42f6c18b6234268b96416dd` | `libHSseki-0.1.0.0-inplace-layer-surface-fixture-ghc9.10.3.so` `bc6bee518f9f291cecb6f2a11eac2f82e5d217fe79e40feaa7ad4e1d6c6f7a53`<br>`libHSseki-0.1.0.0-inplace-seki-wire-ghc9.10.3.so` `3dcb4778707fc2946a81e09e33288962aaef48bd9b4289101370177326a41e12`<br>`libHSseki-0.1.0.0-inplace-seki-core-ghc9.10.3.so` `377a4cfc5d9d49f30fc13c75d5e7253c578c63046b94bd57bf8a9d447c1c6eba` |

Runtime sources under src, src-core, src-public are identical across the revisions, so the m2 differences between them are a noise floor. This report claims no speedup or regression.
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
| 02-head-m3-normal | external_wake | 1000 | 1.4218 | 1.8443 | 2.1191 | 6.1299 | < 50 ms |
| 02-head-m3-normal | ipc_update | 1000 | 0.5064 | 0.8285 | 1.2805 | 6.0637 | < 50 ms |
| 02-head-m3-normal | recompile_completion | 499 | 43.5294 | 45.6194 | 46.8424 | 47.5404 | none |
| 02-head-m3-normal | scratchpad_toggle | 1001 | 1.4454 | 1.7472 | 1.9823 | 13.3364 | < 50 ms |
| 05-head-m3-nonmoving | external_wake | 1000 | 1.4417 | 1.8300 | 2.1115 | 3.2248 | < 50 ms |
| 05-head-m3-nonmoving | ipc_update | 1000 | 0.5029 | 0.8338 | 1.2307 | 2.8164 | < 50 ms |
| 05-head-m3-nonmoving | recompile_completion | 499 | 43.4810 | 45.1738 | 46.1910 | 58.9904 | none |
| 05-head-m3-nonmoving | scratchpad_toggle | 1001 | 1.4421 | 1.7260 | 1.9870 | 12.8057 | < 50 ms |
| 08-head-m3-normal | external_wake | 1000 | 1.3675 | 1.7893 | 2.1025 | 7.0369 | < 50 ms |
| 08-head-m3-normal | ipc_update | 1000 | 0.4917 | 0.8510 | 1.1227 | 5.7619 | < 50 ms |
| 08-head-m3-normal | recompile_completion | 499 | 43.3770 | 45.2536 | 47.1308 | 48.7216 | none |
| 08-head-m3-normal | scratchpad_toggle | 1001 | 1.4322 | 1.7497 | 1.8726 | 13.2006 | < 50 ms |
| 11-head-m3-nonmoving | external_wake | 1000 | 1.3943 | 1.8645 | 2.1092 | 9.8514 | < 50 ms |
| 11-head-m3-nonmoving | ipc_update | 1000 | 0.5027 | 0.8446 | 1.0165 | 9.8088 | < 50 ms |
| 11-head-m3-nonmoving | recompile_completion | 499 | 43.5969 | 45.5310 | 46.7777 | 60.3074 | none |
| 11-head-m3-nonmoving | scratchpad_toggle | 1001 | 1.4153 | 1.7492 | 2.2670 | 3.8723 | < 50 ms |
| 14-head-m3-normal | external_wake | 1000 | 1.4118 | 1.8333 | 2.0392 | 5.6961 | < 50 ms |
| 14-head-m3-normal | ipc_update | 1000 | 0.5061 | 0.8261 | 1.2601 | 5.6552 | < 50 ms |
| 14-head-m3-normal | recompile_completion | 499 | 43.6335 | 45.6019 | 46.9009 | 47.4966 | none |
| 14-head-m3-normal | scratchpad_toggle | 1001 | 1.4372 | 1.7280 | 1.9903 | 14.5783 | < 50 ms |
| 17-head-m3-nonmoving | external_wake | 1000 | 1.3558 | 1.8006 | 2.0175 | 4.3994 | < 50 ms |
| 17-head-m3-nonmoving | ipc_update | 1000 | 0.4929 | 0.8303 | 1.0030 | 3.2106 | < 50 ms |
| 17-head-m3-nonmoving | recompile_completion | 499 | 43.5047 | 45.1101 | 46.1359 | 47.5697 | none |
| 17-head-m3-nonmoving | scratchpad_toggle | 1001 | 1.4374 | 1.7419 | 2.0781 | 12.9222 | < 50 ms |
| 20-head-m3-normal | external_wake | 1000 | 1.3545 | 1.7375 | 2.0290 | 4.6214 | < 50 ms |
| 20-head-m3-normal | ipc_update | 1000 | 0.4383 | 0.7880 | 1.1095 | 3.4697 | < 50 ms |
| 20-head-m3-normal | recompile_completion | 499 | 42.2804 | 44.5281 | 46.2069 | 58.5351 | none |
| 20-head-m3-normal | scratchpad_toggle | 1001 | 1.3885 | 1.7465 | 2.1726 | 13.5367 | < 50 ms |
| 23-head-m3-nonmoving | external_wake | 1000 | 1.4093 | 1.8504 | 2.0929 | 4.3889 | < 50 ms |
| 23-head-m3-nonmoving | ipc_update | 1000 | 0.5133 | 0.8348 | 1.1046 | 3.1028 | < 50 ms |
| 23-head-m3-nonmoving | recompile_completion | 499 | 43.4997 | 45.1300 | 46.1000 | 61.2114 | none |
| 23-head-m3-nonmoving | scratchpad_toggle | 1001 | 1.4000 | 1.7537 | 2.0033 | 4.7040 | < 50 ms |
| 26-head-m3-normal | external_wake | 1000 | 1.3901 | 1.8369 | 2.3568 | 13.0641 | < 50 ms |
| 26-head-m3-normal | ipc_update | 1000 | 0.4994 | 0.8257 | 1.1610 | 13.0279 | < 50 ms |
| 26-head-m3-normal | recompile_completion | 499 | 43.5171 | 45.4704 | 47.7785 | 58.4032 | none |
| 26-head-m3-normal | scratchpad_toggle | 1001 | 1.4362 | 1.7780 | 2.1291 | 6.2284 | < 50 ms |
| 29-head-m3-nonmoving | external_wake | 1000 | 1.3747 | 1.8120 | 2.0763 | 4.2856 | < 50 ms |
| 29-head-m3-nonmoving | ipc_update | 1000 | 0.5077 | 0.8250 | 1.1262 | 2.3139 | < 50 ms |
| 29-head-m3-nonmoving | recompile_completion | 499 | 43.6477 | 45.2147 | 46.1455 | 115.8831 | none |
| 29-head-m3-nonmoving | scratchpad_toggle | 1001 | 1.4267 | 1.7461 | 2.1674 | 13.7536 | < 50 ms |

## Work and validity

Sequences per action count the manage and render sequences inside the interval for each measured action. A rise against the same workload points at repeated proposals.

| Trial | Manage | Render | Manage per action | Render per action | Pure | Major GC in workload | Load before / after |
|---|---:|---:|---:|---:|---:|---|---|
| 00-trunk-m2-normal | 2910 | 2910 | 2.84 | 2.84 | 20000 | True | [1.16259765625, 1.1337890625, 1.17724609375] / [1.3974609375, 1.1865234375, 1.19384765625] |
| 01-head-m2-normal | 2908 | 2908 | 2.84 | 2.84 | 20000 | True | [1.3974609375, 1.1865234375, 1.19384765625] / [1.072265625, 1.13134765625, 1.173828125] |
| 02-head-m3-normal | 5463 | 5463 | 2.19 | 2.19 | 20000 | True | [1.072265625, 1.13134765625, 1.173828125] / [0.787109375, 1.05419921875, 1.146484375] |
| 03-trunk-m2-nonmoving | 2908 | 2908 | 2.84 | 2.84 | 20000 | True | [0.787109375, 1.05419921875, 1.146484375] / [0.6171875, 0.9833984375, 1.1181640625] |
| 04-head-m2-nonmoving | 2909 | 2909 | 2.84 | 2.84 | 20000 | True | [0.6171875, 0.9833984375, 1.1181640625] / [0.658203125, 0.95458984375, 1.1005859375] |
| 05-head-m3-nonmoving | 5465 | 5465 | 2.19 | 2.19 | 20000 | True | [0.658203125, 0.95458984375, 1.1005859375] / [0.478515625, 0.87939453125, 1.06982421875] |
| 06-head-m2-normal | 2918 | 2918 | 2.85 | 2.85 | 20000 | True | [0.478515625, 0.87939453125, 1.06982421875] / [0.76318359375, 0.90966796875, 1.072265625] |
| 07-trunk-m2-normal | 2908 | 2908 | 2.84 | 2.84 | 20000 | True | [0.76318359375, 0.90966796875, 1.072265625] / [0.919921875, 0.93603515625, 1.0751953125] |
| 08-head-m3-normal | 5453 | 5453 | 2.18 | 2.18 | 20000 | True | [0.919921875, 0.93603515625, 1.0751953125] / [0.89599609375, 0.927734375, 1.0673828125] |
| 09-head-m2-nonmoving | 2904 | 2904 | 2.84 | 2.84 | 20000 | True | [0.89599609375, 0.927734375, 1.0673828125] / [0.82080078125, 0.904296875, 1.0537109375] |
| 10-trunk-m2-nonmoving | 2913 | 2913 | 2.85 | 2.85 | 20000 | True | [0.82080078125, 0.904296875, 1.0537109375] / [0.7109375, 0.86767578125, 1.03466796875] |
| 11-head-m3-nonmoving | 5460 | 5460 | 2.18 | 2.18 | 20000 | True | [0.7109375, 0.86767578125, 1.03466796875] / [0.8154296875, 0.880859375, 1.0322265625] |
| 12-trunk-m2-normal | 2903 | 2903 | 2.84 | 2.84 | 20000 | True | [0.8154296875, 0.880859375, 1.0322265625] / [0.7236328125, 0.84765625, 1.0126953125] |
| 13-head-m2-normal | 2904 | 2904 | 2.84 | 2.84 | 20000 | True | [0.7236328125, 0.84765625, 1.0126953125] / [0.853515625, 0.86572265625, 1.009765625] |
| 14-head-m3-normal | 5471 | 5471 | 2.19 | 2.19 | 20000 | True | [0.853515625, 0.86572265625, 1.009765625] / [0.6982421875, 0.828125, 0.99267578125] |
| 15-trunk-m2-nonmoving | 2907 | 2907 | 2.84 | 2.84 | 20000 | True | [0.6982421875, 0.828125, 0.99267578125] / [0.65283203125, 0.80029296875, 0.97802734375] |
| 16-head-m2-nonmoving | 2901 | 2901 | 2.84 | 2.84 | 20000 | True | [0.65283203125, 0.80029296875, 0.97802734375] / [0.76806640625, 0.82080078125, 0.97998046875] |
| 17-head-m3-nonmoving | 5461 | 5461 | 2.18 | 2.18 | 20000 | True | [0.76806640625, 0.82080078125, 0.97998046875] / [0.6064453125, 0.77490234375, 0.95947265625] |
| 18-head-m2-normal | 2895 | 2895 | 2.83 | 2.83 | 20000 | True | [0.6064453125, 0.77490234375, 0.95947265625] / [0.94189453125, 0.84521484375, 0.978515625] |
| 19-trunk-m2-normal | 2899 | 2899 | 2.83 | 2.83 | 20000 | True | [0.94189453125, 0.84521484375, 0.978515625] / [0.8203125, 0.83056640625, 0.97021484375] |
| 20-head-m3-normal | 5468 | 5468 | 2.19 | 2.19 | 20000 | True | [0.8203125, 0.83056640625, 0.97021484375] / [0.7607421875, 0.8154296875, 0.96142578125] |
| 21-head-m2-nonmoving | 2908 | 2908 | 2.84 | 2.84 | 20000 | True | [0.7607421875, 0.8154296875, 0.96142578125] / [0.75146484375, 0.80517578125, 0.953125] |
| 22-trunk-m2-nonmoving | 2904 | 2904 | 2.84 | 2.84 | 20000 | True | [0.75146484375, 0.80517578125, 0.953125] / [0.85888671875, 0.8271484375, 0.95556640625] |
| 23-head-m3-nonmoving | 5463 | 5463 | 2.19 | 2.19 | 20000 | True | [0.85888671875, 0.8271484375, 0.95556640625] / [0.78515625, 0.81201171875, 0.94677734375] |
| 24-trunk-m2-normal | 2912 | 2912 | 2.85 | 2.85 | 20000 | True | [0.78515625, 0.81201171875, 0.94677734375] / [0.58544921875, 0.76513671875, 0.92626953125] |
| 25-head-m2-normal | 2913 | 2913 | 2.85 | 2.85 | 20000 | True | [0.58544921875, 0.76513671875, 0.92626953125] / [0.61767578125, 0.75830078125, 0.91748046875] |
| 26-head-m3-normal | 5462 | 5462 | 2.18 | 2.18 | 20000 | True | [0.61767578125, 0.75830078125, 0.91748046875] / [0.54150390625, 0.7314453125, 0.90283203125] |
| 27-trunk-m2-nonmoving | 2902 | 2902 | 2.84 | 2.84 | 20000 | True | [0.54150390625, 0.7314453125, 0.90283203125] / [0.740234375, 0.76171875, 0.90869140625] |
| 28-head-m2-nonmoving | 2915 | 2915 | 2.85 | 2.85 | 20000 | True | [0.740234375, 0.76171875, 0.90869140625] / [0.970703125, 0.81982421875, 0.9228515625] |
| 29-head-m3-nonmoving | 5480 | 5480 | 2.19 | 2.19 | 20000 | True | [0.970703125, 0.81982421875, 0.9228515625] / [0.935546875, 0.82275390625, 0.91943359375] |

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

## Campaign notes

This section is written by hand. `run.py` generated everything above it.

Trunk is main at `c2b7851`. Head is `a34e564`, the PR head when the campaign ran on 2026-11-09. Later commits on the branch add only this directory.

No PR in the M3-07 stack changes `src/`, `src-core/`, or `src-public/`, so trunk and head run the same seki runtime code. The m2 comparison is therefore a noise floor, and this report claims no speedup and no regression. The seki-core libraries of both revisions have the same SHA-256. The seki-wire and layer-surface-fixture libraries differ only in their RUNPATH, which names each revision's worktree. Their `.text` sections have the same SHA-256.

For each collector, the trunk and head medians of m2 pure p99 differ by at most 0.003 ms, manage p99 by at most 0.004 ms, and render p99 by at most 0.020 ms. Every trunk range overlaps the head range for the same metric. Head render p99 is higher under the normal collector and lower under the nonmoving collector. Both revisions produce the m2 pure checksum 18376811445310300599, which is also the checksum in the retained M2 report.

All 30 trials meet every budget. The worst values are pure p99 0.058 ms, manage p99 0.289 ms, render p99 0.260 ms, and a 6.0 ms stop-the-world pause in `08-head-m3-normal`. The worst m2 pause is 3.7 ms in `19-trunk-m2-normal`. The M3-06 baseline exception applied to PR 46 only. This campaign did not need it.

The host has no nix. The campaign used the GHC 9.10.3 bindist on `PATH`, the system river 0.5.0, wtype, and wlr-randr. River ran headless with `-no-xwayland` and `WLR_RENDERER=pixman`. A 5 s host monitor saw no other river process during the campaign, and its 1-minute load ranged from 0.43 to 1.40. The retained M2 report used GHC 9.6.7 on kernel 7.2.9, so this report makes no comparison with it.

The m3 pauses are longer than the m2 pauses because the m3 WM keeps more live data. Its maximum residency is 4.4 to 8.7 MB, against 2.1 to 4.8 MB for m2, and a copying Gen 1 collection takes longer as live data grows. The longest nonmoving m3 pause, 4.2 ms in `05-head-m3-nonmoving`, is a nonmoving sync. The m3 WM also allocates 3.1 GB against 1.4 GB for m2. Its mutator time in `08-head-m3-normal` is 6.2 s, against 1.1 s in `06-head-m2-normal`. That cost was not profiled to source.

The build worker's 10 ms subprocess poll bounds `recompile_completion`. A separate diagnostic run that is not part of this report sampled the WM's children from `/proc` every 0.2 ms. Each build runs three subprocesses. Each one ran for a median 2.5 ms and then waited a median 9.1 ms (8.1 to 9.8 ms) as a zombie until the worker's next poll reaped it. The next spawn followed 1.1 ms later. Those three cycles account for about 38 ms of the 40.5 ms median in that run.

For the IPC commands, the subscriber line arrives a median 0.50 ms after the send. The probe sees the keyboard owner change a median 1.39 ms after a workspace command and 1.43 ms after a toggle. The fixture does not time the path between those two outcomes. It is inferred to be river's work from the WM's sequence to the keyboard enter event, plus the 0.1 ms probe poll. Single samples reach 14.6 ms for the IPC commands and 116 ms for one recompile. These were not traced. Every p99 stays at or below 2.4 ms for the IPC commands and 47.8 ms for recompiles.

The full trial directories, the host log, and the limiter analysis stay in the ignored `artifacts/m3/M3-07/perf/` directory of the run host.
