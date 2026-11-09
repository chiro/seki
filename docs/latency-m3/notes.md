# M3-07 campaign notes

These notes are written by hand. `run.py` generates [report.md](report.md),
`summary.json`, and `manifest.json`. `run.py --report-only` rewrites
`report.md` and `summary.json` and leaves this file alone.

## Revisions

Trunk is main at `3c30373`. Head is `82a31cc`, the code head of PR 72 when the
campaign ran on 2026-11-09. Later commits on the branch change only
documentation under `docs/`.

## Noise-floor scope

These statements cover trunk `3c30373` and head `82a31cc` only.

`git diff 3c30373 82a31cc` is empty for `src`, `src-core`, `src-public`,
`src-generated`, and `cbits`, the trees that the seki runtime libraries build
from. It is also empty for `tools/layer-surface-fixture/src` and
`tools-generated/layer-surface-fixture`, the trees of the probe library. The WM
process in these trials is `seki-latency wm`, and its policy setup (`runWM` in
`tools/latency`) changed. The m2 policy it builds is the same at both revisions.
The m2 layout, bindings, and manage hook are unchanged by reading, and an
equivalence smoke run of the old and new drivers gave identical seed-determined
driver and WM outputs. On that basis, and only for these two revisions, the m2
manage and render comparison is treated as a noise floor. The generated report
draws no such conclusion, because its source check sees the driver differ. This
report claims no speedup and no regression.

The latency driver differs between the revisions. This PR changes
`tools/latency`, and PR 68 and this PR both change
`integration/Seki/Integration/Harness.hs`. The pure m2 interval runs the
driver's `Seki.Latency.Pure`, which this PR changed. The change adds the pad
state to the checksum fold and adds the m3 operation codes. Only the output of the pure
m2 code was checked. Both revisions give the m2 pure checksum
18376811445310300599 over 20000 steps, which is also the checksum in the
retained M2 report. Equal checksums show that the output is the same, not that
the cost is the same, so the pure m2 timings are not part of the noise floor.

The seki-core libraries of both revisions have the same SHA-256. The seki-wire
and layer-surface-fixture libraries differ. Their RUNPATH names each revision's
worktree, and the head path is longer. This moves `.plt` by 16 bytes and changes
the displacement of each PLT call in `.text`. Every object file in the two
libraries, 51 for seki-wire and 7 for layer-surface-fixture, has the same
`.text` at both revisions.

## Results

All 30 trials meet every budget. The worst values are pure p99 0.054 ms in
`13-head-m2-normal`, manage p99 0.296 ms in `23-head-m3-nonmoving`, render p99
0.268 ms in `06-head-m2-normal`, and a 6.3 ms stop-the-world pause in
`14-head-m3-normal`. The worst m2 pause is 2.5 ms in `13-head-m2-normal`.

For each collector, the trunk and head medians of m2 manage p99 differ by at
most 0.003 ms, and render p99 by at most 0.005 ms. Every trunk range overlaps
the head range for each manage, render, and WM metric, and for pure p50, p95,
and p99. Pure m2 maximum under the nonmoving collector does not overlap: 0.189
to 0.230 ms at trunk against 0.120 to 0.132 ms at head. The pure m2 code
changed, so that difference is outside the noise floor. Under the normal collector, the median m2
maximum pause is 1.7 ms at trunk (range 1.7 to 1.8 ms) and 2.1 ms at head
(range 1.7 to 2.5 ms). The RTS summary prints pauses at 0.1 ms resolution, and
the maximum covers the whole WM process lifetime, including startup and
shutdown. The WM process also runs the driver's changed `runWM`. This report
does not attribute that difference.

## Limiters

Under the normal collector, the maximum pause of every m3 trial, 4.9 to 6.3 ms,
is a copying Gen 1 collection. An m3 WM runs 12 Gen 1 collections, 9 of them
inside the measured interval, against 4 and 2 for m2. Its mean Gen 1 pause is
2.8 to 3.1 ms, against 1.2 to 1.5 ms for m2. It allocates 3.1 GB, against
1.37 GB for m2. Peak residency does not explain the longest pause.
`14-head-m3-normal` paused 6.3 ms with a peak residency of 3.6 MB, and
`08-head-m3-normal` paused 5.8 ms with 5.8 MB. Under the nonmoving collector,
the m3 maximum is 1.1 to 1.8 ms. The m3 WM mutator time is 5.9 to 6.4 s, against
1.0 to 1.1 s for m2. That cost was not profiled to source.

The 10 m3 trials each time 1000 `external_wake`, 1000 `ipc_update`, 1001
`scratchpad_toggle`, and 499 `recompile_completion` samples. Across the
trials, the median p50 is 0.51 ms for `ipc_update`, 1.42 ms for
`external_wake`, 1.43 ms for `scratchpad_toggle`, and 43.6 ms for
`recompile_completion`. The p99 ranges are 1.02 to 1.31 ms, 2.05 to 2.27 ms,
1.94 to 2.24 ms, and 46.1 to 48.4 ms. Every trial meets the 50 ms p99 target of
the three endpoints that have one.

The subscriber line arrives a median 0.51 ms after the send. The probe sees the
keyboard owner change about 0.9 ms later. The fixture does not time the path
between those two outcomes. It is inferred to be river's work from the WM's
sequence to the keyboard enter event, plus the 0.1 ms probe poll.

In 9 of the 10 m3 trials, the largest `external_wake` sample, 10.7 to 14.0 ms,
is sample 111. That is a workspace view about 3 s into the measured interval.
In `20-head-m3-normal`, the outlier is `scratchpad_toggle` sample 119 at
14.2 ms, at about the same time. The outlier recurs at the same step under both
WM collectors. The driver always runs the default copying collector, and its
allocation follows the seed, so a collection in the driver is a likely cause.
That cause is a guess, because the outlier was not traced.

The build worker's 10 ms subprocess poll, at `src/Seki/Run/ConfigBuild.hs:184`,
bounds `recompile_completion`. Of the campaign's 4990 samples, 97% fall
between 40 and 46 ms. A separate
diagnostic run at `a34e564` sampled the WM's children from `/proc` every
0.2 ms. That run is not part of this report, and the runtime trees of
`a34e564` match this head. Each build runs three subprocesses. Each one ran for
a median 2.5 ms and then waited a median 9.1 ms as a zombie until the worker's
next poll reaped it. Those three cycles account for about 38 ms of the 40.5 ms
median in that run. The largest `recompile_completion` sample in this campaign
is 60.6 ms.

## Host

The host has no nix. The campaign used the GHC 9.10.3 bindist on `PATH`, the
system river 0.5.0, wtype, and wlr-randr. River ran headless with
`-no-xwayland` and `WLR_RENDERER=pixman`. The runner built both revisions
before the first trial. A 5 s host monitor took 196 samples during the trials.
It saw no other river process, no integration run, and no cabal or GHC process
during that time. Its 1-minute load ranged from 0.39 to 1.20, with a median of
0.72. The retained M2 report used GHC 9.6.7 on kernel 7.2.9, so this report
makes no comparison with it. The M3-06 baseline exception applied to PR 46
only. This campaign did not need it.

The full trial directories, the host log, and the limiter analysis stay in the
ignored `artifacts/m3/M3-07/perf/campaign-82a31cc` directory of the run host.
