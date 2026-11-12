#!/usr/bin/env python3
import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import platform
import re
import statistics
import signal
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]

WORKLOADS = {
    "m1": {"text": "eight native tiled windows; focus and view actions", "exercised": [], "live_steps": 1023},
    "m2": {"text": "six native tiled windows and two parent-hinted dialog floats under Tall with Crop; "
                   "focus, view, Shrink/Expand, and client fullscreen actions",
           "exercised": ["clip_resizes", "fullscreen_enters", "fullscreen_exits_by_focus",
                         "fullscreen_exits_by_request", "hidden_fullscreen_views", "float_raises"], "live_steps": 1023,
           "exercised_note": "Every count except float raises changed what the probe observed, so a barrier confirmed it."},
    "m3": {"text": "two native tiles and a scratchpad window adopted by app ID under Tall; IPC workspace views, "
                   "scratchpad toggles, and recompiles on idle river with no physical input",
           "exercised": [], "live_steps": 2500,
           "endpoint_heading": "M3 command endpoints",
           "endpoint_method": "Each sample runs from the driver's send of one IPC command to the outcome named below, on idle river after a {gap_ms:g} ms gap, "
                              "with no physical input. Probe outcomes are polled every {poll_ms:g} ms; subscriber lines are stamped when the driver's reader thread takes them. "
                              "No ratio compares these with the m2 sequences.",
           "endpoints": {
               "external_wake": {"text": "IPC workspace command sent to the probe's keyboard owner change", "target_p99_ms": 50},
               "ipc_update": {"text": "the same command to the subscriber line that publishes the new focused workspace", "target_p99_ms": 50},
               "scratchpad_toggle": {"text": "IPC scratchpad toggle to the probe's keyboard owner change", "target_p99_ms": 50},
               "recompile_completion": {"text": "IPC recompile to the subscriber line that publishes the build generation's result", "target_p99_ms": None},
           },
           "unsupported": {
               "input_result": "river {river} lists no input device for the fixture's wtype virtual keyboard, so no input rule has a device "
                               "to report on; integration/input/backend.so creates devices only when preloaded into river, which "
                               "m3-input-perf measures",
               "recompile_handover": "a validated build makes the WM stop managing and exec its target, which ends the process that "
                                     "owns the trial's samples; the stub candidate fails its check instead, and the m3-daily-build "
                                     "integration lane covers the exec into a copied candidate and the restore",
           }},
}

SCHEMA = 5

PROJECT_FILES = ":(glob)cabal.project*"

SOURCES = {"runtime": {"paths": ["src", "src-core", "src-public", "src-generated", "cbits"],
                       "components": ["library", "library seki-core", "library seki-wire"]},
           "driver": {"paths": ["tools/latency", "integration", "tools/layer-surface-fixture/src", "tools-generated/layer-surface-fixture"],
                      "components": ["executable seki-latency", "library layer-surface-fixture"]}}

METADATA_FIELDS = {"synopsis", "description", "homepage", "bug-reports", "license", "license-file", "author",
                   "maintainer", "category", "tested-with", "extra-source-files", "extra-doc-files"}

SEPARATE_KINDS = {"test-suite", "benchmark", "executable"}

ENABLED_BY = {"test-suite": "tests", "benchmark": "benchmarks"}

COMPILED_KINDS = {"library", "foreign-library", "executable", "test-suite", "benchmark"}

LOCAL_FIELDS = {"type", "main-is", "hs-source-dirs", "exposed-modules", "other-modules", "visibility", "ghc-options",
                "c-sources", "cc-options", "include-dirs", "includes"}

LIST_FIELDS = {"build-depends", "build-tool-depends", "import"}

OWN_PACKAGE = re.compile(r"seki(?![\w-])")

OWN_TOOL = re.compile(r"(?<![\w-])seki:([\w-]+)")

FIELD = re.compile(r"\s*([A-Za-z][\w-]*)\s*:(.*)")

PATH_TOKEN = re.compile(r'"([^"]*)"|([^\s,"]+)')

BUDGETS = {"pure_p99": ("pure", "p99_ms", 2), "manage_p99": ("manage", "p99_ms", 10),
           "render_p99": ("render", "p99_ms", 10), "wm_gc_max": ("wm_rts", "max_stw_pause_ms", 10)}


def read(path):
    return json.loads(Path(path).read_text())


def write(path, value):
    Path(path).write_text(json.dumps(value, indent=2) + "\n")


def require(condition, reason):
    if not condition:
        raise ValueError(reason)


def quantiles(values):
    require(bool(values), "missing latency samples")
    ordered = sorted(values)
    require(all(isinstance(v, int) and v >= 0 for v in values), "invalid sample duration")
    return {label: ordered[max(0, math.ceil(len(ordered) * q) - 1)] / 1e6
            for label, q in [("p50_ms", .5), ("p95_ms", .95), ("p99_ms", .99), ("max_ms", 1)]} | {"count": len(values)}


def rts_summary(path):
    text = Path(path).read_text()
    def number(pattern):
        match = re.search(pattern, text)
        require(match is not None, f"missing RTS field {pattern} in {path}")
        return int(match[1].replace(",", ""))
    pauses, syncs, generations = [], [], {}
    for line in text.splitlines():
        match = re.match(r"\s*Gen\s+(\d+)\s+(\d+)\s+(colls|syncs),", line)
        if match:
            durations = re.findall(r"([\d.]+)s", line)
            require(bool(durations), "unrecognized RTS collection row")
            (pauses if match[3] == "colls" else syncs).append(float(durations[-1]) * 1000)
            if match[3] == "colls":
                generations[match[1]] = int(match[2])
    require(bool(pauses), "no RTS stop-the-world collection rows")
    return {"allocated_bytes": number(r"([\d,]+) bytes allocated in the heap"),
            "max_residency_bytes": number(r"([\d,]+) bytes maximum residency"),
            "colls_max_pause_ms": max(pauses), "nonmoving_sync_max_pause_ms": max(syncs, default=0),
            "max_stw_pause_ms": max(pauses + syncs), "pause_resolution_ms": .1,
            "collections": generations, "scope": "process lifetime including shutdown; no forced GC"}


def reversed_on_odd_rounds(names, index):
    return names[::-1] if index % 2 else names


def trial_plan(rounds, revisions, workloads, supported):
    for workload in workloads:
        require(any(workload in supported[name] for name in revisions), f"no revision supports workload {workload}")
    order = []
    for index in range(rounds):
        for collector in ("normal", "nonmoving"):
            for workload in workloads:
                names = [name for name in revisions if workload in supported[name]]
                order += [(name, workload, collector) for name in reversed_on_odd_rounds(names, index)]
    return order


def workload_support(returncode, output):
    if returncode == 0:
        return None
    match = re.search(r"invalid workload: \S+", output)
    require(match is not None, f"support probe failed for another reason: {output.strip()[:500]}")
    return match[0]


def linked_seki_libraries(ldd_output):
    paths = []
    for line in ldd_output.splitlines():
        match = re.match(r"\s*(libHSseki\S*)\s+=>\s+(.*?)\s*(\(0x[0-9a-f]+\))?$", line)
        if match:
            require(match[2] != "not found", f"{match[1]} not found")
            paths.append(match[2])
    return paths


def summarize_trial(directory, workload):
    directory = Path(directory)
    wm, driver, pure = (read(directory / f"{name}.json") for name in ["wm", "driver", "pure"])
    require(wm["clean_exit"] and driver["valid"] and driver["native_checks"] and pure["valid"], "failed workload")
    require(driver["workload"] == workload and pure["workload"] == workload, "trial ran a different workload")
    exercised = driver["exercised"]
    for name in WORKLOADS[workload]["exercised"]:
        require(exercised.get(name, 0) > 0, f"workload did not exercise {name}")
    require(wm["failed"] == wm["dropped"] == 0, "failed or dropped sequences")
    require(wm["successful"] == len(wm["samples"]), "missing raw WM samples")
    require(driver["observed_actions"] == driver["steps"] == len(driver["action_codes"]), "missing observed actions")
    require(pure["steps"] == len(pure["samples_ns"]) and pure["checksum"] != 0, "missing pure work or checksum")
    planned = WORKLOADS[workload].get("endpoints", {})
    timed = driver.get("endpoints", {})
    require(sorted(timed) == sorted(planned), f"timed endpoints {sorted(timed)} differ from the workload's {sorted(planned)}")
    for name in planned:
        require(len(timed[name]) == driver["endpoint_steps"][name] > 0, f"{name} has {len(timed[name])} samples for {driver['endpoint_steps'][name]} planned steps")
    method = {key: driver[key] for key in ["idle_gap_us", "observation_poll_us"]} if planned else None
    start, end = driver["start_ns"], driver["end_ns"]
    require(start < end, "invalid shared monotonic measurement interval")
    samples = wm["samples"]
    require(all(s["kind"] in ("manage", "render") and s["start_ns"] <= s["end_ns"] for s in samples), "invalid WM sample")
    require(all(a["end_ns"] <= b["start_ns"] for a,b in zip(samples, samples[1:])), "overlapping WM samples")
    require(samples[0]["start_ns"] < start and samples[-1]["end_ns"] > end, "WM clock/measurement interval not enclosed")
    selected = [s for s in samples if start <= s["start_ns"] and s["end_ns"] <= end]
    result = {kind: quantiles([s["end_ns"] - s["start_ns"] for s in selected if s["kind"] == kind]) for kind in ["manage", "render"]}
    require(result["manage"]["count"] >= driver["steps"] and result["render"]["count"] >= driver["steps"], "missing sequence completions for observed actions")
    before, after = (read(directory / f"wm-checkpoint-{i}.json") for i in [0, 1])
    delta = {key: after[key] - before[key] for key in before}
    require(all(v >= 0 for v in delta.values()), "RTS checkpoint counters moved backwards")
    return result | {"exercised": exercised, "endpoints": {name: quantiles(samples) for name, samples in timed.items()}, "endpoint_method": method,
                     "manage_per_action": result["manage"]["count"] / driver["steps"],
                     "render_per_action": result["render"]["count"] / driver["steps"],
                     "wm_workload_rts_delta": delta, "pure": quantiles(pure["samples_ns"]), "pure_checksum": pure["checksum"],
                     "wm_rts": rts_summary(directory / "wm.rts"), "pure_rts": rts_summary(directory / "pure.rts"),
                     "wm_stats_before_shutdown": {k: v for k,v in wm.items() if k != "samples"},
                     "selected_samples": len(selected), "outside_samples": len(samples)-len(selected),
                     "measurement_wall_s": (end-start)/1e9,
                     "major_gc_observed_before_shutdown": wm["major_gcs"] > 0, "major_gc_during_workload": delta["major_gcs"] > 0}


def budget_values(trial, workload):
    values = {name: (trial[group][metric], limit) for name, (group, metric, limit) in BUDGETS.items()}
    for name, spec in WORKLOADS[workload].get("endpoints", {}).items():
        if spec["target_p99_ms"] is not None:
            values[f"{name}_p99"] = (trial["endpoints"][name]["p99_ms"], spec["target_p99_ms"])
    return values


def budget_outcomes(trial, workload):
    return {name: value < limit for name, (value, limit) in budget_values(trial, workload).items()}


def budget_section(trials):
    misses = [f"{t['directory']} {name} = {value:.4f} ms (budget < {limit} ms)"
              for t in trials for name, (value, limit) in budget_values(t, t["workload"]).items() if not t["budgets"][name]]
    lines = ["", "## Budgets", "",
             "Budgets are pure p99 < 2 ms, manage and render sequence p99 < 10 ms, and maximum observed WM stop-the-world pause < 10 ms. "
             "An m3 trial must also meet each command target that names one."]
    lines += ["", "| Trial | Revision | Workload | Collector | Pure p99 | Manage p99 | Render p99 | Max STW | Endpoint p99 | Pass |",
              "|---|---|---|---|---:|---:|---:|---:|---|---|"]
    for t in trials:
        endpoints = ", ".join(f"{name} {v['p99_ms']:.2f}" for name, v in t["endpoints"].items()) or "none"
        lines.append(f"| {t['directory']} | {t['revision']} | {t['workload']} | {t['collector']} | {t['pure']['p99_ms']:.4f} | {t['manage']['p99_ms']:.4f} | "
                     f"{t['render']['p99_ms']:.4f} | {t['wm_rts']['max_stw_pause_ms']:.1f} | {endpoints} | {'pass' if all(t['budgets'].values()) else 'FAIL'} |")
    if misses:
        lines += ["", "Missed:", ""] + [f"- {miss}" for miss in misses]
    else:
        lines += ["", f"All {len(trials)} trials met every budget."]
    return lines


def revision_section(manifest, checksums):
    lines = ["", "## Revisions", "",
             "| Revision | Commit | Compiler | Binary SHA-256 | Linked seki libraries (SHA-256) |", "|---|---|---|---|---|"]
    for name, r in manifest["revisions"].items():
        libraries = "<br>".join(f"`{Path(path).name}` `{digest}`" for path, digest in r["linked_seki_libraries"].items()) or "not recorded"
        lines.append(f"| {name} | `{r['git_sha']}` | {r['compiler']} | `{r['binary_sha256']}` | {libraries} |")
    sources = manifest["sources"]
    lines.append("")
    if sources is None:
        lines.append("One revision ran, so no sources were compared.")
    else:
        runtime, driver = sources["runtime"], sources["driver"]
        lines.append(f"Runtime sources under {compared(runtime)} {verdict(runtime)}.")
        if not driver["identical"]:
            lines.append(f"Latency driver sources under {compared(driver)} {verdict(driver)}. The WM runs the driver's policy setup, and the pure interval "
                         "includes the driver's checksum fold, so this report draws no conclusion from differences between revisions. Matching pure checksums show the same output, not the same cost.")
        elif runtime["identical"]:
            lines.append(f"Latency driver sources under {compared(driver)} are identical across the revisions, "
                         "so manage, render, and pure differences between them are a noise floor.")
        else:
            lines.append(f"Latency driver sources under {compared(driver)} are identical across the revisions.")
        if runtime["identical"]:
            lines.append("This report claims no speedup or regression.")
    for workload, by_revision in checksums.items():
        if len(by_revision) > 1:
            same = len(set(by_revision.values())) == 1
            lines.append(f"{workload} pure checksums {'agree' if same else 'differ'} across revisions.")
    for item in manifest["unsupported"]:
        lines.append(f"{item['revision']} does not run {item['workload']}: {item['reason']}.")
    return lines


def compared(group):
    return f"{listed(group['paths'])}, with the root `cabal.project*` files and the parts of `seki.cabal` that can change the build of {listed(group['components'])},"


def verdict(group):
    if group["identical"]:
        return "are identical across the revisions"
    if group["reason"]:
        return f"count as different across the revisions, because {group['reason']}"
    return "differ across the revisions"


def listed(paths):
    quoted = [f"`{path}`" for path in paths]
    return quoted[0] if len(quoted) == 1 else ", ".join(quoted[:-1]) + (" and " if len(quoted) == 2 else ", and ") + quoted[-1]


def endpoint_section(trials, river):
    lines = []
    for workload in dict.fromkeys(t["workload"] for t in trials if t["endpoints"]):
        spec = WORKLOADS[workload]
        timed = [t for t in trials if t["workload"] == workload]
        methods = {tuple(t["endpoint_method"].items()) for t in timed}
        require(len(methods) == 1, f"{workload} trials disagree on the idle gap and poll interval: {sorted(methods)}")
        method = dict(methods.pop())
        lines += ["", f"## {spec['endpoint_heading']}", "",
                  spec["endpoint_method"].format(gap_ms=method["idle_gap_us"] / 1000, poll_ms=method["observation_poll_us"] / 1000), ""]
        lines += [f"- `{name}`: {endpoint['text']}." for name, endpoint in spec["endpoints"].items()]
        lines += [f"- `{name}` is unsupported: {reason.format(river=river.split()[0])}." for name, reason in spec.get("unsupported", {}).items()]
        lines += ["", "| Trial | Endpoint | Samples | p50 ms | p95 ms | p99 ms | Max ms | Target p99 |", "|---|---|---:|---:|---:|---:|---:|---|"]
        for t in timed:
            for name, v in t["endpoints"].items():
                target = spec["endpoints"][name]["target_p99_ms"]
                lines.append(f"| {t['directory']} | {name} | {v['count']} | {v['p50_ms']:.4f} | {v['p95_ms']:.4f} | {v['p99_ms']:.4f} | {v['max_ms']:.4f} | "
                             f"{f'< {target} ms' if target is not None else 'none'} |")
    return lines


def report(directory):
    directory = Path(directory)
    manifest = read(directory / "manifest.json")
    require(manifest["schema"] == SCHEMA, f"report mode reads schema {SCHEMA} manifests; {directory / 'manifest.json'} is schema {manifest['schema']}")
    trials = []
    for item in manifest["trials"]:
        trial = item | summarize_trial(directory / item["directory"], item["workload"])
        trial["budgets"] = budget_outcomes(trial, item["workload"])
        trials.append(trial)
    checksums = {}
    for t in trials:
        seen = checksums.setdefault(t["workload"], {}).setdefault(t["revision"], t["pure_checksum"])
        require(seen == t["pure_checksum"], f"deterministic checksum differs across {t['workload']} trials at {t['revision']}")
    groups = {}
    for t in trials:
        groups.setdefault((t["workload"], t["revision"], t["collector"]), []).append(t)
    summary = {"manifest": manifest, "workloads": list(checksums), "trials": trials, "pure_checksums": checksums,
               "verdict": "inconclusive; production defaults unchanged"}
    lines = ["# Latency and collector evidence", "", "Collector selection is inconclusive. Production defaults remain unchanged.", "",
             "The diagnostic uses the same optimized threaded build at `-N1`; only `--nonmoving-gc` differs. WM sequence latency is request submission time, not compositor acknowledgment or display latency.", ""]
    per_group = min(len(group) for group in groups.values())
    lines += [f"Trials per workload, revision, and collector: {per_group}. Fewer than five is exploratory evidence only.",
              f"Machine: {manifest['uname']}; {manifest['cpu_model']}, {manifest['cpu_count']} CPUs. River {manifest['river']} with `WLR_RENDERER={manifest['renderer']}`."]
    lines += [f"Workload {w}: {WORKLOADS[w]['text']}; {manifest['live_steps'][w]} acknowledged actions after 24 warmup actions." for w in checksums]
    lines += [f"Seed {manifest['seed']}. Pure mode uses {manifest['pure_windows']} windows, {manifest['pure_steps']} events, and 128 warmup events.", "",
              "| Workload | Revision | Collector | Metric | Trials | Median | Min | Max |", "|---|---|---|---|---:|---:|---:|---:|"]
    for (workload, revision, collector), group in groups.items():
        metrics = [(f"{kind} {metric}", [t[kind][metric] for t in group]) for kind in ["pure","manage","render"] for metric in ["p50_ms","p95_ms","p99_ms","max_ms"]]
        metrics += [(f"WM {metric}", [t["wm_rts"][metric] for t in group]) for metric in ["allocated_bytes","max_residency_bytes","max_stw_pause_ms","nonmoving_sync_max_pause_ms"]]
        for name, values in metrics:
            lines.append(f"| {workload} | {revision} | {collector} | {name} | {len(values)} | {statistics.median(values):.4f} | {min(values):.4f} | {max(values):.4f} |")
    lines += budget_section(trials)
    lines += revision_section(manifest, checksums)
    lines += endpoint_section(trials, manifest["river"])
    lines += ["", "## Work and validity", "",
              "Sequences per action count the manage and render sequences inside the interval for each measured action. A rise against the same workload points at repeated proposals.", "",
              "| Trial | Manage | Render | Manage per action | Render per action | Pure | Major GC in workload | Load before / after |", "|---|---:|---:|---:|---:|---:|---|---|"]
    for t in trials:
        lines.append(f"| {t['directory']} | {t['manage']['count']} | {t['render']['count']} | {t['manage_per_action']:.2f} | {t['render_per_action']:.2f} | {t['pure']['count']} | {t['major_gc_during_workload']} | {t['load_before']} / {t['load_after']} |")
    for workload in checksums:
        names = WORKLOADS[workload]["exercised"]
        if not names:
            continue
        note = WORKLOADS[workload].get("exercised_note")
        lines += ["", f"## Exercised {workload.upper()} features", "",
                  "Counts come from the model's transitions over the measured actions." + (f" {note}" if note else ""), "",
                  "| Trial | " + " | ".join(names) + " |", "|---|" + "---:|" * len(names)]
        for t in trials:
            if t["workload"] == workload:
                lines.append(f"| {t['directory']} | " + " | ".join(str(t["exercised"][name]) for name in names) + " |")
    lines += ["", "## Interpretation", "",
              "Per-trial results and budget outcomes are retained in summary.json. Budgets are engineering budgets, not river guarantees, and no ordinary unit test enforces wall-clock budgets.", "",
              "RTS allocation and residency describe the WM process lifetime, including startup and shutdown. The `-s` per-generation collection maxima have 0.1 ms printed resolution. Nonmoving synchronization maxima are reported separately and included in the observed stop-the-world maximum; concurrent cycle duration is excluded. There is no forced final GC. The earlier RTSStats snapshot reports whether major GC occurred before shutdown. A trial without major GC gives limited collector evidence.", "",
              "The pure interval includes concrete structural checksum folding over every state, layout, tag, stack, floating rectangle, fullscreen window, pointer op, intent, and desired-scene field. Inputs are pre-generated and previous state fully evaluated. Independent invariants and deterministic replay occur outside the interval. Replay, serialization, and validation still contribute to pure-process lifetime RTS totals.", "",
              "Each native action waits until the probe observes the expected keyboard owner and the size every window last committed. Pictures are polled and checked pixel by pixel only outside the measured interval: before it, after it, and after the restoring actions. Final input delivery checks the target window. An idle-phase WM acknowledgment after the last action ensures the last completed render precedes interval end. Signal/checkpoint overhead lies in the broad driver interval, outside sequence timers. Startup, warmup, cleanup, and samples crossing the interval boundary remain in raw wm.json but are excluded from quantiles. Driver/native-client and river costs are excluded from WM RTS statistics.", "",
              "Scheduling, GC, and waiting remain possible limiters. Separate profile mode records a WM eventlog and Linux process CPU snapshots for WM, river, and driver; it is excluded from these trials. These observations do not identify source-level hotspots. No collector winner is inferred from noisy distributions or an unproven limiter.", "",
              "The local compiler may differ from the repository's pinned GHC 9.10.3. Consult manifest.json for the exact compiler, build flags, platform, tools, trial order, and workloads. Reproduction commands and limitations are in docs/latency.md."]
    write(directory / "summary.json", summary)
    (directory / "report.md").write_text("\n".join(lines) + "\n")
    return summary


def process_cpu(directory):
    path = directory / "processes.json"
    if not path.exists():
        return None
    result = {"monotonic_ns": time.monotonic_ns()}
    for entry in read(path):
        role, pid = entry["role"], entry["pid"]
        try:
            fields = Path(f"/proc/{pid}/stat").read_text().rsplit(")", 1)[1].split()
            if int(fields[19]) != entry["start_ticks"]:
                continue
            result[role] = {"pid": pid, "user_ticks": int(fields[11]), "system_ticks": int(fields[12]), "start_ticks": int(fields[19])}
        except FileNotFoundError:
            pass
    return result


def cleanup_groups(directory):
    path = directory / "processes.json"
    if not path.exists():
        return
    entries = read(path)
    for sig in (signal.SIGTERM, signal.SIGKILL):
        for entry in entries:
            if not entry["owned_group"]:
                continue
            try:
                fields = Path(f"/proc/{entry['pid']}/stat").read_text().rsplit(")", 1)[1].split()
                if int(fields[19]) == entry["start_ticks"] and os.getpgid(entry["pid"]) == entry["pid"]:
                    os.killpg(entry["pid"], sig)
            except ProcessLookupError:
                pass
            except FileNotFoundError:
                pass
        if sig == signal.SIGTERM:
            time.sleep(.2)


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def output(command, cwd=None, stderr=None):
    return subprocess.check_output(command, cwd=cwd, stderr=stderr, text=True).strip()


def parse_revisions(items):
    worktrees = {}
    for item in items:
        name, separator, path = item.partition("=")
        require(separator and re.fullmatch(r"[a-z][a-z0-9]*", name) and name not in worktrees, f"invalid revision {item}")
        worktree = Path(path).resolve()
        hidden = set(output(["git", "ls-files", "--others", "--", PROJECT_FILES], cwd=worktree).splitlines())
        hidden |= {"cabal.project.local"} if (worktree / "cabal.project.local").exists() else set()
        require(not hidden, f"revision {name} has {', '.join(str(worktree / path) for path in sorted(hidden))}, which the manifest would not record")
        worktrees[name] = worktree
    return worktrees


def prepare_revision(worktree, binary, compiler, build_flags):
    record = {"worktree": str(worktree) if worktree else None}
    if binary is None:
        subprocess.run(["cabal", "build", "seki-latency", *build_flags], cwd=worktree, check=True)
        binary = Path(output(["cabal", "list-bin", "seki-latency", *build_flags], cwd=worktree))
        record |= {"build_command": ["cabal", "build", "seki-latency", *build_flags], "build_provenance": "built by runner",
                   "compiler": output([compiler, "--numeric-version"])}
    else:
        record |= {"build_command": None, "build_provenance": "provided binary; dependency optimization flags unverified",
                   "compiler": output([str(binary), "info"]) + " (patch version unverified)"}
    libraries = linked_seki_libraries(output(["ldd", str(binary)]))
    if record["build_command"]:
        for library in libraries:
            require(Path(library).resolve().is_relative_to(worktree), f"{binary} loads {library} from outside {worktree}")
    record |= {"git_sha": output(["git", "rev-parse", "HEAD"], cwd=worktree) if worktree else None,
               "dirty_paths": output(["git", "status", "--porcelain", "--untracked-files=no"], cwd=worktree).splitlines() if worktree else [],
               "binary": str(binary), "binary_sha256": sha256(binary), "binary_reported_compiler": output([str(binary), "info"]),
               "linked_seki_libraries": {library: sha256(library) for library in libraries}}
    return record


def probe_support(binary, workload, scratch):
    result = subprocess.run([binary, "pure", str(scratch), "1909", "2", "1", workload, "+RTS", "-N1", "-RTS"],
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, timeout=60)
    return workload_support(result.returncode, result.stdout)


def cabal_stanzas(text):
    stanzas = []
    for line in text.splitlines():
        if not line.strip() or line.lstrip().startswith("--"):
            continue
        if line[0].isspace() and stanzas:
            stanzas[-1].append(line.rstrip())
        else:
            stanzas.append([line.rstrip()])
    return stanzas


def cabal_fields(stanza):
    fields, indent = [], None
    for line in stanza:
        depth = len(line) - len(line.lstrip())
        field = FIELD.fullmatch(line)
        if indent is not None and depth > indent:
            fields[-1][1].append(line.strip())
        elif field:
            indent = depth
            fields.append((field[1].lower(), [field[2].strip()]))
        else:
            indent = None
            fields.append((None, [line.strip()]))
    return [(name, " ".join(filter(None, values))) for name, values in fields]


def build_inputs(stanzas, components):
    components = set(components)
    while True:
        compared, solver = split_stanzas(stanzas, components)
        tools = {f"executable {tool}" for stanza in compared for name, value in cabal_fields(stanza)
                 if name == "build-tool-depends" for tool in OWN_TOOL.findall(value)} - components
        if not tools:
            return compared, solver
        components |= tools


def split_stanzas(stanzas, components):
    elsewhere = {component for group in SOURCES.values() for component in group["components"]} - components
    compared, solver = [], set()
    for stanza in stanzas:
        kind, *name = stanza[0].split()
        key = " ".join([kind.lower(), *name])
        field = FIELD.fullmatch(stanza[0])
        if field and field[1].lower() in METADATA_FIELDS or kind.lower() == "source-repository":
            continue
        entries = None if key in components or not (kind.lower() in SEPARATE_KINDS or key in elsewhere) else solver_inputs(stanza)
        if entries is None:
            compared.append(stanza)
        else:
            solver |= {(ENABLED_BY.get(kind.lower(), "always"), *entry) for entry in entries}
    return compared, solver


def solver_inputs(stanza):
    entries = set()
    for name, value in cabal_fields(stanza[1:]):
        if name in (None, "buildable"):
            return None
        if name in LIST_FIELDS:
            entries |= {(name, " ".join(item.split())) for item in value.split(",") if item.strip() and (name == "import" or not OWN_PACKAGE.match(item.strip()))}
        elif name not in LOCAL_FIELDS:
            entries.add((name, value))
    return entries


def runs_setup_script(stanzas):
    for stanza in stanzas:
        field = FIELD.fullmatch(stanza[0])
        if field and field[1].lower() == "build-type" and field[2].strip().lower() != "simple" or stanza[0].split()[0].lower() == "custom-setup":
            return True
    return False


def names_other_packages(project):
    return any(path != "." for block in cabal_stanzas(project) for name, value in cabal_fields(block) if name == "packages" for path in cabal_paths(value))


def cabal_paths(value):
    return [os.path.normpath(quoted or bare) for quoted, bare in PATH_TOKEN.findall(value)]


def source_paths(stanzas, declared):
    named = set()
    for stanza in stanzas:
        fields = cabal_fields(stanza)
        named |= {path for name, value in fields if name and name.endswith(("-dirs", "-sources")) for path in cabal_paths(value)}
        if stanza[0].split()[0].lower() in COMPILED_KINDS and "hs-source-dirs" not in {name for name, _ in fields}:
            named.add(".")
    return sorted(path for path in named if not any(path == p or path.startswith(p + "/") for p in declared))


def compare_sources(revisions):
    named = [r for r in revisions.values() if r["git_sha"]]
    if len(named) < 2:
        return None
    first = named[0]
    git = lambda *args: output(["git", *args], cwd=first["worktree"])
    cabal = {r["git_sha"]: cabal_stanzas(git("show", f"{r['git_sha']}:seki.cabal")) for r in named}
    projects = [(path, git("show", f"{r['git_sha']}:{path}")) for r in named
                for path in git("ls-tree", "--name-only", r["git_sha"]).splitlines() if path.startswith("cabal.project")]
    importing = sorted({path for path, text in projects if re.search(r"^\s*import\s*:", text, re.M | re.I)})
    packaging = sorted({path for path, text in projects if names_other_packages(text)})
    custom = [r["git_sha"][:7] for r in named if runs_setup_script(cabal[r["git_sha"]])]
    found = [(importing, "{} import{s} another project file, which the runner does not compare"),
             (packaging, "{} name{s} another local package, which the runner does not compare"),
             (custom, "`seki.cabal` at {} builds through a setup script, which the runner does not compare")]
    reason = "; ".join(message.format(listed(items), s="s" if len(items) == 1 else "") for items, message in found if items) or None
    sources = {}
    for name, group in SOURCES.items():
        compared = {sha: build_inputs(stanzas, group["components"]) for sha, stanzas in cabal.items()}
        paths = group["paths"] + source_paths([stanza for stanzas, _ in compared.values() for stanza in stanzas], group["paths"])
        identical = reason is None and all(
            compared[r["git_sha"]] == compared[first["git_sha"]]
            and subprocess.run(["git", "diff", "--quiet", first["git_sha"], r["git_sha"], "--", *paths, PROJECT_FILES], cwd=first["worktree"]).returncode == 0
            for r in named[1:])
        sources[name] = {"paths": paths, "components": group["components"], "identical": identical, "reason": reason}
    return sources


def run_live(trial, binary, workload, collector, steps, seed, profile):
    with (trial / "driver.log").open("w") as log:
        command = [str(binary),"live",str(trial),str(seed),str(steps),collector,workload,"+RTS","-N1","-RTS"]
        process = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
        cpu = []
        deadline = time.monotonic() + max(120, steps)
        try:
            while process.poll() is None:
                require(time.monotonic() < deadline, "live trial timed out")
                if profile:
                    snapshot = process_cpu(trial)
                    if snapshot:
                        cpu.append(snapshot)
                time.sleep(.1)
            require(process.returncode == 0, f"live trial failed; see {trial / 'driver.log'}")
        finally:
            if process.poll() is None:
                process.terminate()
                try:
                    process.wait(timeout=30)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait()
            cleanup_groups(trial)
            if profile:
                write(trial / "cpu.json", cpu)


def run_pure(trial, binary, workload, collector, seed, windows, steps):
    with (trial / "pure.rts").open("w") as log:
        flags = ["-N1","-T","-s"] + (["--nonmoving-gc"] if collector == "nonmoving" else [])
        subprocess.run([str(binary),"pure",str(trial/"pure.json"),str(seed),str(windows),str(steps),workload,"+RTS",*flags,"-RTS"], stdout=log, stderr=subprocess.STDOUT, check=True, timeout=120)


def main():
    parser = argparse.ArgumentParser(description="Collect opt-in latency and collector evidence")
    parser.add_argument("directory", type=Path)
    parser.add_argument("--trials", type=int, default=5)
    parser.add_argument("--workload", action="append", choices=sorted(WORKLOADS))
    parser.add_argument("--revision", action="append", default=[], metavar="NAME=WORKTREE",
                        help="build and interleave a clean worktree; repeat for each revision")
    parser.add_argument("--seed", type=int, default=1909)
    parser.add_argument("--live-steps", type=int)
    parser.add_argument("--pure-steps", type=int, default=20000)
    parser.add_argument("--pure-windows", type=int, default=32)
    parser.add_argument("--compiler", default="ghc-9.10.3")
    parser.add_argument("--binary", type=Path)
    parser.add_argument("--report-only", action="store_true")
    parser.add_argument("--profile", action="store_true")
    args = parser.parse_args()
    directory = args.directory.resolve()
    if args.report_only:
        report(directory)
        return
    require(args.workload, "--workload is required to collect trials")
    workloads = list(dict.fromkeys(args.workload))
    require(args.trials > 0 and 0 <= args.seed < 2**64, "invalid trials or seed")
    require(not (args.binary and args.revision), "--binary and --revision are exclusive")
    require(not directory.exists(), "use a fresh artifact directory; previous runs are retained")
    worktrees = parse_revisions(args.revision)
    build_flags = ["-w", args.compiler, "--offline", "--enable-optimization=2", "--builddir=dist-newstyle/latency"]
    if worktrees:
        revisions = {name: prepare_revision(path, None, args.compiler, build_flags) for name, path in worktrees.items()}
        for name, r in revisions.items():
            require(not r["dirty_paths"], f"revision {name} has uncommitted changes: {r['dirty_paths']}")
    else:
        revisions = {"local": prepare_revision(None if args.binary else ROOT, args.binary and args.binary.resolve(), args.compiler, build_flags)}
    directory.mkdir(parents=True)
    supported, unsupported = {}, []
    for name, r in revisions.items():
        supported[name] = set()
        for workload in workloads:
            reason = probe_support(r["binary"], workload, directory / f"support-{name}-{workload}.json")
            if reason is None:
                supported[name].add(workload)
            else:
                unsupported.append({"revision": name, "workload": workload, "reason": reason})
    if args.profile:
        require(len(revisions) == 1 and len(workloads) == 1, "profile mode runs one revision and one workload")
        order = [(next(iter(revisions)), workloads[0], "profile")]
    else:
        order = trial_plan(args.trials, list(revisions), workloads, supported)
    live_steps = {w: args.live_steps or WORKLOADS[w]["live_steps"] for w in workloads}
    manifest = {"schema": SCHEMA, "workloads": workloads, "machine": platform.platform(), "uname": " ".join(os.uname()), "cpu_count": os.cpu_count(),
                "cpu_model": next((l.split(":",1)[1].strip() for l in Path("/proc/cpuinfo").read_text().splitlines() if l.startswith("model name")), "unknown"),
                "river": output(["river", "-version"], stderr=subprocess.STDOUT), "renderer": "pixman",
                "revisions": revisions, "unsupported": unsupported, "sources": compare_sources(revisions),
                "seed": args.seed, "live_steps": live_steps, "pure_steps": args.pure_steps, "pure_windows": args.pure_windows,
                "rts_common": ["-N1","-T","-s"], "rts_difference": "--nonmoving-gc", "profile": args.profile,
                "clock_ticks_per_second": os.sysconf("SC_CLK_TCK"), "trials": []}
    write(directory / "manifest.json", manifest)
    for index, (name, workload, collector) in enumerate(order):
        trial = directory / f"{index:02d}-{name}-{workload}-{collector}"
        trial.mkdir()
        item = {"directory": trial.name, "revision": name, "workload": workload, "collector": collector,
                "load_before": os.getloadavg(), "started_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ",time.gmtime())}
        manifest["trials"].append(item)
        write(directory / "manifest.json", manifest)
        binary = revisions[name]["binary"]
        run_live(trial, binary, workload, collector, live_steps[workload], args.seed, args.profile)
        if not args.profile:
            run_pure(trial, binary, workload, collector, args.seed, args.pure_windows, args.pure_steps)
            summarize_trial(trial, workload)
            print(f"validated {trial.name}", flush=True)
        item["load_after"] = os.getloadavg()
        write(directory / "manifest.json", manifest)
        if args.profile:
            return
    for name, r in revisions.items():
        require(sha256(r["binary"]) == r["binary_sha256"] and all(sha256(path) == digest for path, digest in r["linked_seki_libraries"].items()),
                f"revision {name}'s binary or seki libraries changed during the trials")
    report(directory)


if __name__ == "__main__":
    main()
