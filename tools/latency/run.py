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


def summarize_trial(directory):
    directory = Path(directory)
    wm, driver, pure = (read(directory / f"{name}.json") for name in ["wm", "driver", "pure"])
    require(wm["clean_exit"] and driver["valid"] and driver["native_checks"] and pure["valid"], "failed workload")
    require(wm["failed"] == wm["dropped"] == 0, "failed or dropped sequences")
    require(wm["successful"] == len(wm["samples"]), "missing raw WM samples")
    require(driver["observed_actions"] == driver["steps"] == len(driver["action_codes"]), "missing observed actions")
    require(pure["steps"] == len(pure["samples_ns"]) and pure["checksum"] != 0, "missing pure work or checksum")
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
    return result | {"wm_workload_rts_delta": delta, "pure": quantiles(pure["samples_ns"]), "pure_checksum": pure["checksum"],
                     "wm_rts": rts_summary(directory / "wm.rts"), "pure_rts": rts_summary(directory / "pure.rts"),
                     "wm_stats_before_shutdown": {k: v for k,v in wm.items() if k != "samples"},
                     "selected_samples": len(selected), "outside_samples": len(samples)-len(selected),
                     "measurement_wall_s": (end-start)/1e9,
                     "major_gc_observed_before_shutdown": wm["major_gcs"] > 0, "major_gc_during_workload": delta["major_gcs"] > 0}


def report(directory):
    directory = Path(directory)
    manifest = read(directory / "manifest.json")
    trials = []
    for item in manifest["trials"]:
        trials.append(item | summarize_trial(directory / item["directory"]))
    checksums = {t["pure_checksum"] for t in trials}
    require(len(checksums) == 1, "deterministic checksum differs across trials")
    summary = {"manifest": manifest, "trials": trials, "verdict": "inconclusive; production defaults unchanged"}
    write(directory / "summary.json", summary)
    lines = ["# Latency and collector evidence", "", "Collector selection is inconclusive. Production defaults remain unchanged.", "",
             "The diagnostic uses the same optimized threaded build at `-N1`; only `--nonmoving-gc` differs. WM sequence latency is request submission time, not compositor acknowledgment or display latency.", "",
             f"Trials per collector: {len(trials)//2}. Fewer than five is exploratory evidence only.",
             f"Machine: {manifest['machine']}. Compiler: {manifest['compiler']}. Binary SHA-256: `{manifest['binary_sha256']}`.",
             f"Seed {manifest['seed']}; eight native windows; {manifest['live_steps']} acknowledged focus/view actions after 24 warmup actions. Pure mode uses {manifest['pure_windows']} windows, {manifest['pure_steps']} events, and 128 warmup events.", "",
             "| Collector | Metric | Trials | Median | Min | Max |", "|---|---|---:|---:|---:|---:|"]
    for collector in ["normal", "nonmoving"]:
        group = [t for t in trials if t["collector"] == collector]
        if not group:
            continue
        metrics = [(f"{kind} {metric}", [t[kind][metric] for t in group]) for kind in ["pure","manage","render"] for metric in ["p50_ms","p95_ms","p99_ms","max_ms"]]
        metrics += [(f"WM {metric}", [t["wm_rts"][metric] for t in group]) for metric in ["allocated_bytes","max_residency_bytes","max_stw_pause_ms","nonmoving_sync_max_pause_ms"]]
        for name, values in metrics:
            lines.append(f"| {collector} | {name} | {len(values)} | {statistics.median(values):.4f} | {min(values):.4f} | {max(values):.4f} |")
    lines += ["", "## Work and validity", "", "| Trial | Manage | Render | Pure | Major GC in workload | Load before / after |", "|---|---:|---:|---:|---|---|"]
    for t in trials:
        lines.append(f"| {t['directory']} | {t['manage']['count']} | {t['render']['count']} | {t['pure']['count']} | {t['major_gc_during_workload']} | {t['load_before']} / {t['load_after']} |")
    lines += ["", "## Interpretation", "",
              "Budgets are pure p99 < 2 ms, complete sequence p99 < 10 ms, and maximum observed stop-the-world pause < 10 ms. They are engineering budgets, not river guarantees. Per-trial results and budget outcomes are retained in summary.json; no ordinary unit test enforces wall-clock budgets.", "",
              "RTS allocation and residency describe the WM process lifetime, including startup and shutdown. The `-s` per-generation collection maxima have 0.1 ms printed resolution. Nonmoving synchronization maxima are reported separately and included in the observed stop-the-world maximum; concurrent cycle duration is excluded. There is no forced final GC. The earlier RTSStats snapshot reports whether major GC occurred before shutdown. A trial without major GC gives limited collector evidence.", "",
              "The pure interval includes concrete structural checksum folding over every state, layout, tag, stack, floating rectangle, intent, and desired-scene field. Inputs are pre-generated and previous state fully evaluated. Independent invariants and deterministic replay occur outside the interval. Replay, serialization, and validation still contribute to pure-process lifetime RTS totals.", "",
              "Native actions wait for exact keyboard-owner changes. Initial and restored captures verify client colors and Tall geometry; final input delivery checks the target window. Captures verify pixels and may add render sequences. An idle-phase WM acknowledgment after the final capture ensures the last render completion precedes interval end. Signal/checkpoint overhead lies in the broad driver interval, outside sequence timers. Startup, warmup, cleanup, and samples crossing the interval boundary remain in raw wm.json but are excluded from quantiles. Driver/native-client and river costs are excluded from WM RTS statistics.", "",
              "Scheduling, GC, and waiting remain possible limiters. Separate profile mode records a WM eventlog and Linux process CPU snapshots for WM, river, and driver; it is excluded from these trials. These observations do not identify source-level hotspots. No collector winner is inferred from noisy distributions or an unproven limiter.", "",
              "The local compiler may differ from the repository's pinned GHC 9.6.6. Consult manifest.json for the exact compiler, build flags, platform, tools, trial order, and workload. Reproduction commands and limitations are in docs/latency.md."]
    for t in trials:
        t["budgets"] = {"pure_p99": t["pure"]["p99_ms"] < 2,
                        "manage_p99": t["manage"]["p99_ms"] < 10,
                        "render_p99": t["render"]["p99_ms"] < 10,
                        "wm_gc_max": t["wm_rts"]["max_stw_pause_ms"] < 10}
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


def main():
    parser = argparse.ArgumentParser(description="Collect opt-in latency and collector evidence")
    parser.add_argument("directory", type=Path)
    parser.add_argument("--trials", type=int, default=5)
    parser.add_argument("--seed", type=int, default=1909)
    parser.add_argument("--live-steps", type=int, default=1023)
    parser.add_argument("--pure-steps", type=int, default=20000)
    parser.add_argument("--pure-windows", type=int, default=32)
    parser.add_argument("--compiler", default="ghc-9.6.7")
    parser.add_argument("--binary", type=Path)
    parser.add_argument("--report-only", action="store_true")
    parser.add_argument("--profile", action="store_true")
    args = parser.parse_args()
    directory = args.directory.resolve()
    if args.report_only:
        report(directory)
        return
    require(args.trials > 0 and 0 <= args.seed < 2**64, "invalid trials or seed")
    require(not directory.exists(), "use a fresh artifact directory; previous runs are retained")
    directory.mkdir(parents=True)
    build_flags = ["-w", args.compiler, "--offline", "--enable-optimization=2", "--builddir=dist-newstyle/latency"]
    if args.binary:
        binary = args.binary.resolve()
    else:
        subprocess.run(["cabal", "build", "seki-latency", *build_flags], cwd=ROOT, check=True)
        binary = Path(subprocess.check_output(["cabal", "list-bin", "seki-latency", *build_flags], cwd=ROOT, text=True).strip())
    manifest = {"schema": 1, "machine": platform.platform(), "cpu_count": os.cpu_count(),
                "cpu_model": next((l.split(":",1)[1].strip() for l in Path("/proc/cpuinfo").read_text().splitlines() if l.startswith("model name")), "unknown"),
                "compiler": (subprocess.check_output([str(binary),"info"], text=True).strip() + " (patch version unverified)" if args.binary else subprocess.check_output([args.compiler,"--numeric-version"], text=True).strip()),
                "binary_reported_compiler": subprocess.check_output([str(binary),"info"], text=True).strip(),
                "river": subprocess.check_output(["river","-version"], stderr=subprocess.STDOUT, text=True).strip(),
                "build_command": None if args.binary else ["cabal", "build", "seki-latency", *build_flags],
                "build_provenance": "provided binary; dependency optimization flags unverified" if args.binary else "built by runner", "binary": str(binary), "binary_sha256": hashlib.sha256(binary.read_bytes()).hexdigest(),
                "seed": args.seed, "live_steps": args.live_steps, "pure_steps": args.pure_steps, "pure_windows": args.pure_windows,
                "rts_common": ["-N1","-T","-s"], "rts_difference": "--nonmoving-gc", "profile": args.profile,
                "clock_ticks_per_second": os.sysconf("SC_CLK_TCK"), "trials": []}
    write(directory / "manifest.json", manifest)
    order = ["profile"] if args.profile else [c for _ in range(args.trials) for c in ("normal","nonmoving")]
    for index, collector in enumerate(order):
        trial = directory / f"{index:02d}-{collector}"
        trial.mkdir()
        item = {"directory": trial.name, "collector": collector, "load_before": os.getloadavg(), "started_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ",time.gmtime())}
        manifest["trials"].append(item)
        write(directory / "manifest.json", manifest)
        with (trial / "driver.log").open("w") as output:
            command = [str(binary),"live",str(trial),str(args.seed),str(args.live_steps),collector,"m1","+RTS","-N1","-RTS"]
            process = subprocess.Popen(command, stdout=output, stderr=subprocess.STDOUT, start_new_session=True)
            cpu = []
            deadline = time.monotonic() + max(120, args.live_steps)
            try:
                while process.poll() is None:
                    require(time.monotonic() < deadline, "live trial timed out")
                    if args.profile:
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
                if args.profile:
                    write(trial / "cpu.json", cpu)
        if args.profile:
            item["load_after"] = os.getloadavg()
            write(directory / "manifest.json", manifest)
            return
        with (trial / "pure.rts").open("w") as output:
            flags = ["-N1","-T","-s"] + (["--nonmoving-gc"] if collector == "nonmoving" else [])
            subprocess.run([str(binary),"pure",str(trial/"pure.json"),str(args.seed),str(args.pure_windows),str(args.pure_steps),"+RTS",*flags,"-RTS"], stdout=output, stderr=subprocess.STDOUT, check=True, timeout=120)
        item["load_after"] = os.getloadavg()
        summarize_trial(trial)
        write(directory / "manifest.json", manifest)
        print(f"validated {trial.name}", flush=True)
    report(directory)


if __name__ == "__main__":
    main()
