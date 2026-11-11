"""Check that a running session recompiles against a delivery installed after it started.

Usage: python3 ci/check-delivery-upgrade.py FIRST_DELIVERY SECOND_DELIVERY

The two config-delivery outputs must ship different seki archives. The script
starts a headless river whose init command runs `seki` through a link to the
first delivery, with a fresh home, configuration and state. It then points the
link at the second delivery and runs `seki --recompile`. It passes when the new
profile loads the seki library that Cabal built from the second archive.

CI does not run this script, because it needs two delivery builds and river.
"""
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time


def check(work, first, second):
    archives = {"first": (first / "share/seki/source.tar.gz").resolve(strict=True),
                "second": (second / "share/seki/source.tar.gz").resolve(strict=True)}
    assert archives["first"] != archives["second"], "the deliveries ship the same archive"
    for name in ["home", "config", "state"]:
        (work / name).mkdir()
    (work / "runtime").mkdir(mode=0o700)
    link = work / "delivery"
    link.symlink_to(first)
    env = {"HOME": str(work / "home"), "PATH": f"{link}/bin:/usr/bin:/bin",
           "XDG_CONFIG_HOME": str(work / "config"), "XDG_STATE_HOME": str(work / "state"),
           "XDG_RUNTIME_DIR": str(work / "runtime"), "WLR_BACKENDS": "headless", "WLR_RENDERER": "pixman",
           "WLR_LIBINPUT_NO_DEVICES": "1"}
    init = work / "init"
    init.write_text(f"#!/bin/sh\necho $$ > '{work}/seki.pid'\nexec seki >> '{work}/session.log' 2>&1\n")
    init.chmod(0o755)
    project = work / "config/seki/cabal.project"

    def build_status():
        result = subprocess.run([str(link / "bin/sekictl"), "snapshot"], env=env, capture_output=True, text=True, timeout=30)
        return json.loads(result.stdout)["result"]["snapshot"]["build"] if result.returncode == 0 else None

    def wait(what, probe):
        deadline = time.monotonic() + 1800
        while time.monotonic() < deadline:
            value = probe()
            if value:
                return value
            assert river.poll() is None, f"river exited with {river.returncode} while waiting for {what}"
            time.sleep(1)
        raise AssertionError(f"timeout waiting for {what}:\n{(work / 'session.log').read_text()}")

    def label(archive):
        return next((name for name, path in archives.items() if str(path) == str(archive)), f"neither ({archive})")

    def report(phase):
        pid = int((work / "seki.pid").read_text())
        image = os.readlink(f"/proc/{pid}/exe")
        exported = next(entry.split("=", 1)[1] for entry in Path(f"/proc/{pid}/environ").read_text().split("\0") if entry.startswith("SEKI_CONFIG_ARCHIVE="))
        log = max((work / "state/seki").glob("build*.log"), key=lambda path: path.stat().st_mtime_ns)
        plan = json.loads((work / "config/seki/dist-newstyle/cache/plan.json").read_text())
        units = {p["id"]: p["pkg-src"]["path"] for p in plan["install-plan"] if p["pkg-name"] == "seki"}
        loaded = [line.split()[2] for line in subprocess.run(["ldd", image], capture_output=True, text=True, check=True).stdout.splitlines() if "libHSseki-" in line]
        linked = sorted({label(archive) for unit, archive in units.items() if any(f"/{unit}/" in path for path in loaded)})
        quoted = [part for line in project.read_text().splitlines() if line.startswith("packages:") for part in line.split('"')[1::2]]
        print(f"UPGRADE {phase}")
        print(f"  link           {os.readlink(link)}")
        print(f"  session        pid {pid} runs {image}")
        print(f"  environment    SEKI_CONFIG_ARCHIVE={exported} ({label(exported)})")
        print(f"  build log      {log}: {log.read_text(errors='replace').splitlines()[0]}")
        print(f"  cabal.project  {quoted} ({', '.join(label(path) for path in quoted)})")
        print(f"  plan.json      {sorted(set(units.values()))} ({', '.join(sorted({label(path) for path in units.values()}))})")
        for path in loaded:
            print(f"  ldd            {path}")
        print(f"  profile links  {', '.join(linked) or 'no planned seki unit'}", flush=True)
        return linked, label(exported)

    with (work / "river.log").open("w") as log:
        river = subprocess.Popen(["river", "-no-xwayland", "-c", str(init)], env=env, stdout=log, stderr=subprocess.STDOUT)
    try:
        started = wait("the first session", build_status)
        assert report("session started from the first delivery") == (["first"], "first")
        os.symlink(second, work / "delivery.next")
        os.replace(work / "delivery.next", link)
        subprocess.run([str(link / "bin/seki"), "--recompile"], env=env, check=True, capture_output=True, timeout=30)

        def handed_over():
            build = build_status()
            assert not build or build["state"] != "failed", build
            return build and build["state"] == "idle" and build["active_image"] != started["active_image"]

        wait("the handover after the recompile", handed_over)
        linked, exported = report("after the link moved to the second delivery and seki --recompile")
        assert linked == ["second"], f"the recompile built against the {' and '.join(linked)} delivery"
        assert exported == "second", f"the new image's environment names the {exported} delivery"
    finally:
        river.send_signal(signal.SIGTERM)
        try:
            river.wait(timeout=10)
        except subprocess.TimeoutExpired:
            river.kill()


if __name__ == "__main__":
    deliveries = [Path(argument).resolve(strict=True) for argument in sys.argv[1:3]]
    with tempfile.TemporaryDirectory(prefix="seki-upgrade-") as temporary:
        check(Path(temporary), *deliveries)
