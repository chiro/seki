"""Exercise the installed launcher from a config-delivery output with fresh state.

Usage: seki-config-env python3 ci/check-product.py DELIVERY
"""
import hashlib
import json
import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile


def prove(command, root, env, archive, provisioned=False):
    root = Path(root).resolve()
    root.mkdir()
    env = dict(env)
    for key in ["DISPLAY", "WAYLAND_DISPLAY", "WAYLAND_SOCKET"]:
        env.pop(key, None)
    env.update(XDG_CONFIG_HOME=str(root / "config space"), XDG_STATE_HOME=str(root / "state"),
               XDG_RUNTIME_DIR=str(root / "runtime"))
    Path(env["XDG_RUNTIME_DIR"]).mkdir(mode=0o700)
    project = Path(env["XDG_CONFIG_HOME"]) / "seki"
    state = Path(env["XDG_STATE_HOME"]) / "seki"
    project.mkdir(parents=True)
    (project / "unrelated").write_text("keep me")
    if env.get("SEKI_PRODUCT_CACHED") == "1":
        # Explicit developer-only cached source acquisition; installed mode forbids repositories.
        (project / "cabal.project.local").write_text("active-repositories: hackage.haskell.org\n")

    def run(args, expected=0, overrides=None):
        result = subprocess.run([str(command), *args], env=dict(env, **(overrides or {})),
                                cwd=root, capture_output=True, text=True, timeout=240)
        print("PRODUCT", args, result.returncode, result.stdout, result.stderr, flush=True)
        assert result.returncode == expected, result
        return result

    runtime = Path(env["XDG_RUNTIME_DIR"])
    with socket.socket(socket.AF_UNIX) as sentinel:
        sentinel.bind(str(runtime / "wayland"))
        sentinel.listen()
        sentinel.settimeout(0.1)
        env["WAYLAND_DISPLAY"] = str(runtime / "wayland")
        def untouched():
            assert not (runtime / "seki/ipc.sock").exists()
            assert not (root / "startup-marker").exists()
            try:
                connection, _ = sentinel.accept()
            except TimeoutError:
                return
            connection.close()
            raise AssertionError("check reached Wayland")

        run(["--check"])
        untouched()
        assert not (state / "profile").exists()
        assert (project / "unrelated").read_text() == "keep me"
        sources = {p.name: p.read_bytes() for p in project.iterdir() if p.is_file()}
        plan = json.loads((project / "dist-newstyle/cache/plan.json").read_text())
        packages = [p for p in plan["install-plan"] if p["pkg-name"] == "seki"]
        assert packages and all(p["pkg-src"] == {"type": "local-tar", "path": str(archive)} for p in packages)
        if provisioned:
            assert all(p["type"] == "pre-existing" for p in plan["install-plan"] if p["pkg-name"] not in ["seki", "seki-user"])
        flags = ["exe:seki-user", "--offline", "--enable-optimization=2", "-j1"]
        # list-bin uses the same provisioned environment as the product.
        bin_command = [env["SEKI_CONFIG_ENV"], env["SEKI_CONFIG_CABAL"]] if "SEKI_CONFIG_ENV" in env else [str(command.parent / "seki-config-env"), "cabal"]
        binary = Path(subprocess.check_output([*bin_command, "--config-file=" + env.get("SEKI_CONFIG_CABAL_CONFIG", str(state / "cabal.config")), "list-bin", *flags], cwd=project, env=env, text=True).strip())
        fingerprint = lambda p: [hashlib.sha256(p.read_bytes()).hexdigest(), p.stat().st_mtime_ns]
        before = fingerprint(binary)
        repeat = run(["--check"])
        assert "Up to date" in "".join(p.read_text() for p in state.glob("build*.log"))
        assert fingerprint(binary) == before
        assert all((project / name).read_bytes() == content for name, content in sources.items())
        assert all(p.read_bytes() == binary.read_bytes() for p in state.glob("candidate*"))
        untouched()
        # A custom public candidate proves that the user's value is validated.
        main = project / "Main.hs"
        main.write_text('{-# LANGUAGE OverloadedStrings #-}\nmodule Main (main) where\nimport Seki\nmain :: IO ()\nmain = seki def{workspaces=["custom", "other"], startupHook=error "hook evaluated", manageHook=error "manage evaluated"}\n')
        run(["--check"])
        untouched()
        main.write_text(main.read_text().replace('["custom", "other"]', '["custom", "custom"]'))
        invalid = main.read_bytes()
        profile = state / "profile"
        old = root / "old-runtime"
        old.write_text("#!/bin/sh\nexit 99\n")
        old.chmod(0o500)
        profile.symlink_to(old)
        result = run(["--check"], 1)
        assert "PolicyTagError" in result.stderr
        assert profile.readlink() == old and main.read_bytes() == invalid
        untouched()
        main.write_text("not Haskell\n")
        run(["--check"], 1)
        assert profile.readlink() == old
        untouched()
        project_file = project / "cabal.project"
        project_bytes = project_file.read_bytes()
        project_file.write_text(project_file.read_text().replace("with-compiler:", "with-compiler: /missing/compiler\n--"))
        run(["--check"], 1)
        assert profile.readlink() == old
        project_file.write_bytes(project_bytes)
        # Restore bypasses broken source and tools; exactly two runtime arguments.
        recorded = root / "recorded"
        old.chmod(0o700)
        old.write_text('#!/bin/sh\nprintf "%s\\n" "$0" "$@" > "' + str(recorded) + '"\n')
        restore = root / "restore sentinel"
        restore.write_bytes(b"do not consume")
        run(["--restore", str(restore)], overrides={"SEKI_CONFIG_ENV": "/missing/compiler"})
        assert recorded.read_text().splitlines() == [str(profile), "--restore", str(restore)]
        assert restore.read_bytes() == b"do not consume"
        run(["--restore", "--check"], 1)
        # An unwritable/invalid state location fails check before runtime.
        blocked = root / "blocked"
        blocked.write_text("file")
        run(["--check"], 1, {"XDG_STATE_HOME": str(blocked)})
        untouched()
        # Real default boundary: failed initial build attempts the shipped runtime.
        # The sentinel observes connection only; this is not a full default WM session.
        with subprocess.Popen([str(command)], env=dict(env, SEKI_CONFIG_ENV="/missing/compiler"),
                              cwd=root, stdout=subprocess.PIPE, stderr=subprocess.PIPE) as process:
            sentinel.settimeout(10)
            connection, _ = sentinel.accept()
            connection.close()
            out, err = process.communicate(timeout=10)
            print("DEFAULT-ATTEMPT", process.returncode, out.decode(), err.decode(), flush=True)
            assert b"attempting shipped default WM" in err
        # Valid startup copies before promotion and preserves profile argv0.
        main.write_text('module Main (main) where\nimport Seki\nmain :: IO ()\nmain = seki def\n')
        with subprocess.Popen([str(command)], env=env, cwd=root, stdout=subprocess.PIPE, stderr=subprocess.PIPE) as process:
            try:
                sentinel.settimeout(240)
                connection, _ = sentinel.accept()
                cmdline = Path(f"/proc/{process.pid}/cmdline").read_bytes().split(b"\0")
                assert cmdline[0] == os.fsencode(profile) and cmdline[1] == b""
                selected = profile.resolve()
                assert selected.parent == state and selected.name.startswith("candidate")
                assert selected.read_bytes() == binary.read_bytes()
                assert not selected.stat().st_mode & 0o222
                connection.close()
            finally:
                process.kill()
                process.communicate(timeout=10)


if __name__ == "__main__":
    delivery = Path(sys.argv[1]).resolve(strict=True)
    with tempfile.TemporaryDirectory(prefix="seki-smoke-") as temporary:
        home = Path(temporary) / "home"
        home.mkdir()
        env = dict(os.environ, HOME=str(home), CABAL_DIR=str(home / ".cabal"), GHC_ENVIRONMENT="-")
        prove(delivery / "bin/seki", Path(temporary) / "product", env, (delivery / "share/seki/source.tar.gz").resolve(strict=True), True)
