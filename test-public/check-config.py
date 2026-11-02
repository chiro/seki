"""Build public-only candidates from an sdist, then exercise their real CLI.

Usage: python3 test-public/check-config.py ARCHIVE NEW_EXTERNAL_DIRECTORY
The caller selects GHC through PATH. Dependency resolution is offline.
"""

import os
from pathlib import Path
import socket
import subprocess
import sys
import tarfile
import tempfile

archive = Path(sys.argv[1]).resolve()
project = Path(sys.argv[2]).resolve()
project.mkdir()
with tarfile.open(archive) as package:
    package.extractall(project / "package", filter="data")
(project / "cabal.project").write_text(f"""packages: . {archive}
index-state: 2026-08-08T10:30:36Z
with-compiler: ghc-9.10.3
write-ghc-environment-files: never
shared: true
library-vanilla: false
executable-dynamic: true
package *
  shared: true
  library-vanilla: false
""")
candidates = {
    "valid": ('def{workspaces=["actual", "candidate"]}', None),
    "duplicate": ('def{workspaces=["actual", "actual"]}', "PolicyTagError"),
    "bindings": ('def{keys=const [(KeyBinding Mod4 (Keysym 1), pure ()), (KeyBinding Mod4 (Keysym 1), pure ())]}', "PolicyBindingConflict"),
    "border": ('def{borders=(borders def){borderWidth= -1}}', "PolicyInvalidSceneStyle"),
    "input": ('def{inputRules=[InputRule (either (error . show) id (compileDevicePattern "*")) Nothing [AccelSpeed (0/0)]]}', "PolicyInvalidInputSetting"),
    "scratchpad": ('def{scratchpads=[Scratchpad "" "true" (pure True) (either (error . show) OutputRect (mkSize 10 10 >>= mkRect (Point 0 0)))]}', "PolicyInvalidScratchpads"),
    "poison": ('def{startupHook=error "startup evaluated", manageHook=error "manage evaluated"}', None),
    "spawn": ('def{startupHook=spawn "touch startup-marker", manageHook=pure (error "manage action evaluated")}', None),
}
cabal = "cabal-version: 3.0\nname: check-candidates\nversion: 0.0.0\nbuild-type: Simple\n"
for name, (expression, _) in candidates.items():
    (project / f"{name}.hs").write_text(f'''{{-# LANGUAGE OverloadedStrings #-}}
module Main (main) where
import Seki
main :: IO ()
main = seki ({expression})
''')
    cabal += f"""executable {name}
  main-is: {name}.hs
  default-language: GHC2021
  ghc-options: -Werror
  build-depends: base >=4.20 && <4.21, seki ==0.1.0.0
"""
(project / "check-candidates.cabal").write_text(cabal)
flags = ["--offline", "--enable-optimization=2", "-j1"]
def command(args):
    print("COMMAND", args, flush=True)
    return subprocess.run(args, cwd=project, check=True)
command(["cabal", "build", "all", *flags])
command(["cabal", "build", "all", *flags])
binaries = {name: subprocess.check_output(["cabal", "list-bin", f"exe:{name}", *flags], cwd=project, text=True).strip() for name in candidates}
with tempfile.TemporaryDirectory(prefix="sk-check-", dir="/tmp") as root:
    root = Path(root)
    env = {k: v for k, v in os.environ.items() if k not in ["DISPLAY", "WAYLAND_DISPLAY", "WAYLAND_SOCKET", "XDG_RUNTIME_DIR"]}
    env["XDG_RUNTIME_DIR"] = str(root)
    restore = root / "restore"
    restore.write_bytes(b"unchanged restore sentinel\x00\xff")
    original = restore.read_bytes()
    with socket.socket(socket.AF_UNIX) as sentinel:
        sentinel.bind(str(root / "wayland"))
        sentinel.listen()
        sentinel.settimeout(0.05)
        def run(name, args, expected, error=None):
            result = subprocess.run([binaries[name], *args], cwd=root, env=env, capture_output=True, text=True, timeout=5)
            print(name, args, result.returncode, result.stderr, flush=True)
            assert result.returncode == expected, result
            if error:
                assert error in result.stderr, result
            assert restore.read_bytes() == original
            assert not (root / "seki" / "ipc.sock").exists()
            assert not (root / "startup-marker").exists()
            try:
                connection, _ = sentinel.accept()
            except TimeoutError:
                return
            connection.close()
            raise AssertionError("check connected to Wayland")
        for discovery in [None, "missing", str(root / "wayland")]:
            if discovery is None:
                env.pop("WAYLAND_DISPLAY", None)
            else:
                env["WAYLAND_DISPLAY"] = discovery
            for name, (_, error) in candidates.items():
                run(name, ["--check"], 1 if error else 0, error)
        for args in [["--check", "--restore", str(restore)], ["--restore", str(restore), "--check"], ["--check", "--check"], ["--check", "extra"], ["--restore"], ["--recompile"]]:
            run("poison", args, 1, "ConfigUsageError")
        # A real connection is the sentinel's positive control. Malformed input
        # must not reach it, while ordinary runtime must attempt discovery.
        env["WAYLAND_DISPLAY"] = "missing"
        run("valid", [], 1, "ConfigRuntimeError")
        run("duplicate", [], 1, "PolicyTagError")
        env["WAYLAND_DISPLAY"] = str(root / "wayland")
        process = subprocess.Popen([binaries["valid"]], cwd=root, env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        try:
            sentinel.settimeout(5)
            connection, _ = sentinel.accept()
            connection.close()
            process.communicate(timeout=5)
        finally:
            if process.poll() is None:
                process.kill()
                process.communicate()
print("PASS exact public candidates; runtime positive control; no check connection/hooks/IPC/restore effects")
