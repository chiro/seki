"""Deterministic failure controls for the raw launcher, not positive runtime proof."""
import os
from pathlib import Path
import subprocess
import sys
import tempfile

launcher = Path(sys.argv[1]).resolve()
templates = Path(sys.argv[2]).resolve()
with tempfile.TemporaryDirectory(prefix="sk-launcher-") as temporary:
    root = Path(temporary)
    env = dict(os.environ, XDG_CONFIG_HOME=str(root / "config"), XDG_STATE_HOME=str(root / "state"),
               SEKI_CONFIG_ARCHIVE=str(root / "archive"), SEKI_CONFIG_GHC="/missing/ghc",
               SEKI_CONFIG_CABAL=str(root / "cabal"), SEKI_CONFIG_ENV="/usr/bin/env",
               SEKI_CONFIG_TEMPLATES=str(templates), SEKI_CONFIG_DEFAULT=str(root / "default"))
    config = root / "config/seki"
    config.mkdir(parents=True)
    (config / "Main.hs").write_text("partial user file")
    (config / "unrelated").write_text("keep")
    (root / "default").write_text('#!/bin/sh\ntouch "' + str(root / "fallback") + '"\n')
    (root / "default").chmod(0o500)
    def run(args, expected=1):
        result = subprocess.run([launcher, *args], env=env, capture_output=True, timeout=15)
        print(args, result.returncode, result.stderr.decode(), flush=True)
        assert result.returncode == expected
        assert (config / "Main.hs").read_text() == "partial user file"
        assert (config / "unrelated").read_text() == "keep"
        return result
    run(["--check"])
    assert not (root / "fallback").exists()
    run([], 0)
    assert (root / "fallback").exists()
    (root / "fallback").unlink()
    # Both output and time refusal must preserve an existing target.
    state = root / "state/seki"
    profile = state / "profile"
    profile.symlink_to("/old/runtime")
    candidate = root / "bad-candidate"
    cabal = root / "cabal"
    cabal.write_text('#!/bin/sh\nshift\ncase "$1" in\nbuild) exit 0;;\nlist-bin) echo "' + str(candidate) + '";;\nesac\n')
    cabal.chmod(0o500)
    for body in ['exit 17', 'exec sleep 30', 'exec yes excessive-output']:
        if candidate.exists():
            candidate.chmod(0o700)
        candidate.write_text("#!/bin/sh\n" + body + "\n")
        candidate.chmod(0o500)
        result = run(["--check"])
        assert profile.readlink() == Path("/old/runtime")
        assert not (root / "fallback").exists()
        if body != 'exit 17':
            assert b"limit exceeded" in result.stderr
    # A pre-existing project file is never rewritten, even when malformed.
    (config / "cabal.project").write_bytes(b"partial\x00project")
    run(["--check"])
    assert (config / "cabal.project").read_bytes() == b"partial\x00project"
print("PASS: missing compiler, fallback-attempt helper, partial preservation, invalid/time/output candidates")
