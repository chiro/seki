"""Bootstrap artifact, driver mode and CI failure-propagation controls."""

import importlib.util
import contextlib
import io
import os
from pathlib import Path
import subprocess
import tarfile
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("bootstrap", Path(__file__).with_name("check-bootstrap.py"))
bootstrap = importlib.util.module_from_spec(spec)
spec.loader.exec_module(bootstrap)


class ArchiveControls(unittest.TestCase):
    def test_source_identity(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "Main.hs").write_bytes(b"import Seki\n")
            archive = root / "package.tar.gz"
            with tarfile.open(archive, "w:gz") as package:
                package.add(root / "Main.hs", arcname="seki/Main.hs")
            self.assertEqual(set(bootstrap.archive_receipt(archive, root)["members"]), {"Main.hs"})
            (root / "Main.hs").write_bytes(b"stale source\n")
            with self.assertRaisesRegex(AssertionError, "source mismatch: Main.hs"):
                bootstrap.archive_receipt(archive, root)

    def test_duplicate_and_traversal(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "Main.hs").write_bytes(b"import Seki\n")
            for names, error in [(["seki/Main.hs"] * 2, "duplicate member"), (["seki/../Main.hs"], None)]:
                archive = root / "package.tar.gz"
                with tarfile.open(archive, "w:gz") as package:
                    for name in names:
                        data = b"import Seki\n"
                        info = tarfile.TarInfo(name)
                        info.size = len(data)
                        package.addfile(info, io.BytesIO(data))
                with self.assertRaisesRegex(AssertionError, error or ".*"):
                    bootstrap.archive_receipt(archive, root)


class DriverControls(unittest.TestCase):
    def run_driver(self, output, extra):
        def command(args, **kwargs):
            return subprocess.CompletedProcess(args, 17 if args[:2] == ["cabal", "sdist"] else 0)

        with patch.object(bootstrap.sys, "argv", ["check-bootstrap.py", str(output), *extra]), \
             patch.object(bootstrap.shutil, "which", side_effect=lambda tool: "/nix/store/control/bin/" + tool), \
             patch.object(bootstrap.subprocess, "run", side_effect=command), \
             contextlib.redirect_stdout(io.StringIO()):
            bootstrap.main()

    def test_source_mode_reaches_archive_command(self):
        with tempfile.TemporaryDirectory() as temporary:
            with self.assertRaises(subprocess.CalledProcessError) as failure:
                self.run_driver(Path(temporary) / "output", [])
            self.assertEqual(failure.exception.returncode, 17)
            self.assertEqual(failure.exception.cmd[:2], ["cabal", "sdist"])

    def test_delivery_mode_reaches_requested_path(self):
        with tempfile.TemporaryDirectory() as temporary:
            missing = Path(temporary) / "missing-delivery"
            with self.assertRaises(FileNotFoundError) as failure:
                self.run_driver(Path(temporary) / "output", ["--delivery", str(missing)])
            self.assertEqual(Path(failure.exception.filename), missing)

    def test_logged_workflow_commands_propagate_failure(self):
        lines = (Path(__file__).resolve().parent.parent / ".github/workflows/ci.yml").read_text().splitlines()
        scripts = []
        for index, line in enumerate(lines):
            if line.strip() != "run: |":
                continue
            indent = len(line) - len(line.lstrip())
            body = []
            for following in lines[index + 1:]:
                if following.strip() and len(following) - len(following.lstrip()) <= indent:
                    break
                body.append(following[indent + 2:])
            script = "\n".join(body)
            if "| tee" in script:
                scripts.append(script)
        self.assertTrue(scripts)
        for script in scripts:
            with self.subTest(script=script), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                (root / "flake.lock").write_text("control")
                (root / "artifacts/installed-delivery").mkdir(parents=True)
                programs = root / "bin"
                programs.mkdir()
                delivery = root / "seki-config-delivery/bin"
                delivery.mkdir(parents=True)
                for program in [programs / "nix", delivery / "seki-config-env"]:
                    program.write_text("#!/bin/sh\nprintf 'controlled failure\\n'\nexit 17\n")
                    program.chmod(0o755)
                env = dict(os.environ, PATH=str(programs) + os.pathsep + os.environ["PATH"], RUNNER_TEMP=str(root))
                result = subprocess.run(["bash", "-e", "-c", script], cwd=root, env=env, capture_output=True, text=True)
                self.assertIn("controlled failure", result.stdout)
                self.assertEqual(result.returncode, 17, result.stderr)


if __name__ == "__main__":
    unittest.main()
