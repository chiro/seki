"""Artifact controls for the bootstrap source receipt."""

import importlib.util
import io
from pathlib import Path
import tarfile
import tempfile
import unittest

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


if __name__ == "__main__":
    unittest.main()
