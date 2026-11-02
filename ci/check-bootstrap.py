"""Prove external candidate builds using provisioned dependencies and fresh Cabal state.

Run inside `nix develop --no-write-lock-file .#ci`.
Receipts go to the supplied directory. Temporary projects stay outside the checkout.
"""

import hashlib
import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile
import tempfile


def archive_receipt(archive, source):
    members = {}
    with tarfile.open(archive) as package:
        for member in package.getmembers():
            if member.isdir():
                continue
            assert member.isfile(), f"unexpected archive member: {member.name}"
            relative = Path(*Path(member.name).parts[1:])
            assert relative.parts and ".." not in relative.parts and not relative.is_absolute()
            data = package.extractfile(member).read()
            assert data == (source / relative).read_bytes(), f"source mismatch: {relative}"
            assert str(relative) not in members, f"duplicate member: {relative}"
            members[str(relative)] = hashlib.sha256(data).hexdigest()
    assert members, "empty archive"
    return {"sha256": hashlib.sha256(archive.read_bytes()).hexdigest(), "members": members}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    parser.add_argument("--delivery", type=Path)
    args = parser.parse_args()
    source = Path(__file__).resolve().parent.parent
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="seki-bootstrap-") as temporary:
        root = Path(temporary)
        assert not root.is_relative_to(source)
        env = dict(os.environ)
        for key in ["HOME", "CABAL_DIR", "CABAL_CONFIG", "XDG_CACHE_HOME", "XDG_CONFIG_HOME"]:
            env[key] = str(root / key.lower())
        for key in ["HOME", "CABAL_DIR", "XDG_CACHE_HOME", "XDG_CONFIG_HOME"]:
            Path(env[key]).mkdir()
        env["GHC_ENVIRONMENT"] = "-"
        cache, store = root / "cache", root / "store"
        cache.mkdir()
        store.mkdir()
        config = Path(env["CABAL_CONFIG"])
        config.write_text(f"remote-repo-cache: {cache}\nstore-dir: {store}\nlogs-dir: {root / 'logs'}\nactive-repositories: :none\n")
        shutil.copyfile(config, output / "cabal.config")

        def run(name, args, cwd=root):
            with (output / name).open("w") as log:
                print("COMMAND", args, flush=True)
                result = subprocess.run(args, cwd=cwd, env=env, stdout=log, stderr=subprocess.STDOUT)
            result.check_returncode()
            return (output / name).read_text()

        provenance = {"source-head": run("head.txt", ["git", "rev-parse", "HEAD"], source).strip(),
                      "environment": {k: env.get(k) for k in ["PATH", "GHC_PACKAGE_PATH", "NIX_GHC", "NIX_GHCPKG", "NIX_CFLAGS_COMPILE", "NIX_LDFLAGS", "HOME", "CABAL_CONFIG", "CABAL_DIR", "GHC_ENVIRONMENT"]}}
        for name, command_args in {
            "ghc": ["ghc", "--info"], "cabal": ["cabal", "--version"],
            "package-dbs": ["ghc-pkg", "list"], "packages": ["ghc-pkg", "dump"],
            "pkg-config": ["pkg-config", "--modversion", "wayland-client", "xkbcommon"],
            "c-flags": ["pkg-config", "--cflags", "--libs", "wayland-client", "xkbcommon"],
            "cc": ["cc", "--version"],
        }.items():
            run(name + ".txt", command_args)
        run("native-prefixes.txt", ["pkg-config", "--variable=prefix", "wayland-client", "xkbcommon"])
        for tool in ["ghc", "ghc-pkg", "cabal", "pkg-config", "cc"]:
            provenance[tool] = str(Path(shutil.which(tool)).resolve())
        assert all(provenance[tool].startswith("/nix/store/") for tool in ["ghc", "ghc-pkg", "cabal", "pkg-config", "cc"]), "bootstrap requires the provisioned Nix toolchain"
        (output / "provenance.json").write_text(json.dumps(provenance, indent=2) + "\n")
        if args.delivery:
            delivery = args.delivery.resolve(strict=True)
            assert str(delivery).startswith("/nix/store/"), "delivery must be installed in Nix store"
            archive = (delivery / "share/seki/source.tar.gz").resolve(strict=True)
            default = (delivery / "bin/seki-default").resolve(strict=True)
            assert str(archive).startswith("/nix/store/")
            assert str(default).startswith("/nix/store/")
            assert not default.is_relative_to(source)
            (output / "delivery.json").write_text(json.dumps({
                "delivery": str(delivery), "archive": str(archive), "default": str(default),
                "default-sha256": hashlib.sha256(default.read_bytes()).hexdigest(),
            }, indent=2) + "\n")
            with (output / "default-check.log").open("w") as log:
                subprocess.run([default, "--check"], cwd=root, env=env, stdout=log,
                               stderr=subprocess.STDOUT, check=True, timeout=10)
            run("default-ldd.txt", ["ldd", str(default)])
        else:
            run("sdist.log", ["cabal", "sdist", "--output-directory=" + str(root / "sdist")], source)
            archive, = (root / "sdist").glob("*.tar.gz")
        receipt = archive_receipt(archive, source)
        (output / "archive.json").write_text(json.dumps(receipt, indent=2) + "\n")
        shutil.copyfile(archive, output / archive.name)
        empty = {str(p): list(p.iterdir()) for p in [cache, store, Path(env["CABAL_DIR"])]}
        assert not any(empty.values()), "Cabal state populated before external build"
        (output / "empty-before.json").write_text(json.dumps(empty, indent=2) + "\n")
        project = root / "user"
        checker = root / "check-config.py"
        with tarfile.open(archive) as package:
            member, = [m for m in package.getmembers() if m.name.endswith("/test-public/check-config.py")]
            checker.write_bytes(package.extractfile(member).read())
        try:
            run("public-check.log", [sys.executable, str(checker), str(archive), str(project), "--provisioned"])
        finally:
            for name in ["cabal.project", "check-candidates.cabal", "build-receipt.json", "dist-newstyle/cache/plan.json"]:
                path = project / name
                if path.exists():
                    shutil.copyfile(path, output / Path(name).name)
            for path in project.glob("*.hs"):
                shutil.copyfile(path, output / path.name)
        assert not list(cache.rglob("*")), "disabled Cabal repositories populated cache"
        binaries = json.loads((project / "build-receipt.json").read_text())
        for path in binaries:
            if "/build/" in path:
                run(Path(path).name + "-ldd.txt", ["ldd", path])
        (output / "PASS").write_text("Provisioned external build and unchanged offline repeat; eight CLI candidates and sentinel controls passed.\n")


if __name__ == "__main__":
    main()
