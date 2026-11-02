# Prove a configuration build with provisioned dependencies

This experimental CI proof builds eight external Haskell configurations against the
current source distribution. It runs each binary's exclusive `--check` mode and
repeats the unchanged build offline. It does not activate a configuration.

Run from the repository with Nix installed on x86_64 Linux:

```sh
nix develop --no-write-lock-file .#ci --command python3 ci/check-bootstrap.py "$PWD/artifacts/bootstrap"
```

The locked `ci` shell supplies GHC 9.10.3, Cabal, installed Haskell dependencies,
the C compiler, pkg-config, Wayland, and xkbcommon development libraries. The first
machine must fetch the flake inputs and Nix dependency closure, or build missing
store paths. This requires network access and the configured Nix binary-cache
trust and signature verification. Keep that closure available for dynamically
linked candidate executables. Copying one binary does not provide its libraries.

The driver creates a temporary home, Cabal configuration, package cache, and store.
It verifies the cache and store are empty immediately before the external build.
It generates a new sdist and compares every archived file with repository source.
The user project lives outside the checkout, imports only `Seki`, and depends on
that archive. It cannot use repository `dist-newstyle` outputs.

Cabal repositories are disabled with `active-repositories: :none`, and builds use
`--offline --enable-optimization=2`. These are Cabal controls. The job does not deny
network access at the operating system level. Nix provisioning uses the network.
This is an explicitly provisioned dependency environment, not an empty Nix store,
a Hackage bootstrap test, or a proof that every cache is cold.

The `provisioned-public-bootstrap` artifact preserves the archive hash and source
manifest, exact solved plan, compiler and package database details, native library
flags, Nix closure paths, initial compilation log, unchanged warm build receipt,
dynamic library dependencies, and real CLI checks. Invalid policies must compile
and then fail validation. Sentinel controls check that `--check` leaves Wayland,
hooks, IPC, and the restore file untouched. Ordinary runtime must connect to the
sentinel as a positive control.

A passing job establishes this provisioning candidate for the tested source and
closure. The proof remains pending until that job executes successfully. It does
not settle distribution, launcher ownership, asynchronous builds, or promotion.

Cabal documents the repository control in its
[project reference](https://cabal.readthedocs.io/en/stable/cabal-project-description-file.html#cfg-field-active-repositories).
Nix documents environment recording with `--profile` in the
[nix develop reference](https://nix.dev/manual/nix/2.28/command-ref/new-cli/nix3-develop).
