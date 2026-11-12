import importlib.util
import re
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest import mock

spec = importlib.util.spec_from_file_location("latency_run", Path(__file__).with_name("run.py"))
runner = importlib.util.module_from_spec(spec)
spec.loader.exec_module(runner)

RTS = """1,024 bytes allocated in the heap
256 bytes maximum residency (1 sample(s))
  Gen  0         3 colls,  0 par  0.001s 0.001s 0.0001s 0.0002s
  Gen  1         1 colls,  0 par  0.001s 0.001s 0.0001s 0.0003s
  Gen  1         1 syncs,        0.001s 0.0001s 0.0004s
  Gen  1      concurrent,       9.000s 9.000s 9.0000s 9.0000s
"""


def driver_for(workload):
    driver = dict(valid=True, native_checks=True, observed_actions=1, steps=1, action_codes=[0], start_ns=10, end_ns=20,
                  workload=workload, exercised={name: 1 for name in runner.WORKLOADS[workload]["exercised"]})
    endpoints = runner.WORKLOADS[workload].get("endpoints", {})
    if endpoints:
        driver |= dict(endpoints={name: [1_000_000, 2_000_000] for name in endpoints}, endpoint_steps={name: 2 for name in endpoints},
                       idle_gap_us=1000, observation_poll_us=100)
    return driver


def pure_for(workload, checksum=123):
    return dict(valid=True, steps=1, checksum=checksum, samples_ns=[3], workload=workload)


def revision(sha):
    return dict(git_sha=sha, compiler="ghc 9.10", binary_sha256="b" + sha, linked_seki_libraries={"/lib/" + sha: "l" + sha})


def manifest(trials, revisions, unsupported=(), sources=None, river="0.5.0 +xwayland"):
    workloads = list(dict.fromkeys(w for _, _, w in trials))
    return dict(schema=runner.SCHEMA, workloads=workloads, machine="m", uname="Linux host", cpu_count=12, cpu_model="cpu", river=river, renderer="pixman",
                revisions=revisions, unsupported=list(unsupported), sources=sources,
                seed=1, live_steps={w: 1 for w in workloads}, pure_steps=1, pure_windows=2,
                trials=[dict(directory=d, revision=r, workload=w, collector="normal", load_before=[0.5], load_after=[0.6]) for d, r, w in trials])


class ReportTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.path = Path(self.temp.name)
        self.wm = dict(clean_exit=True, successful=4, failed=0, dropped=0, major_gcs=1,
                       samples=[dict(kind=k, start_ns=s, end_ns=s+1) for k,s in [("manage",1),("manage",11),("render",13),("render",21)]])
        self.use("m2")
        for name in ["wm", "pure"]:
            (self.path/f"{name}.rts").write_text(RTS)
        for n in [0,1]:
            runner.write(self.path/f"wm-checkpoint-{n}.json",dict(major_gcs=n,gcs=n,allocated_bytes=n))

    def use(self, workload):
        self.driver, self.pure = driver_for(workload), pure_for(workload)

    def summarize(self, workload="m2"):
        for name in ["wm","driver","pure"]:
            runner.write(self.path/f"{name}.json",getattr(self,name))
        return runner.summarize_trial(self.path, workload)

    def test_concurrent_phase_is_not_pause(self):
        result = self.summarize()
        self.assertEqual(result["wm_rts"]["max_stw_pause_ms"], .4)
        self.assertEqual(result["wm_rts"]["colls_max_pause_ms"], .3)
        self.assertEqual(result["manage"]["count"], 1)

    def test_bad_work_rejected(self):
        for field,value in [("failed",1),("dropped",1),("successful",5),("clean_exit",False)]:
            with self.subTest(field=field):
                original = self.wm[field]
                self.wm[field] = value
                with self.assertRaises(ValueError):
                    self.summarize()
                self.wm[field] = original

    def test_missing_observations_rejected(self):
        self.driver["observed_actions"] = 0
        with self.assertRaisesRegex(ValueError,"missing observed"):
            self.summarize()

    def test_missing_sequence_kind_rejected(self):
        self.wm["samples"][2]["kind"] = "manage"
        with self.assertRaisesRegex(ValueError,"missing latency"):
            self.summarize()

    def test_missing_checksum_rejected(self):
        self.pure["checksum"] = 0
        with self.assertRaisesRegex(ValueError,"checksum"):
            self.summarize()

    def test_bad_clock_rejected(self):
        self.wm["samples"][1]["end_ns"] = 9
        with self.assertRaisesRegex(ValueError,"invalid WM"):
            self.summarize()

    def test_workload_mismatch_rejected(self):
        with self.assertRaisesRegex(ValueError,"different workload"):
            self.summarize("m1")

    def test_unexercised_feature_rejected(self):
        self.driver["exercised"]["fullscreen_enters"] = 0
        with self.assertRaisesRegex(ValueError,"fullscreen_enters"):
            self.summarize()

    def test_m1_needs_no_exercise(self):
        self.driver["workload"] = self.pure["workload"] = "m1"
        self.driver["exercised"] = {name: 0 for name in self.driver["exercised"]}
        self.assertEqual(self.summarize("m1")["exercised"]["fullscreen_enters"], 0)

    def test_report_refuses_an_older_manifest_schema(self):
        runner.write(self.path / "manifest.json", dict(schema=2, workload="m2", trials=[]))
        with self.assertRaisesRegex(ValueError, "schema 2"):
            runner.report(self.path)

    def test_report_names_a_missed_budget_and_counts_per_action(self):
        self.wm["samples"] = [dict(kind=k, start_ns=s, end_ns=e) for k,s,e in [("manage",1,2),("manage",11,12_000_009),("render",12_000_009,12_000_010),("render",12_000_013,12_000_014)]]
        self.driver["end_ns"] = 12_000_010
        trial = self.path/"00-normal"
        trial.mkdir()
        for name in ["wm","driver","pure"]:
            runner.write(trial/f"{name}.json",getattr(self,name))
        for name in ["wm.rts","pure.rts","wm-checkpoint-0.json","wm-checkpoint-1.json"]:
            (trial/name).write_text((self.path/name).read_text())
        runner.write(self.path/"manifest.json", manifest([("00-normal", "head", "m2")], revisions={"head": revision("abc1234")}))
        summary = runner.report(self.path)
        self.assertEqual(summary["trials"][0]["budgets"]["manage_p99"], False)
        text = (self.path/"report.md").read_text()
        self.assertIn("## Budgets", text)
        self.assertIn("00-normal manage_p99 = 12.0000 ms", text)
        self.assertIn("| 00-normal | 1 | 1 | 1.00 | 1.00 |", text)
        self.assertEqual(runner.read(self.path/"summary.json")["trials"][0]["budgets"], summary["trials"][0]["budgets"])


    def test_m3_endpoints_summarized(self):
        self.use("m3")
        result = self.summarize("m3")
        self.assertEqual(sorted(result["endpoints"]), sorted(runner.WORKLOADS["m3"]["endpoints"]))
        self.assertEqual(result["endpoints"]["ipc_update"]["count"], 2)
        self.assertEqual(result["endpoints"]["ipc_update"]["p99_ms"], 2.0)

    def test_m3_missing_endpoint_rejected(self):
        self.use("m3")
        del self.driver["endpoints"]["scratchpad_toggle"]
        with self.assertRaisesRegex(ValueError, "endpoints"):
            self.summarize("m3")

    def test_m3_short_endpoint_rejected(self):
        self.use("m3")
        self.driver["endpoints"]["recompile_completion"] = [1_000_000]
        with self.assertRaisesRegex(ValueError, "recompile_completion"):
            self.summarize("m3")

    def test_m2_with_endpoints_rejected(self):
        self.driver["endpoints"] = {"ipc_update": [1]}
        with self.assertRaisesRegex(ValueError, "endpoints"):
            self.summarize()

    def test_m3_endpoint_targets_are_budgets_except_recompile(self):
        self.use("m3")
        self.driver["endpoints"]["scratchpad_toggle"] = [1_000_000, 60_000_000]
        self.driver["endpoints"]["recompile_completion"] = [1_000_000, 900_000_000]
        trial = self.summarize("m3")
        outcomes = runner.budget_outcomes(trial, "m3")
        self.assertEqual(outcomes["scratchpad_toggle_p99"], False)
        self.assertEqual(outcomes["external_wake_p99"], True)
        self.assertNotIn("recompile_completion_p99", outcomes)
        self.use("m2")
        self.assertEqual(sorted(runner.budget_outcomes(self.summarize(), "m2")), sorted(runner.BUDGETS))

    def test_trial_plan_interleaves_revisions_and_alternates_who_goes_first(self):
        plan = runner.trial_plan(2, ["trunk", "head"], ["m2"], {"trunk": {"m2"}, "head": {"m2"}})
        self.assertEqual(plan, [("trunk", "m2", "normal"), ("head", "m2", "normal"), ("trunk", "m2", "nonmoving"), ("head", "m2", "nonmoving"),
                                ("head", "m2", "normal"), ("trunk", "m2", "normal"), ("head", "m2", "nonmoving"), ("trunk", "m2", "nonmoving")])

    def test_trial_plan_runs_a_workload_only_where_supported(self):
        plan = runner.trial_plan(1, ["trunk", "head"], ["m2", "m3"], {"trunk": {"m2"}, "head": {"m2", "m3"}})
        self.assertEqual(plan, [("trunk", "m2", "normal"), ("head", "m2", "normal"), ("head", "m3", "normal"),
                                ("trunk", "m2", "nonmoving"), ("head", "m2", "nonmoving"), ("head", "m3", "nonmoving")])
        with self.assertRaisesRegex(ValueError, "m3"):
            runner.trial_plan(1, ["trunk"], ["m3"], {"trunk": {"m2"}})

    def test_support_probe_reads_an_unknown_workload_as_unsupported(self):
        self.assertEqual(runner.workload_support(0, ""), None)
        self.assertEqual(runner.workload_support(1, "seki-latency: invalid workload: m3\nCallStack (from HasCallStack):\n"), "invalid workload: m3")
        with self.assertRaisesRegex(ValueError, "support probe"):
            runner.workload_support(1, "seki-latency: pure final invariants failed")

    def test_linked_seki_libraries_parsed_from_ldd(self):
        ldd = """\tlinux-vdso.so.1 (0x00007ffd)
\tlibHSseki-0.1.0.0-inplace-seki-wire-ghc9.10.3.so => /wt/dist-newstyle/latency/libHSseki-wire.so (0x00007f)
\tlibc.so.6 => /usr/lib/libc.so.6 (0x00007f)
\tlibHSseki-0.1.0.0-inplace-seki-core-ghc9.10.3.so => /wt/dist-newstyle/latency/libHSseki-core.so (0x00007f)
"""
        self.assertEqual(runner.linked_seki_libraries(ldd), ["/wt/dist-newstyle/latency/libHSseki-wire.so", "/wt/dist-newstyle/latency/libHSseki-core.so"])
        with self.assertRaisesRegex(ValueError, "not found"):
            runner.linked_seki_libraries("\tlibHSseki-x.so => not found\n")

    def test_a_revision_with_cabal_project_local_is_refused(self):
        clean, local = self.path / "clean", self.path / "local"
        for worktree in (clean, local):
            subprocess.run(["git", "init", "-q", str(worktree)], check=True)
        (local / "cabal.project.local").write_text("optimization: False\n")
        self.assertEqual(runner.parse_revisions([f"trunk={clean}"]), {"trunk": clean.resolve()})
        with self.assertRaisesRegex(ValueError, "cabal.project.local"):
            runner.parse_revisions([f"trunk={clean}", f"head={local}"])
        with self.assertRaisesRegex(ValueError, "invalid revision"):
            runner.parse_revisions([f"trunk={clean}", f"trunk={clean}"])

    def write_trial(self, name, workload, checksum=123, driver=None):
        trial = self.path / name
        trial.mkdir()
        for item, value in [("wm", self.wm), ("driver", driver or driver_for(workload)), ("pure", pure_for(workload, checksum))]:
            runner.write(trial / f"{item}.json", value)
        for item in ["wm.rts", "pure.rts", "wm-checkpoint-0.json", "wm-checkpoint-1.json"]:
            (trial / item).write_text((self.path / item).read_text())

    def campaign(self, runtime=True, driver=True, head_m2_checksum=123, reason=None):
        self.write_trial("00-trunk-m2-normal", "m2")
        self.write_trial("01-head-m2-normal", "m2", head_m2_checksum)
        self.write_trial("02-head-m3-normal", "m3", 456)
        runner.write(self.path / "manifest.json", manifest(
            [("00-trunk-m2-normal", "trunk", "m2"), ("01-head-m2-normal", "head", "m2"), ("02-head-m3-normal", "head", "m3")],
            revisions={"trunk": revision("c2b7851"), "head": revision("abc1234")},
            unsupported=[dict(revision="trunk", workload="m3", reason="invalid workload: m3")],
            sources={name: group | dict(identical=same, reason=None if same else reason) for (name, group), same in zip(runner.SOURCES.items(), [runtime, driver])}))
        return runner.report(self.path)

    def test_campaign_report_groups_by_revision_and_names_the_noise_floor(self):
        summary = self.campaign()
        text = (self.path / "report.md").read_text()
        self.assertEqual([t["revision"] for t in summary["trials"]], ["trunk", "head", "head"])
        self.assertIn("| m2 | trunk | normal | pure p99_ms | 1 |", text)
        self.assertIn("| m2 | head | normal | pure p99_ms | 1 |", text)
        self.assertIn("noise floor", text)
        self.assertIn("| 01-head-m2-normal | head | m2 | normal |", text)
        self.assertIn("c2b7851", text)

    def test_campaign_report_names_what_it_cannot_measure(self):
        self.campaign()
        text = (self.path / "report.md").read_text()
        self.assertIn("input_result", text)
        self.assertIn("recompile_handover", text)
        self.assertIn("trunk does not run m3: invalid workload: m3", text)

    def test_campaign_report_tabulates_m3_endpoints_against_targets(self):
        self.campaign()
        text = (self.path / "report.md").read_text()
        self.assertIn("| 02-head-m3-normal | external_wake | 2 | 1.0000 | 2.0000 | 2.0000 | 2.0000 | < 50 ms |", text)
        self.assertIn("| 02-head-m3-normal | recompile_completion | 2 | 1.0000 | 2.0000 | 2.0000 | 2.0000 | none |", text)

    def test_campaign_report_without_identical_runtime_claims_no_noise_floor(self):
        self.campaign(runtime=False)
        self.assertNotIn("noise floor", (self.path / "report.md").read_text())

    def test_campaign_report_names_every_compared_path(self):
        self.campaign()
        text = (self.path / "report.md").read_text()
        self.assertIn("Runtime sources under `src`, `src-core`, `src-public`, `src-generated`, and `cbits`, with the root `cabal.project*` files and "
                      "the `common warnings`, `library`, `library seki-core`, and `library seki-wire` stanzas of `seki.cabal`, "
                      "are identical across the revisions.", text)
        self.assertIn("Latency driver sources under `tools/latency`, `integration/Seki/Integration/Harness.hs`, "
                      "`integration/Seki/Integration/Snapshot.hs`, `tools/layer-surface-fixture/src`, "
                      "and `tools-generated/layer-surface-fixture`, with the root `cabal.project*` files and "
                      "the `common warnings`, `executable seki-latency`, and `library layer-surface-fixture` stanzas of `seki.cabal`, "
                      "are identical across the revisions, so manage, render, and pure differences between them are a noise floor.", text)

    def test_a_group_that_counts_as_different_names_the_reason(self):
        self.campaign(runtime=False, driver=False, reason="`cabal.project` imports another project file, which the runner does not compare")
        text = (self.path / "report.md").read_text()
        self.assertIn("stanzas of `seki.cabal`, count as different across the revisions, because `cabal.project` imports another project file, "
                      "which the runner does not compare. The WM runs the driver's policy setup", text)
        self.assertNotIn("noise floor", text)

    def test_driver_group_covers_every_integration_module_the_driver_compiles(self):
        cabal = (Path(__file__).resolve().parents[2] / "seki.cabal").read_text()
        stanzas = ["\n".join(s) for s in runner.cabal_stanzas(cabal) if s[0] == "executable seki-latency"]
        self.assertEqual(len(stanzas), 1, "seki.cabal has one executable seki-latency stanza")
        modules = set(re.findall(r"\bSeki\.Integration(?:\.\w+)+", stanzas[0]))
        self.assertLessEqual({"Seki.Integration.Harness", "Seki.Integration.Snapshot"}, modules)
        for module in modules:
            path = "integration/" + module.replace(".", "/") + ".hs"
            self.assertTrue(any(path == source or path.startswith(source + "/") for source in runner.SOURCES["driver"]["paths"]), path)

    def test_build_stanzas_name_what_builds_the_driver_and_its_libraries(self):
        cabal = (Path(__file__).resolve().parents[2] / "seki.cabal").read_text()
        headers = {block[0] for block in runner.cabal_stanzas(cabal)}
        latency = next("\n".join(block) for block in runner.cabal_stanzas(cabal) if block[0] == "executable seki-latency")
        for group in runner.SOURCES.values():
            self.assertLessEqual(set(group["stanzas"]), headers)
        for library in re.findall(r"\bseki:([\w-]+)", latency):
            self.assertTrue(any(f"library {library}" in group["stanzas"] for group in runner.SOURCES.values()), library)

    def test_a_driver_difference_claims_no_noise_floor(self):
        self.campaign(driver=False)
        text = (self.path / "report.md").read_text()
        self.assertIn("Latency driver sources under `tools/latency`, `integration/Seki/Integration/Harness.hs`, "
                      "`integration/Seki/Integration/Snapshot.hs`, `tools/layer-surface-fixture/src`, "
                      "and `tools-generated/layer-surface-fixture`, with the root `cabal.project*` files and "
                      "the `common warnings`, `executable seki-latency`, and `library layer-surface-fixture` stanzas of `seki.cabal`, "
                      "differ across the revisions.", text)
        self.assertIn("The WM runs the driver's policy setup", text)
        self.assertNotIn("noise floor", text)
        self.assertIn("This report claims no speedup or regression.", text)

    def test_source_comparison_reads_each_group_from_git(self):
        repo = self.path / "repo"
        repo.mkdir()
        git = lambda *args: subprocess.run(["git", "-c", "user.name=t", "-c", "user.email=t@t", *args], cwd=repo, check=True, capture_output=True, text=True).stdout.strip()
        git("init", "-q")
        def commit(path, text=None):
            (repo / path).parent.mkdir(parents=True, exist_ok=True)
            (repo / path).write_text(text or path)
            git("add", "-A")
            git("commit", "-q", "-m", path)
            return git("rev-parse", "HEAD")
        stanzas = {"common warnings": "-Wall", "library seki-core": "-O1", "test-suite seki-test": "-Wall", "executable seki-latency": "-O2"}
        def change(stanza, options):
            stanzas[stanza] = options
            return commit("seki.cabal", "cabal-version: 3.0\nname: seki\n\n" + "\n".join(f"{name}\n  ghc-options: {value}\n" for name, value in stanzas.items()))
        base = change("common warnings", "-Wall")
        wire = commit("cbits/transport.c")
        latency = commit("tools/latency/Seki/Latency/Live.hs")
        fixture = commit("tools/layer-surface-fixture/src/Fixture.hs")
        test_stanza = change("test-suite seki-test", "-Wall -Wextra")
        core_stanza = change("library seki-core", "-O0")
        latency_stanza = change("executable seki-latency", "-O0")
        common_stanza = change("common warnings", "-O0")
        project = commit("cabal.project", "shared: false\n")
        compare = lambda a, b: {name: group["identical"] for name, group in
                                runner.compare_sources({"trunk": dict(git_sha=a, worktree=str(repo)), "head": dict(git_sha=b, worktree=str(repo))}).items()}
        self.assertEqual(compare(base, wire), {"runtime": False, "driver": True})
        self.assertEqual(compare(wire, latency), {"runtime": True, "driver": False})
        self.assertEqual(compare(latency, fixture), {"runtime": True, "driver": False})
        self.assertEqual(compare(fixture, test_stanza), {"runtime": True, "driver": True})
        self.assertEqual(compare(test_stanza, core_stanza), {"runtime": False, "driver": True})
        self.assertEqual(compare(core_stanza, latency_stanza), {"runtime": True, "driver": False})
        self.assertEqual(compare(latency_stanza, common_stanza), {"runtime": False, "driver": False})
        self.assertEqual(compare(common_stanza, project), {"runtime": False, "driver": False})
        self.assertIsNone(runner.compare_sources({"local": dict(git_sha=None, worktree=None)}))

    def test_unsupported_input_results_name_the_river_that_ran(self):
        self.write_trial("00-head-m3-normal", "m3", 456)
        runner.write(self.path / "manifest.json", manifest([("00-head-m3-normal", "head", "m3")], revisions={"head": revision("abc1234")}, river="0.6.1 +xwayland"))
        runner.report(self.path)
        self.assertIn("`input_result` is unsupported: river 0.6.1 lists no input device", (self.path / "report.md").read_text())

    def test_exercised_heading_names_its_workload(self):
        with mock.patch.dict(runner.WORKLOADS["m3"], {"exercised": ["float_raises"]}):
            self.write_trial("00-head-m3-normal", "m3", 456)
            runner.write(self.path / "manifest.json", manifest([("00-head-m3-normal", "head", "m3")], revisions={"head": revision("abc1234")}))
            runner.report(self.path)
        text = (self.path / "report.md").read_text()
        self.assertIn("## Exercised M3 features", text)
        self.assertNotIn("M2", text)
        self.assertNotIn("Every count except float raises", text)

    def test_endpoint_method_comes_from_the_trials(self):
        driver = driver_for("m3") | dict(idle_gap_us=2500, observation_poll_us=200)
        self.write_trial("00-head-m3-normal", "m3", 456, driver)
        self.write_trial("01-head-m3-nonmoving", "m3", 456, driver)
        trials = [("00-head-m3-normal", "head", "m3"), ("01-head-m3-nonmoving", "head", "m3")]
        runner.write(self.path / "manifest.json", manifest(trials, revisions={"head": revision("abc1234")}))
        runner.report(self.path)
        text = (self.path / "report.md").read_text()
        self.assertIn("on idle river after a 2.5 ms gap", text)
        self.assertIn("Probe outcomes are polled every 0.2 ms", text)

    def test_trials_with_different_endpoint_methods_are_refused(self):
        self.write_trial("00-head-m3-normal", "m3", 456)
        self.write_trial("01-head-m3-nonmoving", "m3", 456, driver_for("m3") | dict(idle_gap_us=2000))
        trials = [("00-head-m3-normal", "head", "m3"), ("01-head-m3-nonmoving", "head", "m3")]
        runner.write(self.path / "manifest.json", manifest(trials, revisions={"head": revision("abc1234")}))
        with self.assertRaisesRegex(ValueError, "idle gap and poll interval"):
            runner.report(self.path)

    def test_checksums_must_agree_within_a_workload_and_revision_only(self):
        summary = self.campaign(head_m2_checksum=789)
        self.assertEqual(summary["pure_checksums"], {"m2": {"trunk": 123, "head": 789}, "m3": {"head": 456}})
        self.assertIn("m2 pure checksums differ across revisions", (self.path / "report.md").read_text())


    def test_campaign_report_marks_a_failing_trial(self):
        driver = driver_for("m3")
        driver["endpoints"]["external_wake"] = [1_000_000, 70_000_000]
        self.write_trial("00-head-m3-normal", "m3", 456, driver)
        runner.write(self.path / "manifest.json", manifest([("00-head-m3-normal", "head", "m3")], revisions={"head": revision("abc1234")}))
        runner.report(self.path)
        text = (self.path / "report.md").read_text()
        self.assertIn("One revision ran", text)
        self.assertIn("| FAIL |", text)
        self.assertIn("00-head-m3-normal external_wake_p99 = 70.0000 ms (budget < 50 ms)", text)


CABAL = """cabal-version: 3.0
name: seki
version: 0.1.0.0
build-type: Simple
extra-doc-files: README.md

common warnings
  ghc-options: -Wall

library
  import: warnings
  hs-source-dirs: src-public
  exposed-modules: Seki
  build-depends: base, seki:seki-core, seki:seki-wire

library seki-core
  import: warnings
  visibility: private
  hs-source-dirs: src-core
  exposed-modules: Seki.Core
  ghc-options: -O2
  build-depends: base, text >=2.1

library seki-wire
  import: warnings
  visibility: private
  hs-source-dirs: src
  exposed-modules: Seki.Wire
  build-depends: base, seki:seki-core

library layer-surface-fixture
  import: warnings
  visibility: private
  hs-source-dirs: tools/layer-surface-fixture/src
  exposed-modules: Fixture
  build-depends: base, seki:seki-wire

executable seki-latency
  import: warnings
  hs-source-dirs: tools/latency integration
  main-is: Main.hs
  other-modules: Seki.Integration.Harness
  build-depends: base, text >=2.1, seki:seki-core, seki:layer-surface-fixture

test-suite seki-test
  import: warnings
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: Main.hs
  ghc-options: -threaded
  build-tool-depends: seki:seki-latency
  build-depends: base, seki

source-repository head
  type: git
  location: https://example.invalid/seki.git
"""


def edit(text, old, new):
    assert text.count(old) == 1, old
    return text.replace(old, new)


class SourceComparisonTest(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.repo = Path(temp.name)
        self.git("init", "-q")

    def git(self, *args):
        return subprocess.run(["git", "-c", "user.name=t", "-c", "user.email=t@t", *args], cwd=self.repo, check=True, capture_output=True, text=True).stdout.strip()

    def commit(self, files):
        for path, text in files.items():
            (self.repo / path).parent.mkdir(parents=True, exist_ok=True)
            (self.repo / path).write_text(text)
        self.git("add", "-A")
        self.git("commit", "-q", "--allow-empty", "-m", "snapshot")
        return self.git("rev-parse", "HEAD")

    def sources(self, before, after):
        base = {"seki.cabal": CABAL, "cabal.project": "packages: .\n", "src-core/Seki/Core.hs": "module Seki.Core where\n"}
        trunk, head = self.commit(base | before), self.commit(after)
        return runner.compare_sources({"trunk": dict(git_sha=trunk, worktree=str(self.repo)), "head": dict(git_sha=head, worktree=str(self.repo))})

    def identical(self, before, after):
        return {name: group["identical"] for name, group in self.sources(before, after).items()}

    def test_a_column_zero_comment_hides_no_field_below_it(self):
        commented = edit(CABAL, "  ghc-options: -O2\n", "-- the core is pure\n  ghc-options: -O2\n")
        self.assertEqual(self.identical({"seki.cabal": commented}, {"seki.cabal": edit(commented, "-O2", "-O0")}), {"runtime": False, "driver": True})

    @unittest.expectedFailure
    def test_a_common_stanza_that_a_component_imports_is_compared(self):
        perf = edit(edit(CABAL, "common warnings\n", "common perf\n  ghc-options: -O2\n\ncommon warnings\n"),
                    "library seki-core\n  import: warnings\n", "library seki-core\n  import: warnings, perf\n")
        self.assertEqual(self.identical({"seki.cabal": perf}, {"seki.cabal": edit(perf, "common perf\n  ghc-options: -O2", "common perf\n  ghc-options: -O0")}),
                         {"runtime": False, "driver": False})

    def proto(self):
        return edit(edit(CABAL, "  build-depends: base, seki:seki-core\n", "  build-depends: base, seki:seki-core, seki:seki-proto\n"),
                    "library layer-surface-fixture\n", "library seki-proto\n  import: warnings\n  visibility: private\n  hs-source-dirs: src-proto\n"
                                                       "  exposed-modules: Seki.Proto\n  ghc-options: -O2\n\nlibrary layer-surface-fixture\n")

    @unittest.expectedFailure
    def test_a_library_that_no_group_names_is_compared(self):
        proto = self.proto()
        self.assertEqual(self.identical({"seki.cabal": proto}, {"seki.cabal": edit(proto, "Seki.Proto\n  ghc-options: -O2", "Seki.Proto\n  ghc-options: -O0")}),
                         {"runtime": False, "driver": False})

    @unittest.expectedFailure
    def test_the_sources_of_a_library_that_no_group_names_are_compared(self):
        self.assertEqual(self.identical({"seki.cabal": self.proto(), "src-proto/Seki/Proto.hs": "module Seki.Proto where\n"},
                                        {"src-proto/Seki/Proto.hs": "module Seki.Proto where\nslow :: Int\nslow = 1\n"}),
                         {"runtime": False, "driver": False})

    @unittest.expectedFailure
    def test_a_flag_default_is_compared(self):
        flagged = edit(edit(CABAL, "common warnings\n", "flag fast\n  default: True\n  manual: True\n\ncommon warnings\n"),
                       "  ghc-options: -O2\n", "  if flag(fast)\n    ghc-options: -O2\n  else\n    ghc-options: -O0\n")
        self.assertEqual(self.identical({"seki.cabal": flagged}, {"seki.cabal": edit(flagged, "default: True", "default: False")}),
                         {"runtime": False, "driver": False})

    @unittest.expectedFailure
    def test_a_custom_build_type_is_compared(self):
        custom = edit(edit(CABAL, "build-type: Simple", "build-type: Custom"), "common warnings\n", "custom-setup\n  setup-depends: base, Cabal\n\ncommon warnings\n")
        self.assertEqual(self.identical({"Setup.hs": "import Distribution.Simple\nmain = defaultMain\n"},
                                        {"seki.cabal": custom, "Setup.hs": "import Distribution.Simple\nmain = defaultMainWithHooks simpleUserHooks\n"}),
                         {"runtime": False, "driver": False})

    @unittest.expectedFailure
    def test_the_cabal_version_is_compared(self):
        self.assertEqual(self.identical({}, {"seki.cabal": edit(CABAL, "cabal-version: 3.0", "cabal-version: 3.4")}), {"runtime": False, "driver": False})

    @unittest.expectedFailure
    def test_a_setup_script_counts_as_different_under_a_custom_build_type(self):
        custom = edit(CABAL, "build-type: Simple", "build-type: Custom")
        self.assertEqual(self.identical({"seki.cabal": custom, "Setup.hs": "main = defaultMain\n"}, {"Setup.hs": "main = defaultMainWithHooks simpleUserHooks\n"}),
                         {"runtime": False, "driver": False})

    def test_a_freeze_file_is_compared(self):
        self.assertEqual(self.identical({}, {"cabal.project.freeze": "package seki\n  ghc-options: -O0\n"}), {"runtime": False, "driver": False})

    def test_a_project_file_that_imports_another_counts_as_different(self):
        imports = {"cabal.project": "packages: .\nimport: build.project\n"}
        sources = self.sources(imports | {"build.project": "package seki\n  ghc-options: -O2\n"}, {"build.project": "package seki\n  ghc-options: -O0\n"})
        self.assertEqual({name: group["identical"] for name, group in sources.items()}, {"runtime": False, "driver": False})
        self.assertEqual({group["reason"] for group in sources.values()}, {"`cabal.project` imports another project file, which the runner does not compare"})

    @unittest.expectedFailure
    def test_a_component_header_in_another_case_is_still_compared(self):
        capital = edit(CABAL, "library seki-core\n", "Library seki-core\n")
        self.assertEqual(self.identical({"seki.cabal": capital}, {"seki.cabal": edit(capital, "-O2", "-O0")}), {"runtime": False, "driver": True})

    @unittest.expectedFailure
    def test_a_test_suite_dependency_bound_is_compared(self):
        self.assertEqual(self.identical({}, {"seki.cabal": edit(CABAL, "  build-depends: base, seki\n", "  build-depends: base, seki, text <2.1\n")}),
                         {"runtime": False, "driver": False})

    def test_a_change_only_inside_a_test_suite_stays_identical(self):
        tests = edit(edit(CABAL, "-threaded", "-threaded -rtsopts"), "  main-is: Main.hs\n  ghc-options", "  main-is: Spec.hs\n  other-modules: Seki.CoreTest\n  ghc-options")
        self.assertEqual(self.identical({}, {"seki.cabal": tests}), {"runtime": True, "driver": True})

    def test_an_executable_that_adds_no_dependency_constraint_stays_identical(self):
        daily = edit(edit(CABAL, "test-suite seki-test\n", "executable seki-daily-config\n  import: warnings\n  hs-source-dirs: test-public\n"
                                                           "  main-is: DailyConfigMain.hs\n  build-depends: base, seki\n\ntest-suite seki-test\n"),
                     "seki:seki-latency", "seki:seki-latency, seki:seki-daily-config")
        self.assertEqual(self.identical({}, {"seki.cabal": daily}), {"runtime": True, "driver": True})

    def test_a_revision_with_an_untracked_project_file_is_refused(self):
        self.commit({"cabal.project": "packages: .\n"})
        (self.repo / "cabal.project.freeze").write_text("constraints: any.text ==2.0.2\n")
        with self.assertRaisesRegex(ValueError, "cabal.project.freeze"):
            runner.parse_revisions([f"trunk={self.repo}"])


if __name__ == "__main__":
    unittest.main()
