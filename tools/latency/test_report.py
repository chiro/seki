import importlib.util
from pathlib import Path
import tempfile
import unittest

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


class ReportTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.path = Path(self.temp.name)
        self.wm = dict(clean_exit=True, successful=4, failed=0, dropped=0, major_gcs=1,
                       samples=[dict(kind=k, start_ns=s, end_ns=s+1) for k,s in [("manage",1),("manage",11),("render",13),("render",21)]])
        self.driver = dict(valid=True, native_checks=True, observed_actions=1, steps=1, action_codes=[0], start_ns=10,end_ns=20,
                           workload="m2", exercised={name: 1 for name in runner.WORKLOADS["m2"]["exercised"]})
        self.pure = dict(valid=True,steps=1,checksum=123,samples_ns=[3],workload="m2")
        for name in ["wm", "pure"]:
            (self.path/f"{name}.rts").write_text(RTS)
        for n in [0,1]:
            runner.write(self.path/f"wm-checkpoint-{n}.json",dict(major_gcs=n,gcs=n,allocated_bytes=n))

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

    def test_schema_1_driver_is_m1(self):
        del self.driver["workload"], self.driver["exercised"], self.pure["workload"]
        self.assertEqual(self.summarize("m1")["exercised"], {})

    def test_schema_1_manifest_is_m1(self):
        self.assertEqual(runner.manifest_workload({"schema": 1}), "m1")
        self.assertEqual(runner.manifest_workload({"schema": 2, "workload": "m2"}), "m2")

    def test_report_names_a_missed_budget_and_counts_per_action(self):
        self.wm["samples"] = [dict(kind=k, start_ns=s, end_ns=e) for k,s,e in [("manage",1,2),("manage",11,12_000_009),("render",12_000_009,12_000_010),("render",12_000_013,12_000_014)]]
        self.driver["end_ns"] = 12_000_010
        trial = self.path/"00-normal"
        trial.mkdir()
        for name in ["wm","driver","pure"]:
            runner.write(trial/f"{name}.json",getattr(self,name))
        for name in ["wm.rts","pure.rts","wm-checkpoint-0.json","wm-checkpoint-1.json"]:
            (trial/name).write_text((self.path/name).read_text())
        runner.write(self.path/"manifest.json", dict(schema=2, workload="m2", machine="m", compiler="c", binary_sha256="h", seed=1, live_steps=1, pure_steps=1, pure_windows=2,
                                                    trials=[dict(directory="00-normal", collector="normal", load_before=[0], load_after=[0])]))
        summary = runner.report(self.path)
        self.assertEqual(summary["trials"][0]["budgets"]["manage_p99"], False)
        text = (self.path/"report.md").read_text()
        self.assertIn("## Budgets", text)
        self.assertIn("00-normal manage_p99 = 12.0000 ms", text)
        self.assertIn("| 00-normal | 1 | 1 | 1.00 | 1.00 |", text)
        self.assertEqual(runner.read(self.path/"summary.json")["trials"][0]["budgets"], summary["trials"][0]["budgets"])


if __name__ == "__main__":
    unittest.main()
