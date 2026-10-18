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
        self.driver = dict(valid=True, native_checks=True, observed_actions=1, steps=1, action_codes=[0], start_ns=10,end_ns=20)
        self.pure = dict(valid=True,steps=1,checksum=123,samples_ns=[3])
        for name in ["wm", "pure"]:
            (self.path/f"{name}.rts").write_text(RTS)
        for n in [0,1]:
            runner.write(self.path/f"wm-checkpoint-{n}.json",dict(major_gcs=n,gcs=n,allocated_bytes=n))

    def summarize(self):
        for name in ["wm","driver","pure"]:
            runner.write(self.path/f"{name}.json",getattr(self,name))
        return runner.summarize_trial(self.path)

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


if __name__ == "__main__":
    unittest.main()
