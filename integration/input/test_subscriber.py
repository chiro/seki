#!/usr/bin/env python3
"""The layout subscriber must report the latency that its source really has."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest

LATENCY_MS = 80


def snapshot(group):
    keyboard = {'name': 'seki-private-backend-keyboard', 'state': {'layout_index': group}}
    return json.dumps({'result': {'snapshot': {'xkb': {'items': [keyboard]}}}})


def source(receipts):
    """Stand in for the fixture backend, seki, and sekictl.

    Change the layout every 10 ms and publish each change LATENCY_MS later.
    """
    group = 0
    unpublished = []
    print(snapshot(group), flush=True)
    change_at = time.monotonic_ns()
    while True:
        now = time.monotonic_ns()
        if unpublished and unpublished[0][0] <= now:
            print(snapshot(unpublished.pop(0)[1]), flush=True)
        elif change_at <= now:
            group = 1 - group
            with open(receipts, 'a') as file:
                file.write(f'{now} {group}\n')
            unpublished.append((now + LATENCY_MS * 1_000_000, group))
            change_at += 10_000_000
        else:
            time.sleep(0.0005)


class SubscriberTest(unittest.TestCase):
    # Issue 73: the subscriber matches each receipt to the latest source event
    # with the same layout, so it reads any latency as under 20 ms.
    @unittest.expectedFailure
    def test_a_latency_above_the_ceiling_is_reported_and_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            sekictl = Path(directory, 'sekictl')
            sekictl.write_text(f'#!/bin/sh\nexec {sys.executable} -B {__file__} source\n')
            sekictl.chmod(0o700)
            receipts = Path(directory, 'source.txt')
            result = Path(directory, 'subscriber.json')
            done = subprocess.run(
                [sys.executable, '-B', Path(__file__).with_name('subscriber.py'), receipts, result],
                env=dict(os.environ, SEKICTL_EXECUTABLE=str(sekictl), SEKI_FAKE_RECEIPTS=str(receipts)),
                capture_output=True, timeout=60)
            summary = json.loads(result.read_text())['summary']
            self.assertGreaterEqual(summary['p99_ms'], LATENCY_MS)
            self.assertNotEqual(done.returncode, 0)


if __name__ == '__main__':
    if sys.argv[1:] == ['source']:
        source(os.environ['SEKI_FAKE_RECEIPTS'])
    else:
        unittest.main()
