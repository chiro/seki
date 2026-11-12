#!/usr/bin/env python3
"""The layout subscriber must report the latency that its source really has."""
import itertools
import json
import os
import re
import signal
import sys
import tempfile
import time
import unittest

import subscriber

FAULTY = 3


def snapshot(keyboards):
    return json.dumps({'result': {'snapshot': {'xkb': {'items': keyboards}}}})


def keyboard(group):
    return {'name': subscriber.KEYBOARD, 'state': {'layout_index': group}}


def source(ack_path, source_path, latency_ms, fault):
    """Stand in for the fixture backend, seki, and sekictl.

    Change the layout once for each acknowledgement byte and publish the
    change `latency_ms` later. `fault` applies to change FAULTY.
    """
    if fault == 'absent':
        print(snapshot([]), flush=True)
        signal.pause()
    group = 0
    print(snapshot([keyboard(group)]), flush=True)
    with open(ack_path, 'rb', buffering=0) as acknowledgements:
        for change in itertools.count():
            if not acknowledgements.read(1):
                return
            if (fault, change) == ('unemitted', FAULTY):
                signal.pause()
            group = 1 - group
            if (fault, change) != ('uncommanded', FAULTY):
                with open(source_path, 'a') as file:
                    file.write(f'{time.monotonic_ns()} {group}\n')
            time.sleep(latency_ms / 1000)
            while (fault, change) == ('unpublished', FAULTY):
                print(snapshot([keyboard(1 - group)]), flush=True)
            print(snapshot([keyboard(group)]), flush=True)


def hung(signum, frame):
    raise TimeoutError('the subscriber hung')


class SubscriberTest(unittest.TestCase):
    def setUp(self):
        # A subscriber that hangs must fail its test, not block the run.
        signal.signal(signal.SIGALRM, hung)
        signal.alarm(10)
        self.addCleanup(signal.alarm, 0)
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.ack_path = os.path.join(directory.name, 'ack')
        self.source_path = os.path.join(directory.name, 'source.txt')
        # The backend makes and opens the FIFO like this when river starts.
        os.mkfifo(self.ack_path)
        self.addCleanup(os.close, os.open(self.ack_path, os.O_RDONLY | os.O_NONBLOCK))

    def measure(self, latency_ms=0, fault='none'):
        command = [sys.executable, '-B', __file__, 'source', self.ack_path, self.source_path,
                   str(latency_ms), fault]
        return subscriber.measure(command, self.ack_path, self.source_path, timeout=0.5)

    def test_a_latency_below_the_ceilings_passes(self):
        self.assertEqual(len(self.measure()), subscriber.CHANGES)

    def test_a_latency_above_the_ceiling_is_reported_and_fails(self):
        with self.assertRaisesRegex(
                RuntimeError, 'layout change 10 took .* 11 changes have taken 50 ms or more') as failure:
            self.measure(latency_ms=60)
        reported = float(re.search(r'took ([0-9.]+) ms', str(failure.exception)).group(1))
        self.assertGreaterEqual(reported, 60)

    def test_a_latency_above_the_maximum_fails_at_once(self):
        with self.assertRaisesRegex(
                RuntimeError, 'layout change 0 took .* the maximum is 100 ms') as failure:
            self.measure(latency_ms=110)
        reported = float(re.search(r'took ([0-9.]+) ms', str(failure.exception)).group(1))
        self.assertGreaterEqual(reported, 110)

    def test_an_unpublished_change_times_out_by_index(self):
        with self.assertRaisesRegex(
                RuntimeError, f'layout change {FAULTY} was emitted but not published within 0.5 s'):
            self.measure(fault='unpublished')

    def test_an_unemitted_change_times_out_by_index(self):
        with self.assertRaisesRegex(
                RuntimeError, f'layout change {FAULTY} was not emitted within 0.5 s'):
            self.measure(fault='unemitted')

    def test_a_change_without_a_source_event_fails(self):
        with self.assertRaisesRegex(
                RuntimeError, f'layout change {FAULTY} did not come from source event {FAULTY}'):
            self.measure(fault='uncommanded')

    def test_a_keyboard_that_never_appears_times_out(self):
        with self.assertRaisesRegex(
                RuntimeError, f'no layout for {subscriber.KEYBOARD} within 0.5 s'):
            self.measure(fault='absent')


if __name__ == '__main__':
    if sys.argv[1:2] == ['source']:
        source(sys.argv[2], sys.argv[3], int(sys.argv[4]), sys.argv[5])
    else:
        unittest.main()
