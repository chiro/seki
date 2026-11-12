#!/usr/bin/env python3
"""Native layout notification to complete subscriber line; no screenshot timing.

The fixture backend changes the layout only when it can take a byte from the
acknowledgement FIFO, and this subscriber writes a byte only after it has read
the change before. So receipt N belongs to source event N at any latency, and
a change and its reversal cannot both fall between two snapshots.
"""
import json
import os
import select
import subprocess
import sys
import time

KEYBOARD = 'seki-private-backend-keyboard'
CHANGES = 1000
# The p99 is the 990th smallest of 1000 latencies.
P99_RANK = CHANGES * 99 // 100
P99_MS = 50
MAX_MS = 100
TIMEOUT_S = 5


def layout(line):
    snapshot = json.loads(line)['result']['snapshot']
    keyboards = (snapshot.get('xkb') or {}).get('items', [])
    device = next((d for d in keyboards if d.get('name') == KEYBOARD), None)
    return device['state']['layout_index'] if device else None


def source_stamps(path):
    """Return when the backend started each layout change so far, in nanoseconds."""
    try:
        with open(path) as file:
            return [int(line.split()[0]) for line in file]
    except FileNotFoundError:
        return []


def latency_ms(index, received, source_path):
    """Return how long layout change `index` took from its source event to `received`."""
    stamps = source_stamps(source_path)
    if len(stamps) <= index or stamps[index] > received:
        raise RuntimeError(f'layout change {index} did not come from source event {index}')
    return (received - stamps[index]) / 1e6


def check_ceilings(latencies):
    """Raise as soon as `latencies` can no longer meet the ceilings.

    The run time grows with the latency, so a slow run must not wait for
    every change. The p99 is below P99_MS exactly when at most
    CHANGES - P99_RANK latencies reach it.
    """
    index, latest = len(latencies) - 1, latencies[-1]
    if latest >= MAX_MS:
        raise RuntimeError(f'layout change {index} took {latest:.1f} ms; the maximum is {MAX_MS} ms')
    slow = sum(latency >= P99_MS for latency in latencies)
    if slow > CHANGES - P99_RANK:
        raise RuntimeError(f'layout change {index} took {latest:.1f} ms; {slow} changes have taken '
                           f'{P99_MS} ms or more, and the p99 ceiling allows {CHANGES - P99_RANK} '
                           f'of {CHANGES}')


def measure(command, ack_path, source_path, timeout=TIMEOUT_S):
    """Return the latency of each of CHANGES layout changes, in milliseconds.

    Raise when the next change does not arrive within `timeout` seconds.
    """
    latencies = []
    last = None
    partial = b''
    # With O_NONBLOCK, the open fails when the backend does not read the FIFO.
    with open(os.open(ack_path, os.O_WRONLY | os.O_NONBLOCK), 'wb', buffering=0) as ack, \
            subprocess.Popen(command, stdout=subprocess.PIPE) as process:
        try:
            lines = process.stdout.fileno()
            deadline = time.monotonic() + timeout
            while len(latencies) < CHANGES:
                remaining = deadline - time.monotonic()
                if remaining <= 0 or not select.select([lines], [], [], remaining)[0]:
                    if last is None:
                        raise RuntimeError(f'no layout for {KEYBOARD} within {timeout} s')
                    index = len(latencies)
                    emitted = len(source_stamps(source_path)) > index
                    fate = 'emitted but not published' if emitted else 'not emitted'
                    raise RuntimeError(f'layout change {index} was {fate} within {timeout} s')
                chunk = os.read(lines, 65536)
                received = time.monotonic_ns()
                if not chunk:
                    raise RuntimeError('subscriber EOF')
                *complete, partial = (partial + chunk).split(b'\n')
                for line in complete:
                    group = layout(line)
                    if group is None or group == last:
                        continue
                    if last is not None:
                        latencies.append(latency_ms(len(latencies), received, source_path))
                        check_ceilings(latencies)
                    last = group
                    ack.write(b'\n')
                    deadline = time.monotonic() + timeout
        finally:
            process.terminate()
            process.wait(timeout=3)
    return latencies


if __name__ == '__main__':
    ack_path, source_path, result_path = sys.argv[1:]
    latencies = measure([os.environ['SEKICTL_EXECUTABLE'], 'subscribe'], ack_path, source_path)
    result = {'endpoint': 'native notify_modifiers start to subscriber complete LF',
              'samples': len(latencies), 'p99_ms': sorted(latencies)[P99_RANK - 1],
              'max_ms': max(latencies), 'target_p99_ms': P99_MS, 'failed_or_dropped': 0}
    with open(result_path, 'w') as output:
        json.dump({'summary': result, 'latencies_ms': latencies}, output)
    print(json.dumps(result))
