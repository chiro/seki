#!/usr/bin/env python3
"""Native layout notification to complete subscriber line; no screenshot timing."""
import json
import os
import subprocess
import sys
import time

samples = []
last = None
with subprocess.Popen([os.environ['SEKICTL_EXECUTABLE'], 'subscribe'],
                      stdout=subprocess.PIPE) as process:
    try:
        while len(samples) < 1001:
            line = process.stdout.readline()
            received = time.monotonic_ns()
            if not line:
                raise RuntimeError('subscriber EOF')
            snapshot = json.loads(line)['result']['snapshot']
            keyboards = (snapshot.get('xkb') or {}).get('items', [])
            device = next((d for d in keyboards if d.get('name') ==
                           'seki-private-backend-keyboard'), None)
            if not device or device['state']['layout_index'] is None:
                continue
            group = device['state']['layout_index']
            if group != last:
                samples.append((received, group))
                last = group
    finally:
        process.terminate()
        process.wait(timeout=3)

source = [tuple(map(int, line.split())) for line in open(sys.argv[1])]
matched = []
for received, group in samples[1:]:
    candidates = [(index, stamp) for index, (stamp, reported) in enumerate(source)
                  if reported == group and stamp <= received]
    if not candidates:
        raise RuntimeError('missing source event')
    index, stamp = candidates[-1]
    matched.append((index, received - stamp))
if any(b[0] != a[0] + 1 for a, b in zip(matched, matched[1:])):
    raise RuntimeError('dropped/coalesced native layout notifications')
values = sorted(delta / 1e6 for _, delta in matched)
result = {'endpoint': 'native notify_modifiers start to subscriber complete LF',
          'samples': len(values), 'p99_ms': values[989], 'max_ms': max(values),
          'target_p99_ms': 50, 'failed_or_dropped': 0}
with open(sys.argv[2], 'w') as output:
    json.dump({'summary': result, 'raw': matched}, output)
print(json.dumps(result))
assert len(values) == 1000 and values[989] < 50 and max(values) < 100
