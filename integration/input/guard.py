#!/usr/bin/env python3
"""Refuse a serial fixture run if another headless river is alive."""
import json
from pathlib import Path

active = []
for entry in Path('/proc').iterdir():
    if not entry.name.isdigit():
        continue
    try:
        args = entry.joinpath('cmdline').read_bytes().split(b'\0')
        if not args or args[0].rsplit(b'/', 1)[-1] != b'river':
            continue
        if entry.joinpath('stat').read_text().split(') ', 1)[1].split()[0] == 'Z':
            continue
        env = dict(value.split(b'=', 1) for value in
                   entry.joinpath('environ').read_bytes().split(b'\0') if b'=' in value)
        if env.get(b'WLR_BACKENDS') == b'headless':
            active.append({'pid': int(entry.name), 'runtime':
                           env.get(b'XDG_RUNTIME_DIR', b'').decode(errors='replace')})
    except (FileNotFoundError, ProcessLookupError, PermissionError):
        continue
print(json.dumps({'active_headless_rivers': active}))
if active:
    raise SystemExit('serial private-compositor guard failed; no process was killed')
