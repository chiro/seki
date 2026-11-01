import json
import os
from pathlib import Path
import selectors
import socket
import subprocess
import sys
import tempfile
import time


def check_subscription(executable):
    with tempfile.TemporaryDirectory(prefix="seki-cli-stream-") as runtime:
        directory = Path(runtime) / "seki"
        directory.mkdir(mode=0o700)
        with socket.socket(socket.AF_UNIX) as server:
            server.bind(str(directory / "ipc.sock"))
            server.listen()
            server.settimeout(3)
            child = subprocess.Popen(
                [executable, "subscribe"],
                env={**os.environ, "XDG_RUNTIME_DIR": runtime},
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            data = b""
            try:
                connection, _ = server.accept()
                with connection:
                    request = b""
                    while not request.endswith(b"\n"):
                        chunk = connection.recv(4096)
                        if not chunk:
                            raise AssertionError("subscriber closed before requesting")
                        request += chunk
                        assert len(request) <= 65536
                    assert json.loads(request)["op"] == "subscribe"
                    payloads = [
                        {"result": {"snapshot": {"marker": "initial"}}},
                        {"error": {"code": "snapshot_too_large", "message": "test boundary"}},
                        {"result": {"snapshot": {"marker": "recovered"}}},
                    ]
                    for payload in payloads:
                        envelope = {"version": 1, "id": "sekictl", **payload}
                        connection.sendall(json.dumps(envelope).encode() + b"\n")
                    with selectors.DefaultSelector() as selector:
                        selector.register(child.stdout, selectors.EVENT_READ)
                        deadline = time.monotonic() + 2
                        while time.monotonic() < deadline:
                            if not selector.select(0.1):
                                continue
                            chunk = os.read(child.stdout.fileno(), 4096)
                            if not chunk:
                                break
                            data += chunk
                            if b"recovered" in data:
                                break
            finally:
                child.terminate()
                remaining, errors = child.communicate(timeout=3)
            lines = [json.loads(line) for line in (data + remaining).splitlines()]
            markers = [line["result"]["snapshot"]["marker"] for line in lines]
            assert markers == ["initial", "recovered"], markers
            diagnostics = [json.loads(line) for line in errors.splitlines() if line.startswith(b"{")]
            assert any(line.get("error", {}).get("code") == "snapshot_too_large" for line in diagnostics)
            print("SEKICTL_STREAM_RECOVERY_PASS protocol-mock")


if __name__ == "__main__":
    check_subscription(sys.argv[1])
