#!/usr/bin/env python3
import html
import json
import signal
import subprocess


def module(snapshot):
    focused = next((w for w in snapshot.get("workspaces", []) if w["focused"]), {})
    text = " ".join(str(value) for value in (
        focused.get("tag", ""), focused.get("layout", ""),
        snapshot.get("focused_title") or "")).strip()
    keyboards = (snapshot.get("xkb") or {}).get("items", [])
    layouts = []
    for keyboard in sorted(keyboards, key=lambda k: (
            k.get("name") or "", json.dumps(k.get("identity"), sort_keys=True))):
        state = keyboard.get("state") or {}
        layouts.append("{}: {}".format(keyboard.get("name") or "?",
                                      state.get("layout_name") or "?"))
    if layouts:
        text += (" | " if text else "") + "; ".join(layouts)
    if (snapshot.get("xkb") or {}).get("truncated"):
        text += " …"
    return {"text": html.escape(text, quote=True), "tooltip": html.escape(text, quote=True)}


def main():
    def stop(_signal, _frame):
        raise KeyboardInterrupt

    signal.signal(signal.SIGTERM, stop)
    with subprocess.Popen(["sekictl", "subscribe"], stdout=subprocess.PIPE,
                          text=True, encoding="utf-8") as process:
        try:
            for line in process.stdout:
                snapshot = json.loads(line)["result"]["snapshot"]
                print(json.dumps(module(snapshot), ensure_ascii=True), flush=True)
        except (BrokenPipeError, KeyboardInterrupt):
            pass
        finally:
            process.terminate()
            process.wait()


if __name__ == "__main__":
    main()
