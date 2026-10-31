# IPC version 1

The IPC endpoint is `$XDG_RUNTIME_DIR/seki/ipc.sock`. The containing directory
belongs to the effective user and has mode 0700. Clients have that user's authority,
including permission to execute shell commands through `spawn`.

Each message is one UTF-8 JSON object followed by LF. The maximum request or reply
line is 65536 bytes excluding LF. CR before LF counts toward this limit. Invalid
UTF-8, malformed JSON, and oversized lines terminate that client connection.
Requests use a string `id` of at most 128 Unicode characters and `version: 1`.
IDs correlate replies within a connection. The server does not deduplicate IDs.

```json
{"version":1,"id":"42","op":"snapshot"}
{"version":1,"id":"43","op":"subscribe"}
{"version":1,"id":"44","op":"workspace","tag":"3"}
{"version":1,"id":"45","op":"spawn","command":"foot"}
{"version":1,"id":"46","op":"layout","action":"next"}
```

Layout actions are `next`, `shrink`, `expand`, `inc-master`, and `dec-master`.
Unknown operations, versions, actions, and workspace tags are errors. Empty spawn
commands are errors. Fields not used by an operation are ignored.

Replies contain `version`, `id`, and either `result` or `error`. Errors contain
`code` and `message`. An application result contains `revision`. It means the
reducer applied the command, including a no-op, and scheduled its intents. It does
not mean a displayed frame or successful child process execution. External input
FIFO does not establish a total order with protocol events.

Snapshot results and subscription events contain a `snapshot` object with a
session-local integer `revision`, an opaque `session` identifier, `outputs`, and `workspaces` in configured order.
Output `name` is the nullable compositor connector name obtained from `wl_output`
version 4. Its registry global and wire ID remain session-local.
Each workspace has `tag`, `occupied`, `visible`, `focused`, and `layout`.
`focused_title` is nullable. Workspace focus expresses policy intent.
`keyboard_available` separately describes layer focus and seat availability.
`urgent`, `xkb`, and deferred `submap` are null because these capabilities are
unsupported in this version. Output wire IDs are session-local. They are not
persistent output names or window identities.

Subscribe delivers an initial snapshot, then updated immutable projections.
A subscriber can have one replaceable pending snapshot. Intermediate revisions
may be skipped. Application replies use a separate bounded queue and are never
replaced by snapshots. Commands have a 256 input backlog and drain in batches of
32. At most 32 connections are active. Slow clients cannot block policy dispatch.
Oversized snapshots produce an explicit `snapshot_too_large` error instead of a
partial snapshot. Titles and output names are limited to 1024 Unicode characters with
explicit truncation indicators. Escaped JSON byte size still obeys the line limit.

The session owns admitted work and wake submission. A client disconnect, timeout,
or failed write abandons its replies, and cannot cancel the wake owner. Session
stop closes external ingress before producer cleanup.

Subscribers reconnect after EOF, with a delay, and issue a new subscribe request.
The initial revision belongs to the new session. A disconnect before a command
reply leaves application ambiguous. Clients never automatically replay spawn or
layout changes. Retry workspace selection only when the desired state is known.
JSON escaping does not escape Pango markup. A bar must escape title markup again
and must never interpolate titles into shell commands.


`subscribe` occupies its connection until EOF. It does not accept further
requests on that connection. `sekictl subscribe` reconnects after one second
and writes diagnostics to stderr. Other commands make one attempt and never
replay after an ambiguous disconnect. Exit codes are 0 for success, 1 for a
server error, 64 for usage or request size errors, and 69 for a transport or
invalid-response error.

The Linux abstract socket guard serializes seki server startup and endpoint
cleanup. A refused liveness probe permits stale socket replacement. A successful,
timed-out, or otherwise failed probe refuses replacement. Foreign regular files
and symlinks are rejected. Cleanup removes only the inode this server bound.

`seki-wire` adds `aeson`, `stm`, and `deepseq`. M3-06 must include these in the
external-configuration package proof. The public Seki configuration is unchanged.
The release shell includes Waybar and Python. The example invokes `sekictl` with a
fixed argument list and escapes Pango markup independently from JSON encoding.
Copy `examples/waybar/seki.py` to the path used in its accompanying config.
