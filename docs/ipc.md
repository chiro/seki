# IPC version 1

The IPC endpoint is `$XDG_RUNTIME_DIR/seki/ipc.sock`. The containing directory
belongs to the effective user and has mode 0700. Clients have that user's authority,
including permission to execute shell commands through `spawn`.

There is one IPC server per runtime directory. Endpoint setup errors are logged,
and window management continues without IPC for that session pass. An existing
owner's endpoint is untouched. Clients still address the fixed path, so a second
WM sharing the runtime directory cannot be selected through IPC. Use a separate
runtime directory for isolated test sessions. Setup is attempted again on a new
session pass; a degraded running session does not replace the existing endpoint.

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
commands are errors. Schema errors receive `invalid_request` and permit another
request on the same connection. Unknown workspace tags receive `invalid_command`.
Fields not used by an operation are ignored.

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
`xkb` describes keyboard layouts; see below. Output wire IDs are session-local.
They are not persistent output names or window identities.

The `scratchpads` array contains `name` and `state` for each owned or pending
scratchpad. States are `pending`, `shown`, and `hidden`. An unresolved launch
stays `pending` until a matching window arrives or `scratchpad-reset` clears it.
An absent name has no entry.

Subscribe delivers an initial snapshot, then updated immutable projections.
A subscriber can have one replaceable pending snapshot. Intermediate revisions
may be skipped. Each command connection processes one request and its reply
before reading the next request. Application replies use per-command result
cells and bounded socket writes; snapshot replacement cannot replace a reply.

The session ingress is the sole command queue. It holds at most 256 inputs,
shared with other external producers, and drains at most 32 per manage sequence.
A full ingress returns `overloaded`; a closed ingress returns `unavailable`.
At most 32 connections are active, each processing one command at a time.
Commands from different connections submit independently. A disconnected command
handler awaiting its receipt keeps its connection slot until it finishes or the
session stops.
Slow clients cannot block policy dispatch. Client reads time out after 60 seconds
and writes after 5 seconds. Each command waits at most 5 seconds after admission
for its application receipt. If no receipt arrives, that request receives
`unavailable` with application unknown. Later commands on the same connection
and other connections remain available. A timeout does not undo a command that
may apply later, and commands are never automatically replayed.
Oversized snapshots produce an explicit `snapshot_too_large` error instead of a
partial snapshot. Titles, output names, and layout descriptions are limited to 1024 Unicode characters with
explicit truncation indicators. Escaped JSON byte size still obeys the line limit.

The runtime owns one wake worker. Ingress callbacks signal it without waiting
for transport, and repeated signals coalesce into one pending flag. A client
disconnect, timeout, or failed write abandons its replies without cancelling
admitted work or the wake worker. Normal stop closes ingress and waits for any
in-flight wake before sending `stop` or `destroy`. Session abort closes ingress,
cancels and joins the wake worker, then closes the Wayland connection.

Subscribers reconnect after EOF, with a delay, and issue a new subscribe request.
The initial revision belongs to the new session. A disconnect before a command
reply leaves application ambiguous. Clients never automatically replay spawn or
layout changes. Retry workspace selection only when the desired state is known.
JSON escaping does not escape Pango markup. A bar must escape title markup again
and must never interpolate titles into shell commands.


`subscribe` occupies its connection until EOF. It does not accept further
requests on that connection. Further client bytes are discarded until EOF; they
are not interpreted as requests and do not signal disconnection.
`sekictl subscribe` prints server error replies to stderr and keeps reading the
same stream, including after `snapshot_too_large`. It reconnects after one second
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
The example handles SIGTERM and reaps its CLI child when Waybar stops.


Invalid requests retain a valid string request ID when JSON can be decoded.
Malformed JSON or invalid IDs use the empty string as the error ID. Diagnostic
messages are capped at 1024 characters. Workspace tags are identities and are
never truncated. A configuration whose full workspace projection exceeds the
line limit receives `snapshot_too_large`, including on subscription. Each such
error retains the subscription's request ID.

### Input devices (M3-03)

`input.availability` is `available` or `unavailable`. Absence of optional input
protocols does not stop window management. `requested_rule_count` accompanies
unavailability. An available inventory has `devices`: `{items, count, truncated}`.
Count is the complete live inventory size; items are a deterministic
generation-ordered prefix whose encoded values consume at most 24000 bytes. No
removal history is retained. A device's `identity` is `{generation}`, a counter
that is unique within the IPC session. `wire_id` is an ephemeral diagnostic and
can be reused; never use it as a stable identity.

Each device has nullable `name`, `type`, lifecycle (`discovering`, `ready`,
`removed`), libinput availability and an `xkb` object. Names are capped at 128
characters with `name_truncated`. Defaults and current values are arrays of
`{setting, value}`; an absent setting is unknown, never an assumed default.
Generic input management has no default/current feedback. XKB has no defaults.
Results are `{setting, requested, revision, status}`. Status is one of `pending`,
`applied`, `rejected_invalid`, `sent`, `observed`, `unavailable`, `unsupported`,
`removed`. `pending` covers both a request not yet sent and one awaiting its
keymap load, compilation, or libinput result.
Only libinput success acknowledgements mean applied. Generic requests remain
sent. XKB layout/lock requests become observed only after matching feedback.
Keymap compilation success permits assignment; assignment remains sent. Local
load errors are unavailable; compositor compilation errors are rejected_invalid.
No keymap contents are serialized. Paths and selectors have explicit truncation flags and are capped at 128
characters. Custom curves include movement type, step, point_count, at most eight
points and an explicit truncated flag; matrix values are finite or null.
Diagnostics use the same bounded collection shape, capped at 1024 encoded bytes,
with at most 32 retained facet messages, each projected as a 128-character
`message` and explicit `truncated` flag. Nonfinite numbers are null.

`xkb` is null when input management is unavailable; otherwise it is a bounded
collection (8000 encoded item bytes) of `{identity, name, state}` for keyboards.
State holds availability, nullable layout_index/layout_name, caps_lock, num_lock,
and scroll_lock. Unknown locks are null, including scroll lock before protocol
v3. Layout names are capped at 128 characters with `layout_name_truncated`; keyboard
summary names have `name_truncated`. Separate keyboards retain separate
states; this is not an active-global-keyboard indicator. The shipped Waybar helper
sorts by name then identity and displays every retained device as `name: layout`,
using `?` for unknown values and an ellipsis when the collection is truncated.
Tiling layout remains the workspace's `layout` field.

Input changes wake the runtime owner and are projected with the same immutable
inventory as the window snapshot. Workers never read input maps or send Wayland
requests. Unchanged input does not trigger another wake. Input changes are logged
as bounded structured JSON with session/device/setting identities and device names
(capped at 128 characters, with `name_truncated`). JSON and Pango
escaping are separate; the Waybar helper escapes the combined display string and
never interpolates device or layout data into a shell command. Older snapshots
with absent or null xkb remain accepted.

When input protocols are unavailable, `requested_rules` contains bounded rule
summaries (`name_pattern`, nullable `type`, bounded `settings`). Each requested
setting explicitly includes `setting`, `requested`, and status `unavailable`. The rule collection has an 8000-byte
item budget and each settings collection 4096 bytes, with counts and truncation.
The pattern is its escaped source form. This diagnostic never claims a matched
device or an applied setting. Status logs omit unchanged current values, so a
layout feedback stream does not duplicate terminal setting-result log entries.

Setting names and values in `requested`, `defaults`, and `current` are:

| Setting | JSON value |
| --- | --- |
| `repeat_info` | `{rate, delay}` integer rate and delay |
| `scroll_factor`, `accel_speed` | finite number |
| `map_output` | null to clear, or `{name, truncated}` |
| `map_rectangle` | null to clear, or `[x, y, width, height]` |
| `send_events` | `events_enabled`, `events_disabled`, `events_disabled_on_external_mouse` |
| `tap`, `drag`, `natural_scroll`, `left_handed`, `middle_emulation`, `scroll_button_lock`, `disable_while_typing`, `disable_while_trackpoint` | boolean |
| `tap_button_map`, `clickfinger_button_map` | `left_right_middle` or `left_middle_right` |
| `drag_lock` | `drag_lock_disabled`, `drag_lock_timeout`, `drag_lock_sticky` |
| `three_finger_drag` | `three_finger_drag_disabled`, `three_finger_drag_enabled`, `four_finger_drag_enabled` |
| `calibration_matrix` | six numbers in protocol order |
| `acceleration` | `accel_none`, `accel_flat`, `accel_adaptive`, `accel_custom`, or an array of custom curve summaries |
| `click_method` | `click_none`, `click_button_areas`, `click_finger` |
| `scroll_method` | `scroll_none`, `scroll_two_finger`, `scroll_edge`, `scroll_on_button` |
| `scroll_button` | unsigned button code |
| `rotation` | unsigned degrees, below 360 |
| `keymap` | `{path, truncated}`; never file contents |
| `active_layout` | `{index}` or `{name, truncated}` |
| `caps_lock`, `num_lock`, `scroll_lock` | boolean |

A custom curve summary is `{type, step, point_count, points, truncated}`. Type is
`curve_fallback`, `curve_motion`, or `curve_scroll`. At most three curves and eight
preview points per curve are serialized. Valid requested curves contain at most
64 points; point_count always describes the complete input. Text values above
use a 128-character cap and explicit flags. A defensive nonfinite numeric input
is null, including a null matrix/curve component; public validation rejects it
before connection. Null clear requests differ from absent unknown current values.

`{"version":1,"id":"build","op":"recompile"}` requests a build in an
installed session. `sekictl recompile` and the installed `seki --recompile` send
this request to the existing owner in the runtime directory. They do not open a
Wayland connection. A session without installed build resources refuses the IPC
request; its configured restart action retains direct M2 handover semantics.
Installed sessions route that action through the same build lifecycle.

The applied revision receipt acknowledges reduction of the request, not build,
validation, activation, or a displayed frame. The additive `build` snapshot field
is null for direct runtimes. Installed sessions report state, generation,
queued generation where applicable, error and full disk log path.
States include idle, building, failed and validated. A validated image
has passed its own `--check`; this does not acknowledge activation. A fresh
managed session and its new IPC session identity establish successful runtime
initialization. Trusted user code and hooks are not sandboxed, and there is no
supervisor promise for arbitrary user initialization crashes.

One compiler runs per session. Requests during that build collapse into one
queued rebuild and invalidate the earlier result. Further requests do not grow
the queue. The worker copies Cabal's resolved executable before validation;
Cabal cannot overwrite the selected copy. Compiler output stays on disk, with
at most 8 KiB of diagnostics retained for a failure. Compilation has no
arbitrary deadline; list-bin and candidate checks retain their bounded
execution/output limits.

Build/check failure leaves the profile and running session intact. Successful
validation precedes stop; successful state serialization precedes profile
promotion. The validated target binds the canonical installed profile, including
when the old session is the shipped default fallback. Immediate exec failure
rolls the profile back before reconnecting
with the old in-memory configuration and saved state. Reconnection recreates
IPC, input adapters and build ownership. Restore uses the existing workspace,
layout and scratchpad fallbacks, retaining accessible surviving windows and
spawnOnce history. This contract does not cover an arbitrary crash after exec.
