# Wayland Wire Protocol and Binding Generator Design

Status: **implemented through M1** / Last updated: 2026-10-18

This document records the implemented wire protocol and runtime through M1.
It refines the wire-protocol portion of `docs/spec.md` and covers the
Wayland client transport, message codec, object lifecycle, XML scanner,
generated bindings, and current river policy. M0-specific sections retain the
initial implementation scope.

The words **MUST**, **SHOULD**, and **MAY** describe requirements on seki's
implementation. They are not additions to the Wayland or river protocols.

## 1. Decisions at a Glance

- The runtime is a native Haskell Wayland client and does not link to
  `libwayland-client`.
- A small C shim around `sendmsg(2)` and `recvmsg(2)` is allowed. It wraps libc
  socket APIs only; it does not implement protocol policy or depend on
  libwayland.
- Protocol XML is pinned and vendored. Generated Haskell modules are committed
  so building seki does not require running the scanner.
- The codec is pure except for file-descriptor ownership. Transport, object
  dispatch, and generated protocol bindings are separate layers.
- Generated descriptors and encoders carry interface and request metadata.
  Connection preparation validates sender types and request versions.
  `ManageM` and `RenderM` wrappers enforce river's sequence phases.
- M0 implements only the protocols needed to enumerate and place windows and
  outputs, launch a terminal, and close the focused window. The scanner itself
  is designed for all Wayland XML argument types from the beginning.

## 2. Scope

### 2.1 Goals

1. Connect using `WAYLAND_SOCKET` or `WAYLAND_DISPLAY`.
2. Encode and decode every Wayland wire argument type.
3. Transfer file descriptors safely with `SCM_RIGHTS`.
4. Track object IDs, interface types, versions, and destruction.
5. Generate typed requests and events from Wayland protocol XML.
6. Negotiate globals without assuming that the compositor and generated
   bindings support identical versions.
7. Produce useful errors and optional wire traces without relying on
   `WAYLAND_DEBUG` from libwayland.
8. Validate the implementation with pure tests, Unix socket-pair tests, a mock
   compositor, and nested river.

### 2.2 Non-goals for M0

- A public general-purpose Wayland library.
- A server-side Wayland implementation.
- Surface rendering, shared-memory buffers, or window-manager UI.
- Dynamic loading of protocol XML at runtime.
- Generating C headers or interoperating with libwayland objects.
- Enforcing river's manage/render sequence in generated code. That belongs in
  the higher-level runtime API.
- Allocation tuning without a benchmark demonstrating a need.

## 3. Protocol Source and Version Policy

Protocol behavior is defined by the pinned XML, not by an online protocol
viewer or a distribution's currently installed files.

### 3.1 Initial source set

| Source | Pin | Used in M0 |
|---|---|---|
| Wayland core `wayland.xml` | Wayland 1.25.0, commit `3e673a438b0a9749e3bdf5cac4befac86333024c` | `wl_display`, `wl_registry`, `wl_callback` |
| river protocols | river `v0.4.5`, commit `f6d961773711dc29d3eb5b60bd14744a6a33894f` | window management and XKB bindings |

The river release contains these interface versions:

| Protocol | Maximum interface version in river 0.4.5 |
|---|---:|
| `river-window-management-v1` | 4 |
| `river-xkb-bindings-v1` | 3 |
| `river-input-management-v1` | 1 |
| `river-layer-shell-v1` | 1 |
| `river-libinput-config-v1` | 1 |
| `river-xkb-config-v1` | 1 |

M0 vendored `wayland.xml`, `river-window-management-v1.xml`, and
`river-xkb-bindings-v1.xml`. M1 added `river-layer-shell-v1.xml` to the runtime.
Input-configuration protocols remain deferred to M2. Integration-only sources
include wlr-layer-shell, xdg-shell, and the image-capture protocols. See
[`protocol/SOURCES.md`](../protocol/SOURCES.md) for the current pins.
Cross-protocol interface and enum references are resolved across all XML
documents given to the scanner.

Each vendored source MUST have its upstream URL, tag or commit, SHA-256 digest,
and license recorded in `protocol/SOURCES.md`. Copyright and license text in
the XML MUST be retained.

### 3.2 Required and shipped versions

seki distinguishes the protocol version whose features it requires from the
version that its minimum compositor release ships:

- seki requires `river_window_manager_v1` version 3 functionality, because
  `set_content_clip_box` is a since-version-3 request;
- river 0.4.5 advertises version 4, which adds the unique window identifier
  used for hot-swap restoration;
- seki binds the lower of the advertised and generated maximum versions; and
- modifier watching is in `river-xkb-bindings-v1` version 3 in river 0.4.5.

### 3.3 Runtime version negotiation

Every object stores the version at which it was created. For a registry global,
the chosen version is:

```text
min(server-advertised version, generated maximum version)
```

Binding fails with a structured error if that value is below seki's required
minimum for the interface. A generated request whose `since` value exceeds the
object version is rejected locally and is not written to the socket.

Newer optional requests and events may be generated without raising seki's
minimum river version. Higher-level code checks capabilities before using
them.

## 4. Layered Architecture

```text
Seki.Run.*
  river phase state machine, policy, reconciler
        |
Seki.Protocol.*                    generated modules
  typed requests and event ADTs    interface metadata and codecs
        |                                  |
Seki.Wire.Connection <-------------+------+
  object table, versions, routing, ID allocation
        |
Seki.Wire.Codec
  pure framing and argument primitives
        |
Seki.Wire.Transport
  Unix socket, sendmsg/recvmsg, SCM_RIGHTS
        |
libc / kernel (not libwayland)
```

The boundaries are intentional:

- `Codec` can be exhaustively unit- and property-tested without IO.
- `Transport` knows nothing about interfaces or opcodes.
- `Connection` knows object metadata but not river policy.
- generated modules know XML signatures but not the manage/render phase.
- `Seki.Run.*` is the only layer allowed to expose phase-specific operations to
  the rest of the window manager.

## 5. Wire Representation

### 5.1 Frames

Every frame begins with two native-endian 32-bit words:

```text
word 0: sender object ID
word 1: message size in upper 16 bits | opcode in lower 16 bits
```

The size includes the eight-byte header. A decoder MUST reject a size smaller
than eight, a size not divisible by four, a frame larger than 65,535 bytes, or
a sender object ID of zero. It MUST wait rather than fail when a valid header
announces a frame whose bytes have not all arrived yet.

Requests and events have separate opcode spaces. Within each space, opcodes are
the zero-based position of the request or event in the interface XML.

```haskell
newtype ObjectId = ObjectId Word32
newtype Opcode = Opcode Word16

data RawFrame = RawFrame
  { rawFrameSender  :: ObjectId
  , rawFrameOpcode  :: Opcode
  , rawFramePayload :: ByteString
  }
```

### 5.2 Argument encoding

| XML type | Wire form | Implemented Haskell form |
|---|---|---|
| `int` | signed 32-bit | `Int32` |
| `uint` | unsigned 32-bit | `Word32` or a generated newtype |
| `fixed` | signed 24.8 fixed point | opaque `Fixed` over `Int32` |
| `string` | byte length including NUL, UTF-8 plus NUL, padded to four bytes | `Text`; `Maybe Text` when nullable |
| `object` | 32-bit object ID; zero means null | `Object a`; `Maybe (Object a)` when nullable |
| `new_id` | 32-bit new object ID | newly allocated or server-provided `Object a` |
| `array` | byte length, bytes, padding | opaque `ByteString`, with protocol-specific typed helpers |
| `fd` | no payload bytes; passed as ancillary data | ownership-tracked `OwnedFd` |

For a `new_id` with no statically named interface, such as
`wl_registry.bind`, the wire form is expanded to interface-name string,
interface version, and object ID. The scanner MUST identify this case from the
missing `interface` attribute; it is not a name-based special case for
`wl_registry`.

Strings MUST have a terminating NUL, contain no interior NUL, fit inside their
frame, and use zero padding when encoded. Decoding rejects invalid UTF-8 at the
generated string boundary. Arrays remain byte arrays in the generic layer;
for example, the later libinput binding will add a native-endian `Double`
array helper.

Unknown enum values MUST remain representable. Non-bitfield enums are generated
as newtypes with patterns or conversion functions rather than closed sum types
that fail when a compositor sends a value added by a compatible extension.
Bitfield enums are represented as a set-like newtype over `Word32` and preserve
unknown bits.

### 5.3 Incremental decoding

Bytes and file descriptors arrive as independent ordered streams. The decoder
therefore has four outcomes:

```haskell
data DecodeResult fd a
  = Done a ByteString [fd]
  | NeedBytes Int
  | NeedFds Int
  | DecodeFailed ArgumentError
```

The implemented argument decoder distinguishes missing bytes from missing
descriptors. A complete frame that expects an `fd` cannot be dispatched until
the corresponding descriptor has arrived.

No parser may read beyond the current frame payload. Trailing payload after all
declared arguments is a malformed-frame error, not ignored extension data.

## 6. Unix Transport

### 6.1 Opening the connection

Connection discovery follows Wayland's standard environment contract:

1. If `WAYLAND_SOCKET` is set, parse it as an already connected file descriptor,
   take ownership, and set close-on-exec.
2. Otherwise, if `WAYLAND_DISPLAY` is an absolute path, connect to it directly.
3. Otherwise, connect to `$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY`, using `wayland-0`
   when `WAYLAND_DISPLAY` is unset.
4. Missing, malformed, overlong, or non-socket values produce a specific startup
   error.

The `network` package may own socket creation and Unix-address handling. A small
portable C shim performs `sendmsg`/`recvmsg` and control-message traversal,
because the `cmsghdr` layout and `CMSG_*` operations should not be reproduced
with hard-coded Haskell structure offsets.

The initial platform target is Linux, matching river. The shim's interface is
kept narrow enough to add another POSIX implementation later.

### 6.2 Receiving

Every socket read MUST use `recvmsg`, including reads for frames that are not
expected to contain descriptors. Wayland permits ancillary descriptors to be
attached to any byte of the stream, including a frame header.

A receive operation returns:

```haskell
data ReceivedChunk = ReceivedChunk
  { receivedBytes :: ByteString
  , receivedFds   :: [OwnedFd]
  }
```

The connection's receive buffer appends bytes and descriptors from each
transport chunk to separate FIFO queues. The dispatcher repeatedly decodes
complete frames from their fronts.

Received descriptors MUST be close-on-exec, preferably using
`MSG_CMSG_CLOEXEC`, with `fcntl` as a fallback. `MSG_CTRUNC`, malformed control
messages, an unsupported control-message type, or exceeding the descriptor
queue limit is fatal. All descriptors already received are closed during error
cleanup.

The receive buffer accepts EOF only after all complete frames have drained
and no partial frame or descriptors remain. Partial frames and missing or
surplus descriptors produce structured errors. Clean EOF does not prove that
an application operation or river sequence completed.

The transport treats `ECONNRESET` on receive as EOF. Linux returns it instead
of EOF when the peer closes before it reads everything seki sent, and only
after it delivers every byte the peer sent. A reset therefore loses seki's
last requests but none of river's events, and the same EOF checks apply.

### 6.3 Sending and descriptor ownership

Request preparation returns a `PreparedRequest` containing frame bytes and
ordered descriptors. The transport receives them through its actual entry point:

```haskell
sendMessage :: Transport -> ByteString -> [OwnedFd] -> IO (Either TransportError ())
```

Messages containing descriptors are sent with one `sendmsg` call initially.
If the call writes at least one byte but only a prefix of the frame, the
ancillary data has been transferred; the remaining bytes are sent without the
descriptors. `EINTR` retries without changing state. `EAGAIN` waits for
writability. A zero-byte successful result is treated as no progress.

Calling `Transport.sendMessage` transfers ownership of its descriptors to
the transport, including on send failure. Constructing an encoder or failing
request preparation does not consume them. After a successful byte transfer
the transport closes its local descriptors; failure cleanup also closes them.
Incoming descriptors reach the event handler only after successful argument
decoding.

`Connection` serializes preparation and submission under its state `MVar`.
The window manager's phase-sensitive requests remain owned by the runtime
event loop and its phase plans.

### 6.4 Resource limits

The transport and connection impose explicit limits on:

- buffered but undispatched bytes;
- queued file descriptors;
- objects in the object table; and
- text and array sizes, which must also fit the 16-bit frame-size limit.

Limits are constants with tests, not silent truncation. Exceeding one closes the
connection after cleaning up owned resources.

## 7. Connection and Object Lifecycle

### 7.1 Object table

Object ID 1 is installed as `wl_display` before any request is sent. Every live
object-table entry stores:

- object ID;
- interface descriptor;
- negotiated version;
- lifecycle state; and
- event decoder/handler information.

The sender ID and opcode select an object and an event decoder. An unknown
object, interface mismatch, unexpected opcode, or event newer than the object's
version is a fatal protocol error. The frame boundary is known, but continuing
would hide a broken binding or a compositor/client disagreement.

### 7.2 ID allocation

Client-created IDs are allocated by the connection. An ID is not reusable after
a destructor request until `wl_display.delete_id` acknowledges deletion.
Allocation and registration of a request-created object happen atomically
before the request is made visible to other Haskell code.

For a server-created `new_id` event, the generated decoder supplies the new
object's typed interface marker. The connection applies the parent-bounded
version rule and registers that object before dispatching any later frame.

Concretely, a statically typed event argument is decoded as `NewObject a`, an
opaque token pairing the wire ID with the generated interface marker. The
event route must turn every token into a `ServerObjectRoute` containing the
matching interface descriptor, child event route, and creation callback. The
connection rejects missing or extra route plans, registers all child objects
at `min(parent version, child maximum version)`, and publishes their routes in
the same state transition that finalizes the parent event. Creation callbacks
then run before the parent event handler. This ensures that even a child event
in the immediately following frame has a route, without exposing an unchecked
object cast.

The lifecycle model distinguishes client- and server-allocated ID namespaces.
Client-created objects become inactive after either a destructor request or a
destructor event, but their IDs remain tombstoned until
`wl_display.delete_id` makes them reusable. Server-created IDs are removed
immediately after their destructor request because the client never allocates
from that namespace:

```text
client active -> destructor request/event -> retired -> delete_id -> reusable
server active -> destructor request -> removed
active -> connection failed -> closed
```

Generated object constructors are not exported, preventing callers from
inventing IDs or assigning the wrong interface to an ID.

### 7.3 Wayland session startup

`Seki.Run.Startup` discovers or accepts an explicit Wayland connection target,
opens the transport, installs the display route, and creates the registry and
sync callback with their routes atomically. It dispatches events until the
`wl_display.sync` callback fires, so the initial catalog is complete when the
session is returned. EOF, connection failures, and invalid registry events
before that barrier fail startup and close all acquired resources.

The session retains the registry route and a synchronized live catalog rather
than only returning the initial snapshot. Later event-loop dispatch therefore
keeps global additions and removals visible through
`readWaylandGlobalCatalog`. Session close is idempotent.

### 7.4 River manager bootstrap

`Seki.Run.River` is the first river-specific layer above the generic
connection. It selects exactly one `river_window_manager_v1` global, caps the
negotiated version at the generated maximum, and rejects versions below 3,
which is the feature baseline required by seki. Binding installs the manager
route atomically with the registry request.

The bootstrap caller supplies typed handlers for manager, window, output, and
seat events plus creation callbacks for the three child types. The manager
route converts each generated `NewObject` token into the corresponding child
route. This keeps interface selection and river's required version out of the
generic Wayland connection while giving later sequence-state code a complete,
typed event tree.

### 7.5 Owned river runtime

`Seki.Run.Runtime` composes synchronized Wayland startup with manager binding.
Opening is all-or-nothing: if global selection or binding fails, it closes the
Wayland session rather than returning a partially initialized runtime. The
result retains typed access to the underlying session and the bound
`river_window_manager_v1`, `river_xkb_bindings_v1`, and `river_layer_shell_v1`
objects.

`runRiverSession` claims single, one-shot ownership of event dispatch; a second
or concurrent runner is rejected. It continues until clean EOF, promotes
connection and live-registry failures to structured runtime errors, and closes
the entire session on every exit path. Handler exceptions remain exceptions,
but resource cleanup still runs before they are rethrown.

### 7.6 River sequence ordering

`Seki.Run.Sequence` models the protocol ordering independently from IO. Its
opaque state begins by requiring `manage_start`; completing that manage
sequence produces a `manage_finish` decision and requires at least one
`render_start`. Completing a render sequence produces `render_finish`, after
which river may start another render sequence or a new manage sequence.

The transition layer rejects overlapping, repeated, and out-of-order starts or
completions with the current phase and attempted operation. It also exposes the
phase capabilities used by the `ManageM` and `RenderM` APIs:
management changes are legal only during manage, while rendering changes are
legal during manage or render. Keeping the transition pure lets the runtime
submit the returned completion request before publishing the next state.

The owned runtime applies these transitions around the manager callback. For a
valid `manage_start` or `render_start`, it publishes the active phase, invokes
the callback, submits the matching finish request, and only then publishes the
next inactive phase. The runtime therefore owns sequence completion; callbacks
must not send finish requests themselves. Invalid boundary events are withheld
from the callback and returned from `runRiverSession` as structured sequence
errors. Completion submission failures remain structured connection errors.

### 7.7 Phase-safe request plans

`Seki.Run.Phase` provides distinct `ManageM` and `RenderM` plan types. The M0
management surface includes closing, dimension proposals, decoration and tiled
state, capabilities, keyboard focus, and XKB binding configuration. The
rendering surface includes visibility, borders and clipping, node position, and
stacking. Each operation is available only in the phase type allowed by the
pinned river XML; finish
requests are deliberately absent because the runtime owns completion.

These plans are pure ordered request lists, not general IO transformers. They
have `Functor`, `Applicative`, and `Monad` instances for composition but no
operation for lifting arbitrary IO. `RiverRuntimeHandlers` pairs the low-level
object/event handlers with IO callbacks that prepare the next plans from a
`RiverRuntimeSnapshot`. Policy uses these callbacks to read mutable state and
launch asynchronous effects. The returned phase plans remain pure. The snapshot
combines the window/output inventory with live seat and binding handles without
exposing mutable runtime state. The pinned river does not enforce a WM response
timer; seki's measured budgets are described in [latency.md](latency.md).

At a valid boundary the runtime delivers the boundary callback, prepares and
submits the corresponding plan in request order, and only then submits the
finish request. It stops at the first structured connection failure. A failed
plan is never followed by a finish request, so the runtime cannot claim that a
partially applied sequence completed successfully.

### 7.8 River object inventory

`Seki.Run.Inventory` is the pure state model fed by the routed window and output
callbacks. It retains typed object handles and persistent window metadata,
actual dimensions, output global names, positions, and dimensions. Transient
policy requests such as maximize are deliberately not persisted. Closed windows
and removed outputs leave the live inventory immediately.

Updates require the exact tracked object handle, including its generation, so a
stale handle cannot modify a reused object ID. IDs are unique across all tracked
river object kinds. The model validates positive dimensions, ordered non-negative
dimension hints, once-only PID/output-name/identifier events, and river v4's
printable-ASCII identifier rule. Complete output position and size state is
exposed as a rectangle for placement planning.

Workspace membership, desired focus, and close targeting belong to the pure
StackSet core. Inventory retains observed protocol facts and live handles; it
does not select an active window or a fallback after closure.

The runtime applies inventory transitions before delivering creation or event
callbacks, and returns validation failures as structured runtime errors without
delivering the invalid event. On each window creation it also makes exactly one
`get_node` request, stores the resulting typed `river_node_v1` handle with the
window, and only then delivers the creation callback. Node-allocation failures
likewise stop dispatch before exposing a partially initialized window. The
latest snapshot remains readable from `RiverSession`, including after shutdown
for diagnostics and tests. On closure/removal, callbacks run once and the
runtime destroys the window's node before its window object, or the removed
output object, so long-running sessions do not accumulate protocol objects.

Each new output likewise gets exactly one `river_layer_shell_v1.get_output`
request before its creation callback, and the typed
`river_layer_shell_output_v1` handle is stored with the output. Its
`non_exclusive_area` events update the output's usable area in the callback,
stored as a core `Rect` built with `mkRect`: zero area and negative origins are
kept exactly, while a negative size or a far edge beyond `Int32` is a
structured inventory error. Only the latest area is kept. On
`river_output_v1.removed` the runtime destroys the layer-shell output before
the river output.

### 7.9 Scene planning and reconciliation

`Seki.Internal.Core.Scene` plans layout selections using pure window IDs,
workspace stacks, output usable areas, and parent relationships. Layout-omitted
windows remain explicitly hidden. Content rectangles are inset by the border
width so that proposed content plus borders fit their tiles. Actual oversized
client commits may overflow under M1's accepted mismatch policy. Degenerate
tiles produce no size proposal.

`Seki.Run.Reconcile` joins those IDs to live inventory handles and compiles
phase-safe plans. Each window has at most one outstanding dimension proposal.
A newer dimensions revision settles it, even when the client repeats its size
or accepts a size different from the proposal. The latest scene supplies the
next target when a proposal is superseded. Closed or replaced object handles
invalidate the cached state.

Render reconciliation applies settled positions, complete family stacking,
borders, and explicit visibility. A first-time selection remains hidden until
its dimensions settle. An existing selection retains its last settled size
while a resize is pending. Focus requests follow the shown state and respect
layer-shell keyboard ownership. The runtime sends a requested `manage_dirty`
only after `render_finish` succeeds. See
[Scene reconciliation](reconciliation.md) for the state transitions and live
compositor evidence.

### 7.10 XKB bindings bootstrap

`Seki.Run.Xkb` selects and binds the unique `river_xkb_bindings_v1` global.
The bootstrap negotiates the generated maximum version and requires version 3,
which provides modifier watching for the configured binding model. Missing, duplicate, obsolete, and request-failure
cases remain distinct structured errors.

`Seki.Run.LayerShell` selects and binds the unique `river_layer_shell_v1`
global at version 1. Binding it is what tells river that the window manager
supports layer surfaces; without it river closes them. The registry rejects
version 0 announcements, so the layer-shell error type has missing, duplicate,
and bind-failure cases but no obsolete case.

The owned runtime preflights all three required river globals,
`river_window_manager_v1`, `river_xkb_bindings_v1`, and `river_layer_shell_v1`,
before submitting any bind request, then binds them in that order. Any failure
closes the partially opened Wayland session. `RiverSession` retains and exposes
all three typed handles.

### 7.11 XKB seat and binding lifecycle

`RiverKeyBinding` describes a keysym, modifier mask, and optional layout
override independently from an action. When the manager announces a
`river_seat_v1`, the runtime creates an initially disabled
`river_xkb_binding_v1` object for every configured definition and installs its
typed event route atomically with the creation request. The seat creation
callback is withheld until every binding object exists; a partial creation
failure becomes the session's structured connection error.

The runtime retains the seat, definitions, and typed binding handles in a
snapshot readable from `RiverSession`. Binding events are delivered with their
definition so the action layer does not need to recover identity from an object
ID. On `river_seat_v1.removed`, the seat leaves the snapshot before callbacks
run, then every binding is destroyed in definition order before the seat itself.
Unknown, duplicate, and stale seat handles are fatal structured runtime errors.

Before creating its bindings, the runtime sends one
`river_layer_shell_v1.get_seat` for the seat and stores the typed
`river_layer_shell_seat_v1` handle in the seat's state. The latest of
`focus_exclusive`, `focus_non_exclusive`, and `focus_none` is recorded per seat
as `RiverLayerFocus` and appears in the next manage snapshot; the scene
reconciler decides what to do with it. On removal the layer-shell seat is
destroyed after the bindings and before the river seat.

Binding objects begin disabled. At the next manage boundary, the runtime
prepends an internal plan that applies each optional layout override and then
enables every pending binding, in seat/object order, before the user manage
plan. A binding is marked initialized only after the entire combined plan
succeeds, and later manage sequences do not repeat its initial configuration.
The phase API also exposes typed layout-override, enable, and disable operations
for later submap support.

### 7.12 Ordered policy events

A typed binding callback queues a core action in `Seki.Run.Policy` and returns
without submitting phase-sensitive requests. Window and output lifecycle
events and window interactions join the same queue, preserving their arrival
order with key presses.

At `manage_start`, Policy drains that queue through the pure reducer. Actions
produce state transitions and spawn, close, or focus intents. Close intents
retain the pure window ID selected when the action ran and are resolved
against live inventory before submission. A window that closed meanwhile
receives no request. Runtime binding initialization precedes the policy's
manage plan. The runtime has no second deferred-action queue.

### 7.13 Configurable default policy

The public `Seki` module supplies `Config`, pure actions, layouts, border
configuration, default bindings, and the `seki` entry point. The executable is
a small user of that API and runs `seki def`. Configuration is validated before
opening a Wayland connection.

The default configuration provides nine workspaces and cycles Tall, Mirror
Tall, and Full. Bindings derive their modifiers, terminal command, and workspace
names from the final configuration record. Mod4-Shift-Return launches `foot`
and Mod4-Shift-c closes the core's focused window. Layout messages, focus
movement, master swapping, and workspace view/shift actions use the same pure
Action and Policy path as a user configuration. Bindings use XKB layout 0 by
default.

Spawn intents use the asynchronous launcher. Process creation and reaping do
not block the event loop, and a launch failure is reported on standard error.
An internal launcher seam supports process tracking in integration tests;
public actions cannot lift arbitrary IO.

Integration tests construct the production policy with their chosen layout, bindings, borders, and launcher,
so terminal, close, focus, and placement checks exercise the same runtime and
reconciler as the default executable.

### 7.14 Fatal display errors

`wl_display.error` is decoded even when the referenced object is already being
destroyed. Its object ID, numeric error code, interface when known, and message
are preserved in the final error. After a display error or transport failure,
the connection transitions once to closed, wakes waiters, closes all queued
descriptors, and rejects new requests.

## 8. Generated Binding Shape

Each interface gets a phantom marker, descriptor, request functions, and an
event ADT and decoder. For example, the committed Wayland module declares:

```haskell
data WlRegistry

wlRegistryInterface :: Interface WlRegistry

encodeWlRegistryBind :: Word32 -> DynamicNewId -> RequestEncoder fd

data WlRegistryEvent fd
  = WlRegistryGlobal Word32 Text Word32
  | WlRegistryGlobalRemove Word32
```

The generated modules in `src-generated/Seki/Protocol` are authoritative for
current names and signatures. Encoders describe arguments and metadata. They
do not allocate objects or send requests.

Names are idiomatic Haskell names prefixed by their interface where needed to
avoid collisions. Generated modules use explicit export lists and include the
XML descriptions as Haddock documentation.

Raw request construction belongs to the private wire component.
`Seki.Wire.Connection.Request.prepareRequest` validates the sender, interface,
version, and arguments, then encodes the frame and applies destructor state.
`prepareNewObjectRequest` adds client-ID allocation and registration to that
pure transaction. `Connection` serializes submission and installs the resulting
object table and routes after a successful send.

Generated event decoders return structured events. The connection registers a
typed route per object; existential packaging stays inside the route table.
The generated river encoders are private and the runtime wraps them:

```text
generated raw request -> ManageM/RenderM capability -> reconciler
```

This keeps XML generation independent of river-specific phase semantics while
making phase-invalid calls unrepresentable in the public runtime.

## 9. XML Scanner

### 9.1 Intermediate representation

The scanner parses XML into a protocol-neutral IR before printing Haskell:

```text
Protocol
  name, copyright, description, interfaces

Interface
  name, version, requests, events, enums

Message
  name, opcode, since, destructor flag, description, arguments

Argument
  name, type, nullable, interface reference, enum reference, summary

Enum
  name, since, bitfield flag, entries
```

Using an existing XML parser is acceptable; "own code generator" means seki
owns the Wayland schema interpretation and Haskell output, not that it
reimplements XML parsing.

The scanner validates at least:

- unique protocol and interface names;
- interface versions and `since` ranges;
- request/event opcode bounds;
- argument types and nullability rules;
- local and cross-protocol interface references;
- local, qualified, and cross-protocol enum references;
- destructor placement; and
- numeric enum values, including hexadecimal and negative values.

Unknown XML elements or attributes are errors by default. Explicitly ignored
documentation-only constructs are listed in one place, so an upstream schema
addition cannot silently alter generated semantics.

### 9.2 Deterministic output

Generation MUST be deterministic and independent of absolute paths, locale,
time zone, map iteration order, and generation time. Generated files contain:

- a do-not-edit header;
- protocol source and pin;
- interface maximum version;
- typed definitions and codecs; and
- retained protocol documentation.

No timestamp is emitted. During ordinary tests,
[`test/Seki/Scanner/GenerateTest.hs`](../test/Seki/Scanner/GenerateTest.hs)
generates text in memory and compares it with every committed runtime and
fixture binding. CI runs these tests; it does not regenerate files followed
by a Git diff.

The scanner is a separate Cabal executable component. Scanner-only dependencies
do not become dependencies of the seki runtime library.

## 10. Repository Layout

```text
protocol/                     pinned runtime and integration-only XML
  SOURCES.md
  wayland/
  river/                      includes river-layer-shell-v1.xml
  river/upstream/             wlr-layer-shell fixture source
  wayland-protocols/          xdg-shell and image-capture fixture sources

src-public/                   public Seki and Core/Layout facades
src-core/                     private seki-core implementations
src/                          private seki-wire transport and runtime
src-generated/                committed runtime bindings

tools/scanner/                private scanner library and executable
tools/seki/                   public-API executable
tools/registry-smoke/         registry diagnostic
tools/layer-surface-fixture/  native fixture and capture implementation
tools-generated/             committed integration-only bindings
tools/latency/               opt-in diagnostic and report runner

cbits/                        libc transport shim
test/                         internal unit, property, mock, and scanner tests
test-public/                  downstream-style public API compile fixture
integration/                  real-river scenarios
```

This list shows component roots, not every module. [`seki.cabal`](../seki.cabal)
is authoritative for component source directories and module lists. The public
library exposes six modules from `src-public`. `seki-core`, `seki-wire`, the
scanner, and the layer-surface fixture are private components.

The runtime uses `binary`, `bytestring`, `containers`, `network`, `process`,
`text`, and `unix`. The scanner uses `xeno` for XML parsing. Test-only
dependencies remain outside the runtime components.

## 11. Errors, Diagnostics, and Tracing

Errors are structured by layer: frame and argument errors feed receive-buffer
or request/event preparation errors; those feed `ConnectionError`; registry,
bootstrap, inventory, and sequence failures feed `RiverRuntimeError`.
Structured values retain object IDs and interface information where available.
Invalid strings are rejected, and descriptor contents are never logged.

Wire tracing is not implemented. If added, it should resemble
`WAYLAND_DEBUG` without logging descriptor contents:

```text
-> 7@river_window_v1.propose_dimensions(1280, 720)
<- 1@wl_display.delete_id(12)
```

Tracing must be off by default, use seki's logger, and must not change ordering
or descriptor lifetime.

## 12. Verification Strategy

### 12.1 Pure codec tests

- Golden vectors for every primitive argument type.
- Encode/decode round trips over valid values.
- Header sizes at the minimum and maximum boundaries.
- Every possible split point of representative frames.
- Multiple frames in one chunk.
- Missing and surplus descriptor cases.
- Invalid lengths, padding, NUL placement, UTF-8, object zero, and trailing data.
- Host-native byte order without assuming x86 little-endian in source code.

QuickCheck generators distinguish valid protocol values from arbitrary bytes;
fuzz-like arbitrary input tests assert termination, bounds safety, and cleanup,
not successful decoding.

### 12.2 Transport tests with `socketpair`

- Fragmented and coalesced stream writes.
- Transfer of a pipe or temporary-file descriptor with `SCM_RIGHTS`.
- Descriptor order across multiple frames and arguments.
- Descriptors attached to chunks containing partial headers.
- Close-on-exec behavior.
- Partial writes, `EINTR`, peer shutdown, and control-message truncation.
- No descriptor leaks on success, decoding failure, or asynchronous exception.

The syscall shim exposes an injectable narrow boundary where deterministic
partial-write and error tests cannot be produced reliably by the kernel.

### 12.3 Object and scanner tests

- Model/property tests for allocate, destroy, acknowledge, and reuse.
- Rejection of wrong-interface senders and unsupported `since` values.
- XML fixture tests for every argument/reference form.
- Golden generated modules.
- Regeneration idempotence.
- Compilation of generated modules with `-Wall` and the project's warning set.

### 12.4 Integration levels

1. A mock compositor implements `wl_display`, `wl_registry`, and `wl_callback`
   over a socket pair.
2. A registry smoke executable connects to a real Wayland display and prints
   globals, then exits cleanly after a `wl_display.sync` round trip.
3. A nested-river test binds the window manager, records window/output events,
   and completes every manage/render sequence without timing out.
4. The M0 vertical slice places all windows Full and provides terminal/close
   bindings.

The real-river component is disabled by default with an explicit message. Once
explicitly enabled, an unavailable river or required client is a failure rather
than a skip. Real-river tests are never replaced by the mock compositor in
release validation. The isolated headless harness and its scenarios are
documented in [Real-river Integration Tests](integration-testing.md).

## 13. Remaining Technical Follow-up

1. **C shim portability:** seki targets Linux. A non-Linux port must define and
   test equivalent send-safety and close-on-exec behavior.
2. **Big-endian testing:** the codec uses native byte order, but CI has not run
   in a big-endian Wayland environment.
3. **Tracing:** structured errors are implemented; optional wire tracing remains
   future diagnostic work.

## 14. References

- [Wayland protocol and model of operation](https://wayland.freedesktop.org/docs/book/Protocol.html)
- [Wayland core protocol specification](https://wayland.freedesktop.org/docs/html/apa.html)
- [river v0.4.5 protocol sources](https://github.com/riverwm/river/tree/v0.4.5/protocol)
- [river window-management protocol documentation](https://isaacfreund.com/docs/wayland/river-window-management-v1/)
- [`unix(7)` ancillary data](https://man7.org/linux/man-pages/man7/unix.7.html)
- [`recvmsg(2)`](https://man7.org/linux/man-pages/man2/recvmsg.2.html)
- [`cmsg(3)`](https://man7.org/linux/man-pages/man3/cmsg.3.html)
