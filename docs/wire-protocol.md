# Wayland Wire Protocol and Binding Generator Design

Status: **accepted — W4 river bootstrap** / Last updated: 2026-08-14

This document refines the wire-protocol portion of `docs/spec.md` into an
implementation design for M0. It covers the Wayland client transport, message
codec, object lifecycle, XML scanner, generated bindings, and the first river
integration slice.

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
- Generated bindings enforce interface versions and object types. The later
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
- Optimizing allocations before correctness and integration tests exist.

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

M0 vendors `wayland.xml`, `river-window-management-v1.xml`, and
`river-xkb-bindings-v1.xml`. The other river XML files are added with the M2
features that use them. Cross-protocol interface and enum references are
resolved across all XML documents given to the scanner.

Each vendored source MUST have its upstream URL, tag or commit, SHA-256 digest,
and license recorded in `protocol/SOURCES.md`. Copyright and license text in
the XML MUST be retained.

### 3.2 Correction applied to the general specification

The earlier `docs/spec.md` described `river-window-management-v1` as version 3
and `river-xkb-bindings-v1` as version 2. The pinned river 0.4.5 release exposes
versions 4 and 3 respectively. It also adds the version 4 unique window
identifier used for hot-swap restoration. The operation that establishes
seki's minimum window-management protocol requirement, `set_content_clip_box`,
remains a since-version-3 request.

The general specification now records that:

- seki requires `river_window_manager_v1` version 3 functionality;
- river 0.4.5 advertises version 4;
- seki binds the lower of the advertised and generated maximum versions; and
- modifier watching is in `river-xkb-bindings-v1` version 3 in river 0.4.5.

This wording separates the required feature version from the version shipped
by the minimum compositor release.

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
  { sender  :: ObjectId
  , opcode  :: Opcode
  , payload :: ByteString
  }
```

The real definitions will use names that do not require globally disabling
record-field warnings.

### 5.2 Argument encoding

| XML type | Wire form | Proposed Haskell form |
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
data DecodeResult a
  = Done a
  | NeedBytes Int
  | NeedFds Int
  | DecodeFailed WireError
```

The exact API may encode the remaining input in a state value, but it MUST
distinguish missing bytes from missing descriptors. A complete frame that
expects an `fd` cannot be dispatched until the corresponding descriptor has
arrived.

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
  { bytes :: ByteString
  , fds   :: [OwnedFd]
  }
```

The transport appends bytes and descriptors to separate FIFO buffers. The
connection layer repeatedly decodes complete frames from their fronts.

Received descriptors MUST be close-on-exec, preferably using
`MSG_CMSG_CLOEXEC`, with `fcntl` as a fallback. `MSG_CTRUNC`, malformed control
messages, an unsupported control-message type, or exceeding the descriptor
queue limit is fatal. All descriptors already received are closed during error
cleanup.

EOF is a clean disconnect only when no partial frame, pending ancillary data,
or live protocol operation remains. Otherwise it is reported as a truncated
connection.

### 6.3 Sending and descriptor ownership

An encoded outgoing message contains its bytes and a list of descriptors:

```haskell
data OutgoingMessage = OutgoingMessage
  { bytes :: ByteString
  , fds   :: [OwnedFd]
  }
```

Messages containing descriptors are sent with one `sendmsg` call initially.
If the call writes at least one byte but only a prefix of the frame, the
ancillary data has been transferred; the remaining bytes are sent without the
descriptors. `EINTR` retries without changing state. `EAGAIN` waits for
writability. A zero-byte successful result is treated as no progress.

Passing an `OwnedFd` to an outgoing message transfers ownership to the
connection. The local descriptor is closed after a successful transfer and is
also closed if the connection fails before transfer. Incoming descriptors are
transferred to the event handler only after successful argument decoding.

Only one writer may use a connection. M0 performs protocol work on the event
loop thread. Later asynchronous tasks submit commands to that thread rather
than writing protocol requests directly; this also protects river's sequence
ordering.

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
result retains typed access to both the underlying session and the bound
`river_window_manager_v1` object.

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
phase capabilities needed by the later `ManageM` and `RenderM` APIs:
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
state, capabilities, and keyboard focus. The rendering surface includes
visibility, borders and clipping, node position, and stacking. Each operation
is available only in the phase type allowed by the pinned river XML; finish
requests are deliberately absent because the runtime owns completion.

These plans are pure ordered request lists, not general IO transformers. They
have `Functor`, `Applicative`, and `Monad` instances for composition but no
operation for lifting arbitrary IO, keeping blocking work outside river's
response deadline. The private runtime execution hooks submit requests in plan
order and stop at the first structured connection failure. The next integration
step will supply these plans from reconciler callbacks inside the checked
sequence boundary.

### 7.8 Fatal display errors

`wl_display.error` is decoded even when the referenced object is already being
destroyed. Its object ID, numeric error code, interface when known, and message
are preserved in the final error. After a display error or transport failure,
the connection transitions once to closed, wakes waiters, closes all queued
descriptors, and rejects new requests.

## 8. Generated Binding Shape

Each interface gets a phantom marker, descriptor, request functions, and an
event ADT. A representative shape is:

```haskell
data WlRegistry

wlRegistryInterface :: Interface WlRegistry

data WlRegistryEvent
  = WlRegistryGlobal GlobalName InterfaceName Version
  | WlRegistryGlobalRemove GlobalName

wlRegistryBind
  :: Object WlRegistry
  -> GlobalName
  -> Interface a
  -> Version
  -> Wire (Object a)
```

Names are idiomatic Haskell names prefixed by their interface where needed to
avoid collisions. Generated modules use explicit export lists and include the
XML descriptions as Haddock documentation.

The common runtime exports neither untyped request construction nor unchecked
object casts to window-manager code. A deliberately internal raw API may exist
for codec and generator tests.

Generated request functions perform:

1. object/interface validation;
2. `since` version validation;
3. argument validation;
4. new-ID allocation and registration;
5. encoding; and
6. ordered submission to the connection.

Generated event decoders perform the inverse and return structured events. The
exact handler-registration surface is intentionally left for a small prototype:
either per-object typed callbacks or an existential routed-event stream can
satisfy the design. The choice must not leak untyped argument lists upward.

The generated river request functions are internal. Later modules wrap them:

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

No timestamp is emitted. Regeneration followed by `git diff --exit-code` is the
CI freshness check.

The scanner is a separate Cabal executable component. Scanner-only dependencies
do not become dependencies of the seki runtime library.

## 10. Proposed Repository Layout

```text
protocol/
  SOURCES.md
  wayland/wayland.xml
  river/river-window-management-v1.xml
  river/river-xkb-bindings-v1.xml

src/Seki/Wire/
  Types.hs
  Codec.hs
  Transport.hs
  Connection.hs

src-generated/Seki/Protocol/
  Wayland.hs
  RiverWindowManagementV1.hs
  RiverXkbBindingsV1.hs

tools/scanner/
  Main.hs
  IR.hs
  Parse.hs
  Generate.hs

cbits/
  seki_wayland_socket.c
  seki_wayland_socket.h

test/
  wire/
  scanner/
  golden/
```

The library uses both `src` and `src-generated` as source directories. The
generated modules are internal modules unless a later use case justifies a
supported low-level API.

Initial runtime dependencies are expected to be `bytestring`, `containers`,
`network`, `text`, and `unix`. Scanner and test dependencies are selected when
their first code is written; dependency weight and maintenance status are part
of that review.

## 11. Errors, Diagnostics, and Tracing

Errors are structured by layer:

```haskell
data WireError
  = ConnectionDiscoveryError ...
  | TransportError ...
  | TruncatedConnection ...
  | MalformedFrame ...
  | InvalidArgument ...
  | DescriptorTruncated
  | UnknownObject ObjectId
  | UnexpectedOpcode ObjectId Opcode
  | UnsupportedVersion InterfaceName Version
  | ProtocolError ...
```

The public error text includes interface and message names when the object table
can resolve them. It never logs raw descriptor contents or silently replaces
invalid strings.

`SEKI_WIRE_DEBUG=1` enables a trace similar in purpose to `WAYLAND_DEBUG`:

```text
-> 7@river_window_v1.propose_dimensions(1280, 720)
<- 1@wl_display.delete_id(12)
```

Tracing is off by default, goes through seki's logger, and must not change
ordering or descriptor lifetime.

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

Real-river tests are tagged integration tests and skipped with an explicit
reason when river or a suitable backend is unavailable. They are never replaced
by the mock compositor in release validation.

## 13. Implementation Sequence

### W0: Pin sources and settle public shapes

- Correct the version wording in `docs/spec.md`.
- Vendor XML and provenance metadata.
- Prototype the event-handler surface and choose typed callbacks or routed
  events.

Acceptance: source digests and licenses are reviewable; parsing all three M0 XML
files produces a validated IR.

### W1: Pure codec

- Wire types, frame header, primitive encoders/decoders, incremental buffering.
- No live Wayland connection yet.

Acceptance: golden, boundary, fragmentation, and property tests pass.

### W2: Unix transport

- Connection discovery, socket wrapper, C ancillary-data shim, ownership and
  cleanup.

Acceptance: socket-pair descriptor tests pass under repeated and failure runs
without leaks.

### W3: Scanner and generated core bindings

- IR validation, deterministic Haskell generation, `wayland.xml` bindings.

Acceptance: generated code is stable across two runs, compiles cleanly, and
completes a mock registry sync.

### W4: Real registry and river bindings

- Connect to a real display, negotiate versions, bind river globals, route
  window/output/seat events.

Acceptance: a smoke program connects to nested river and logs a complete initial
state without protocol errors.

### W5: M0 window-manager slice

- Minimal river sequence state machine.
- Full-output placement for every window.
- One terminal binding and one close-window binding.

Acceptance: a terminal can be launched, focused, resized to the output, used,
and closed on nested river. Every sequence is finished within river's deadline.

Japanese input and Xwayland validation remain final M0 acceptance items from the
general specification; they exercise the compositor/client path rather than
adding wire-layer features.

## 14. Open Review Items

1. **Event delivery API:** typed per-object callbacks or a typed existential
   event stream. Prototype both only far enough to compare object creation and
   cleanup.
2. **XML parser dependency:** select a maintained parser after measuring the
   scanner dependency footprint. Do not write a bespoke XML parser.
3. **C shim portability:** M0 is Linux-only. Decide whether the shim API should
   expose BSD `SO_NOSIGPIPE` immediately or keep only the Linux implementation
   until another target is supported.
4. **Resource limits:** choose numeric byte, descriptor, and object-table limits
   alongside tests rather than guessing them in this document.
5. **Big-endian testing:** the codec must use native byte order, but CI access to
   a big-endian Wayland environment is unresolved.

## 15. References

- [Wayland protocol and model of operation](https://wayland.freedesktop.org/docs/book/Protocol.html)
- [Wayland core protocol specification](https://wayland.freedesktop.org/docs/html/apa.html)
- [river v0.4.5 protocol sources](https://github.com/riverwm/river/tree/v0.4.5/protocol)
- [river window-management protocol documentation](https://isaacfreund.com/docs/wayland/river-window-management-v1/)
- [`unix(7)` ancillary data](https://man7.org/linux/man-pages/man7/unix.7.html)
- [`recvmsg(2)`](https://man7.org/linux/man-pages/man2/recvmsg.2.html)
- [`cmsg(3)`](https://man7.org/linux/man-pages/man3/cmsg.3.html)
