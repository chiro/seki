# Scene reconciliation

The scene planner selects windows and content rectangles from the core's
workspaces. The reconciler decides when those selections can be shown and
focused through river's separate manage and render sequences.

## Layout selection and protocol readiness

`DesiredScene` contains pure window and output IDs. A placement means that
the layout selects a positive content rectangle. It does not mean that river
has received the client's first buffer or accepted a size proposal.

The planner insets each tile by the border width. A tile whose content would
have zero width or height becomes omitted. Hidden workspace windows and
windows omitted by `Full` remain in the scene, with no placement. They receive
no dimension proposal.

The reconciler retains the requested size separately from the client's
actual size, and shows the actual size at the desired top-left. The tile's
size policy (M2-D04), taken from its workspace's layout, handles a mismatch.
Under `Crop`, the default, every render that shows the tile also sets a
content clip box equal to the tile's content box, including while a proposal
is pending, so neither content nor border leaves the tile. Under `Tolerate`,
selected with `tolerant`, nothing is clipped, so an oversized client may
overlap its neighbours, and each proposal is the tile clamped to the
window's minimum and maximum hints. A commit smaller than the tile leaves a
gap under either policy, because a clip box cannot enlarge content. Floats
and fullscreen windows take no size policy, and a float is never clipped.
Comparing future targets with the accepted requested size prevents repeated
proposals to a client that cannot use the exact tile size.

## One proposal per window

Every valid dimensions event increments its inventory revision, including an
event that repeats the previous size. A proposal records the current
revision. A later revision settles that proposal.

While a proposal is pending, the latest desired scene supplies its successor
target. The reconciler sends no second proposal until the first settles.
After rendering settlement, a changed target requests another manage
sequence. Closing a window removes its reconciliation state at the next
sequence boundary.

A window without settled dimensions remains explicitly hidden. An existing
selection retains its last settled size while a new proposal is pending.
This policy also applies when a previously hidden window becomes selected.

## Focus follows shown windows

Manage reconciliation focuses only a selection already shown by a render
sequence. It clears focus until a new selection can be shown. Rendering the
settled selection draws its focused border and requests another manage
sequence. The runtime sends `manage_dirty` after successful `render_finish`.

Exclusive layer focus suppresses all window focus requests. Non-exclusive
layer focus suppresses routine requests, while an interaction or an explicit
`windows` action reclaims focus. Layout messages alone do not reclaim it.
Each layer-focus report has a revision, so repeated `focus_none` reports
invalidate the last focus request. Layer ownership also controls border
colors.

## Historical real-river selection experiment

The 2026-10-17 experiment used river 0.5.0 with its headless backend and pixman
renderer. A scratch xdg-shell client delayed each resize response after its
first commit. Output capture compared two policies while switching from a
red 300 by 200 selection to a green selection previously settled at 600 by
400, with a new target of 900 by 600.

The immediate-selection variant displayed green at 600 by 400 during the
delay, then at 900 by 600 after settlement. The retained-selection variant
displayed red at 300 by 200 until green settled. Both variants completed
without a protocol error. Immediate selection keeps the visible selection
aligned with StackSet and avoids retaining an otherwise omitted window.

Captured pixels also verified border placement. A 300 by 200 content box at
100,100 with 5-pixel borders covered 310 by 210 pixels in total. The blue
border began at 95,95, outside the content rectangle.

The scratch client, policy, runner, logs, and captures remain in the ignored
local `artifacts/wp6/prototype` directory of the measuring checkout. They are
not part of the repository.

## Verification evidence

This section records the verification of 2026-10-17. Its scratch-client runs
are historical local evidence. The reproducible release suite and its
commands are in the [integration guide](integration-testing.md).

The socket-pair tests check exact phase requests, delayed and superseded
proposals, accepted size mismatches, omitted windows, layer focus, ID reuse,
and window retirement. Generated traces compare the emitted proposals,
visibility, and focus requests with an independent settlement model.
Generated scene tests cover Tall, Mirror Tall, and Full across two outputs,
including extreme coordinates, missing usable areas, and border insets.

The historical live Policy check used the same native scratch clients. Pixel
captures verified Tall and Full placement, focus switching, ratio changes, workspace
hiding and restoration, and resizing after a focused window closes. A new
keyboard event in the surviving client verified focus recovery. Its local runner
was `artifacts/wp6/prototype/live.py`; captures are in the adjacent
`policy-live` directory.

`bash ci/check-phase-types.sh` compiles valid plans and checks that the
compiler rejects manage requests in render plans and render requests in
manage plans. CI runs this check beside the ordinary test suite.

These local runs used GHC 9.6.7 through `-w ghc-9.6.7 --offline`, not the
pinned GHC 9.6.6 Nix environment that CI uses.
