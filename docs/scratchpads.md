# Named terminal scratchpads

A scratchpad is one owned window selected by a query when it is first
managed. It floats on the current workspace when shown. Hiding removes it
from the workspace set entirely: normal workspace navigation, layout, focus,
bar occupancy and focused title cannot reach hidden storage.

Configure a distinct app ID for each terminal. This example uses only the
public library:

```haskell
import Seki

main :: IO ()
main = case mkSize 640 480 >>= mkRect (Point 80 60) of
  Left err -> fail (show err)
  Right rect -> seki $ additionalKeys
    def { scratchpads =
      [Scratchpad "term" "foot --app-id=seki-term"
        (appId =? "seki-term") (OutputRect rect)] }
    [(KeyBinding Mod4 (Keysym 0x60), toggleScratchpad "term")]
```

The rectangle is in logical pixels relative to the output showing the pad.
Existing floating rules clamp its placement inside the output and border,
keeping the stored geometry for a later larger output. Tall, Mirror and Full
do not tile the pad. Duplicate or blank names, blank commands and empty
rectangles are configuration errors, checked before connecting to river.

Use `sekictl scratchpad term` to open, hide or bring it. A hidden pad appears
on the current workspace. A shown pad on that workspace hides; a pad shown
elsewhere moves to the current workspace and output. Hide and bring leave
fullscreen. Hide restores the remembered focus if that window is still
eligible on the workspace; otherwise the normal workspace stack chooses.
Closing the owned window allows the next toggle to launch once again.

Definitions are examined in configuration order. The first matching query
wins, even if that definition already owns another window. The first eligible
creation event becomes its owner and bypasses the user `manageHook`.
Unmatched windows and extra matches use the ordinary `manageHook`; extra
matches remain ordinary usable windows and log one ambiguity diagnostic per
definition per WM session. An overlapping later definition does not take an
extra window from an earlier matching definition. Queries use the existing
`appId` and `title` semantics, including the empty string for absent values.
Later title or app-ID changes never rematch a managed window. Launch PIDs
are not used to identify windows.

The first absent toggle launches the shell command. Further toggles while
pending coalesce. An unresolved attempt stays `pending` until a matching
window arrives or you explicitly reset it. It never launches again merely
because a command is slow or the shell exited. A late first matching new
window can still resolve the pending attempt.

After cleaning up the old command yourself, deliberately run
`sekictl scratchpad-reset term` or the typed `resetScratchpad "term"` action.
The next toggle may launch a fresh attempt. Reset does not kill a process or
prove that arbitrary detached descendants have exited; cleaning them up is
the operator's responsibility. Reset changes unresolved launches only and
leaves an owned shown or hidden window intact. Ordinary shell spawning keeps
its existing detached semantics, including its stderr failure reporting.

IPC command receipts identify the reducer's applied revision. A successful
toggle receipt can mean the launch is now pending; it does not mean the
application mapped or a frame displayed. Snapshot states are `pending`,
`shown` and `hidden`; an absent name has no lifecycle entry. Existing IPC v1
operations and receipt/error contracts remain intact.

Restart schema v2 saves owned windows by river identifier, their visibility,
float geometry and eligible prior focus. It accepts v1 files with no pad
ownership. Changing a query or command does not rematch an owned restored
identifier. Saved valid geometry survives configuration changes. A valid
explicit box stored with the pad wins over an absent or conflicting ordinary
float copy. Otherwise restore keeps the ordinary float state, including
a pad deliberately sunk into tiling; showing it again reapplies the configured
box. Removing a definition releases its hidden surviving window to an ordinary
user workspace as a float; missing outputs use the existing workspace fallback. Closed or
unidentified windows cannot restore ownership. A removed definition never
leaves an inaccessible hidden window behind.

Pending attempts are not persisted as processes or windows.
After WM restart an unmatched definition is absent and can launch again.
There is no exactly-once external-process guarantee across restart or manual
reset. Use a query that uniquely names the intended window and account for
any still-starting detached command when restarting or recovering.
