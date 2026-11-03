# Configure input devices

Import `Seki` (or `Seki.Input` for the rule types). Patterns match the entire,
case-sensitive device name. `*` matches any sequence and `?` one character.
Backslash escapes only `*`, `?`, or backslash. Patterns longer than 256 characters
and malformed escapes return `Left`; handle that error explicitly.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Seki

main :: IO ()
main = case compileDevicePattern "*keyboard*" of
  Left err -> fail (show err)
  Right name -> seki def
    { inputRules = [InputRule name (Just Keyboard) [RepeatInfo 37 410]] }
```

Matching rules run in declaration order; each later specified setting replaces
that field without discarding other fields. `Nothing` matches every device type.
The default is `[]`, preserving compositor defaults and existing window behavior.
All settings are validated before connecting. Invalid numeric values, matrices,
curve bounds or selectors fail configuration validation; `validateInputSetting`
also lets configuration builders check values explicitly. Missing optional
protocols or unsupported device capabilities produce results in IPC instead of
stopping the window manager. Keymap path loading is asynchronous and failures
remain per-setting results. See [the IPC contract](ipc.md) for evidence levels.

Custom acceleration functions accept 2–64 finite, nonnegative points and a positive
finite step. This bound matches native libinput config acceptance; an earlier
4096-point prototype exceeded river's 4096-byte message limit. Layout names must
fit 4083 UTF-8 bytes, allowing the encoded request to fit that limit. Local keymap
load failures remain unavailable for the unchanged path/facet/target. Appearing
files are not automatically retried; restart with the corrected configuration or
change the requested path. This prevents repeated background loads and wake loops.
