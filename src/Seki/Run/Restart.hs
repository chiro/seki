{-# LANGUAGE LambdaCase #-}

-- | Replacing the running seki with the executable it started from (M2-D06).
module Seki.Run.Restart (
  RestartRefusal (..),
  describeRefusal,
) where

import Seki.Wire.Transport (ConnectionDiscoveryError)

-- | Why a restart was refused before @stop@. seki keeps running.
data RestartRefusal
  = -- | Neither argv[0] nor the running executable gave a path at startup.
    TargetUnrecorded
  | TargetMissing FilePath
  | TargetNotRegularFile FilePath
  | TargetNotExecutable FilePath
  | -- | No socket path to reconnect to, so neither the new process nor a fallback could connect.
    NoReconnectTarget ConnectionDiscoveryError
  | -- | @XDG_RUNTIME_DIR@ is unset or relative, so the state has nowhere to go.
    NoStateDirectory
  deriving (Eq, Show)

describeRefusal :: RestartRefusal -> String
describeRefusal = \case
  TargetUnrecorded -> "no executable path was recorded at startup"
  TargetMissing path -> path <> " does not exist"
  TargetNotRegularFile path -> path <> " is not a regular file"
  TargetNotExecutable path -> path <> " is not executable"
  NoReconnectTarget e -> "no Wayland socket path to reconnect to: " <> show e
  NoStateDirectory -> "XDG_RUNTIME_DIR is unset or not absolute"
