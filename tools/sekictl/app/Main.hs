module Main (main) where

import Seki.Run.IPCClient (runClient)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runClient
