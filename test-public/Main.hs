module Main (main) where

import Control.Monad (unless)
import Seki.Core.Geometry (GeometryError (..), Point (..), mkRect, mkSize, rectWidth, splitHorizontallyBy)
import System.Exit (exitFailure)

main :: IO ()
main = do
  check "negative size is rejected" (either Just (const Nothing) (mkSize (-1) 0) == Just (NegativeSize (-1) 0))
  case mkSize 1001 10 >>= mkRect (Point (-1920) 0) of
    Left err -> failWith (show err)
    Right rect -> do
      let (left, right) = splitHorizontallyBy (1 / 2) rect
      check "ratio split widths" ((rectWidth left, rectWidth right) == (500, 501))
 where
  check label ok = unless ok (failWith label)
  failWith message = putStrLn ("public API check failed: " <> message) >> exitFailure
