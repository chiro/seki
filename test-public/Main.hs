module Main (main) where

import Control.Monad (unless)
import Seki.Core.Geometry (GeometryError (..), Point (..), mkRect, mkSize, rectWidth, splitHorizontallyBy)
import Seki.Core.Layout (ChangeLayout (..), LayoutClass (..), Mirror (..), SomeMessage (..), (|||))
import Seki.Core.StackSet (Stack (..), StackSet, differentiate, focusDown, greedyView, integrate, integrate', swapMaster)
import Seki.Layout.Full (Full (..))
import Seki.Layout.Tall (Tall (..))
import System.Exit (exitFailure)

main :: IO ()
main = do
  check "negative size is rejected" (either Just (const Nothing) (mkSize (-1) 0) == Just (NegativeSize (-1) 0))
  case mkSize 1001 10 >>= mkRect (Point (-1920) 0) of
    Left err -> failWith (show err)
    Right rect -> do
      let (left, right) = splitHorizontallyBy (1 / 2) rect
      check "ratio split widths" ((rectWidth left, rectWidth right) == (500, 501))
  check "StackSet operations compose" (step `seq` True)
  check "differentiate focuses the first element" (differentiate [1, 2, 3 :: Int] == Just (Stack 1 [] [2, 3]))
  check "integrate lists a stack from the top" (integrate (Stack 2 [1] [3 :: Int]) == [1, 2, 3])
  check "integrate' of an empty list is empty" (null (integrate' (differentiate ([] :: [Int]))))
  check "the first layout is selected" (description layouts == "Tall")
  check "NextLayout selects the next layout" (fmap description (pureMessage layouts (SomeMessage NextLayout)) == Just "Mirror Tall")
 where
  check label ok = unless ok (failWith label)
  failWith message = putStrLn ("public API check failed: " <> message) >> exitFailure
  step :: StackSet String () -> StackSet String ()
  step = greedyView "2" . focusDown . swapMaster
  layouts = Tall 1 (3 / 100) (1 / 2) ||| Mirror (Tall 1 (3 / 100) (1 / 2)) ||| Full
