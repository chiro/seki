module Main (main) where

import ConfigMain (customConfig, fullConfig, mediaConfig, raiseVolume, tolerantConfig)
import Control.Monad (unless)
import Seki
import Seki.Core.StackSet (differentiate, integrate, integrate')
import System.Exit (exitFailure)

main :: IO ()
main = do
  check "negative size is rejected" (either Just (const Nothing) (mkSize (-1) 0) == Just (NegativeSize (-1) 0))
  case mkSize 1001 10 >>= mkRect (Point (-1920) 0) of
    Left err -> failWith (show err)
    Right rect -> do
      let (left, right) = splitHorizontallyBy (1 / 2) rect
      check "ratio split widths" ((rectWidth left, rectWidth right) == (500, 501))
  check "custom layout configuration" (description (layoutHook customConfig) == "Custom")
  check "changed layout type" (description (layoutHook fullConfig) == "Full")
  check "differentiate focuses the first element" (differentiate [1, 2, 3 :: Int] == Just (Stack 1 [] [2, 3]))
  check "integrate lists a stack from the top" (integrate (Stack 2 [1] [3 :: Int]) == [1, 2, 3])
  check "integrate' of an empty list is empty" (null (integrate' (differentiate ([] :: [Int]))))
  check "the first layout is selected" (description layouts == "Tall")
  check "NextLayout selects the next layout" (fmap description (pureMessage layouts (SomeMessage NextLayout)) == Just "Mirror Tall")
  check "a modifier-less media key is bound" (raiseVolume `elem` bound mediaConfig)
  check "the media key joins the default bindings" (length (bound mediaConfig) == length (bound def) + 1)
  check "removeKeys removes the media key" (raiseVolume `notElem` bound (removeKeys mediaConfig [raiseVolume]))
  check "the default layouts crop" (sizePolicy (layoutHook def) == Crop)
  check "tolerant layouts tolerate" (sizePolicy (layoutHook tolerantConfig) == Tolerate)
  check "tolerant keeps the layout description" (description (layoutHook tolerantConfig) == "Tall")
  check "def moves with mod and button 1 and resizes with mod and button 3" (pointerBindings def def == [(PointerBinding Mod4 Button1, MoveWindow), (PointerBinding Mod4 Button3, ResizeWindow)])
  check "a pointer binding joins the defaults" (map fst (pointerBindings customConfig customConfig) == [PointerBinding (Mod4 .|. Shift) Button1, PointerBinding Mod4 Button1, PointerBinding Mod4 Button3])
 where
  check label ok = unless ok (failWith label)
  failWith message = putStrLn ("public API check failed: " <> message) >> exitFailure
  bound config = map fst (keys config config)
  layouts = Tall 1 (3 / 100) (1 / 2) ||| Mirror (Tall 1 (3 / 100) (1 / 2)) ||| Full
