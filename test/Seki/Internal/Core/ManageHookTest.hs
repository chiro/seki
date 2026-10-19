{-# LANGUAGE OverloadedStrings #-}

module Seki.Internal.Core.ManageHookTest (tests) where

import Data.Map.Strict qualified as Map
import Data.Monoid (Endo (..))
import Seki.Internal.Core.Geometry (OutputRect (..), Point (..), mkRect, mkSize)
import Seki.Internal.Core.ManageHook (
  ManageHook,
  WindowProps (..),
  appId,
  askWindow,
  composeAll,
  doF,
  doFloat,
  doShift,
  doSink,
  idHook,
  isDialog,
  noWindowProps,
  runQuery,
  title,
  (-->),
  (=?),
 )
import Seki.Internal.Core.StackSet (FloatGeom (..), StackSet (..), WindowId (..), float, insertUp, shiftWin, sink)
import Seki.Internal.Core.StackSetGen (NonEmptyWindowsStackSet (..), T, arbitraryTag, arbitraryWindow, attach)
import Test.QuickCheck (Gen, Property, arbitrary, elements, forAll, forAllShow, listOf, oneof, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "ManageHook"
    [ testGroup
        "queries"
        [ testCase "appId reads the app_id the window set" $
            runQuery appId w1 foot @?= "foot"
        , testCase "appId reads a missing app_id as empty, as className reads a missing WM_CLASS" $
            runQuery appId w1 noWindowProps @?= ""
        , testCase "a hook keyed on an app_id does not match a window with none" $
            runQuery (appId =? "foot") w1 noWindowProps @?= False
        , testCase "a hook keyed on an app_id matches only that app_id" $
            map (runQuery (appId =? "foot") w1) [foot, noWindowProps{propAppId = Just "footclient"}] @?= [True, False]
        , testCase "title reads the title, and a missing one as empty" $
            map (runQuery title w1) [noWindowProps{propTitle = Just "vim"}, noWindowProps] @?= ["vim", ""]
        , testCase "isDialog means the window has a parent, since river reports no window type" $
            map (runQuery isDialog w1) [noWindowProps{propParent = Just w2}, noWindowProps] @?= [True, False]
        ]
    , testGroup
        "combinators"
        [ testCase "--> runs the hook only when the predicate holds" $
            ( apply (appId =? "foot" --> doShift 3) w1 foot full
            , apply (appId =? "mpv" --> doShift 3) w1 foot full
            )
              @?= (shiftWin 3 w1 full, full)
        , testCase "composeAll applies its last hook first, so the first doShift wins" $
            apply (composeAll [doShift 2, doShift 3]) w1 foot full @?= shiftWin 2 w1 full
        , testCase "composeAll keeps matching hooks and skips the others in list order" $
            apply (composeAll [appId =? "mpv" --> doShift 2, appId =? "foot" --> doShift 3, doSink]) w1 foot floated
              @?= shiftWin 3 w1 (sink w1 floated)
        , testCase "doSink drops the window's floating rectangle" $
            Map.member w1 (floating (apply doSink w1 noWindowProps floated)) @?= False
        , testCase "doFloat floats the window with no stored box, so the client picks its size" $
            Map.lookup w1 (floating (apply doFloat w1 noWindowProps full)) @?= Just ClientSized
        , testCase "isDialog --> doFloat floats only a window with a parent" $
            map (\props -> Map.member w1 (floating (apply (isDialog --> doFloat) w1 props full))) [noWindowProps{propParent = Just w2}, noWindowProps] @?= [True, False]
        , testCase "doShift to an unknown tag changes nothing" $
            apply (doShift 99) w1 noWindowProps full @?= full
        , testCase "idHook and an empty composeAll change nothing" $
            (apply idHook w1 foot full, apply (composeAll []) w1 foot full) @?= (full, full)
        ]
    , testGroup
        "properties"
        [ testProperty "q =? x is (== x) over q" propEqQuery
        , testProperty "p --> h is h when p holds and idHook otherwise" propImplies
        , testProperty "composeAll is right-to-left composition of the Endos" propComposeAll
        , testProperty "idHook is the identity of composition" propIdentity
        , testProperty "doShift i is shiftWin i on the managed window" propDoShift
        , testProperty "doSink is sink on the managed window" propDoSink
        , testProperty "doFloat is a client-sized float of the managed window" propDoFloat
        ]
    ]

apply :: ManageHook i l -> WindowId -> WindowProps -> StackSet i l -> StackSet i l
apply hook w props = appEndo (runQuery hook w props)

foot :: WindowProps
foot = noWindowProps{propAppId = Just "foot"}

w1, w2 :: WindowId
w1 = WindowId 1
w2 = WindowId 2

-- | w1 focused on tag 1 of three, with one screen.
full :: T
full = insertUp w1 (attach 1 0 [1, 2, 3])

floated :: T
floated = float w1 rect full

rect :: FloatGeom
rect = FloatAt (OutputRect (either (error . show) id (mkSize 100 100 >>= mkRect (Point 10 10))))

genProps :: Gen WindowProps
genProps = do
  app <- elements [Nothing, Just "foot", Just "mpv"]
  parent <- elements [Nothing, Just w2]
  pure noWindowProps{propAppId = app, propParent = parent}

genHook :: T -> Gen (String, ManageHook Int Int)
genHook ss =
  oneof
    [ (\t -> ("doShift " ++ show t, doShift t)) <$> arbitraryTag ss
    , pure ("doSink", doSink)
    , pure ("idHook", idHook)
    , (\t -> ("appId =? \"foot\" --> doShift " ++ show t, appId =? "foot" --> doShift t)) <$> arbitraryTag ss
    , (\t -> ("isDialog --> doShift " ++ show t, isDialog --> doShift t)) <$> arbitraryTag ss
    , pure ("doF (float w rect)", doF . (`float` rect) =<< askWindow)
    , pure ("doFloat", doFloat)
    ]

-- | A StackSet with a window in it, a window to manage, and its properties.
data Managed = Managed T WindowId WindowProps
  deriving (Show)

genManaged :: Gen Managed
genManaged = do
  ne@(NonEmptyWindowsStackSet ss) <- arbitrary
  Managed ss <$> arbitraryWindow ne <*> genProps

withHooks :: (Managed -> [ManageHook Int Int] -> Property) -> Property
withHooks prop = forAll genManaged $ \m@(Managed ss _ _) ->
  forAllShow (listOf (genHook ss)) (show . map fst) (prop m . map snd)

propEqQuery :: Property
propEqQuery = forAll genProps $ \props -> forAll (elements ["foot", "mpv", ""]) $ \x ->
  runQuery (appId =? x) w1 props === (runQuery appId w1 props == x)

propImplies :: Bool -> Property
propImplies b = withHooks $ \(Managed ss w props) hooks ->
  let h = composeAll hooks
   in apply (pure b --> h) w props ss === (if b then apply h w props ss else ss)

propComposeAll :: Property
propComposeAll = withHooks $ \(Managed ss w props) hooks ->
  apply (composeAll hooks) w props ss === foldr (\h f -> apply h w props . f) id hooks ss

propIdentity :: Property
propIdentity = withHooks $ \(Managed ss w props) hooks ->
  let h = composeAll hooks
   in (apply (idHook <> h) w props ss, apply (h <> idHook) w props ss) === (apply h w props ss, apply h w props ss)

propDoShift :: Property
propDoShift = forAll genManaged $ \(Managed ss w props) -> forAll (arbitraryTag ss) $ \t ->
  apply (doShift t) w props ss === shiftWin t w ss

propDoSink :: Property
propDoSink = forAll genManaged $ \(Managed ss w props) ->
  apply doSink w props (float w rect ss) === sink w (float w rect ss)

propDoFloat :: Property
propDoFloat = forAll genManaged $ \(Managed ss w props) ->
  apply doFloat w props ss === float w ClientSized ss
