module Seki.Run.SequenceTest (tests) where

import Control.Monad (forM_)
import Seki.Run.Sequence (
  RiverSequenceCompletion (..),
  RiverSequenceError (..),
  RiverSequenceOperation (..),
  RiverSequencePhase (..),
  RiverSequenceState,
  completeRiverSequence,
  initialRiverSequenceState,
  managementChangesAllowed,
  renderingChangesAllowed,
  riverSequencePhase,
  startManageSequence,
  startRenderSequence,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "river sequences"
    [ testCase "accepts the complete manage and repeated-render lifecycle" testLifecycle
    , testCase "exposes management and rendering capabilities by phase" testCapabilities
    , testCase "rejects manage_start in every invalid phase" testInvalidManageStarts
    , testCase "rejects render_start in every invalid phase" testInvalidRenderStarts
    , testCase "rejects completion when no sequence is active" testInvalidCompletions
    ]

testLifecycle :: Assertion
testLifecycle = do
  initial <- pure initialRiverSequenceState
  assertPhase AwaitingInitialManage initial

  managing <- expectRight (startManageSequence initial)
  assertPhase Managing managing
  (manageCompletion, requiredRender) <- expectRight (completeRiverSequence managing)
  assertEqual "manage completion request" SendManageFinish manageCompletion
  assertPhase AwaitingRequiredRender requiredRender

  rendering <- expectRight (startRenderSequence requiredRender)
  assertPhase Rendering rendering
  (renderCompletion, nextSequence) <- expectRight (completeRiverSequence rendering)
  assertEqual "render completion request" SendRenderFinish renderCompletion
  assertPhase AwaitingManageOrRender nextSequence

  repeatedRender <- expectRight (startRenderSequence nextSequence)
  (_, afterRepeatedRender) <- expectRight (completeRiverSequence repeatedRender)
  assertPhase AwaitingManageOrRender afterRepeatedRender

  nextManage <- expectRight (startManageSequence afterRepeatedRender)
  assertPhase Managing nextManage

testCapabilities :: Assertion
testCapabilities = do
  states <- reachableStates
  forM_ states $ \(phase, state) -> do
    assertEqual
      (show phase <> " management capability")
      (phase == Managing)
      (managementChangesAllowed state)
    assertEqual
      (show phase <> " rendering capability")
      (phase == Managing || phase == Rendering)
      (renderingChangesAllowed state)

testInvalidManageStarts :: Assertion
testInvalidManageStarts = do
  states <- reachableStates
  forM_ states $ \(phase, state) ->
    if phase == AwaitingInitialManage || phase == AwaitingManageOrRender
      then assertBool (show phase <> " accepts manage_start") (isRight (startManageSequence state))
      else
        expectLeft
          (InvalidRiverSequenceTransition phase StartManageSequence)
          (startManageSequence state)

testInvalidRenderStarts :: Assertion
testInvalidRenderStarts = do
  states <- reachableStates
  forM_ states $ \(phase, state) ->
    if phase `elem` [AwaitingInitialManage, AwaitingRequiredRender, AwaitingManageOrRender]
      then assertBool (show phase <> " accepts render_start") (isRight (startRenderSequence state))
      else
        expectLeft
          (InvalidRiverSequenceTransition phase StartRenderSequence)
          (startRenderSequence state)

testInvalidCompletions :: Assertion
testInvalidCompletions = do
  states <- reachableStates
  forM_ states $ \(phase, state) ->
    if phase == Managing || phase == Rendering
      then assertBool (show phase <> " accepts completion") (isRight (completeRiverSequence state))
      else
        expectLeft
          (InvalidRiverSequenceTransition phase CompleteActiveSequence)
          (completeRiverSequence state)

reachableStates :: IO [(RiverSequencePhase, RiverSequenceState)]
reachableStates = do
  let initial = initialRiverSequenceState
  managing <- expectRight (startManageSequence initial)
  (_, requiredRender) <- expectRight (completeRiverSequence managing)
  rendering <- expectRight (startRenderSequence requiredRender)
  (_, nextSequence) <- expectRight (completeRiverSequence rendering)
  pure
    [ (AwaitingInitialManage, initial)
    , (Managing, managing)
    , (AwaitingRequiredRender, requiredRender)
    , (Rendering, rendering)
    , (AwaitingManageOrRender, nextSequence)
    ]

assertPhase :: RiverSequencePhase -> RiverSequenceState -> Assertion
assertPhase expected = assertEqual "phase" expected . riverSequencePhase

isRight :: Either left right -> Bool
isRight value = case value of
  Left _ -> False
  Right _ -> True

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight = either (assertFailure . ("unexpected error: " <>) . show) pure

expectLeft :: (Eq errorValue, Show errorValue) => errorValue -> Either errorValue value -> Assertion
expectLeft expected result = case result of
  Left actual -> assertEqual "error" expected actual
  Right _ -> assertFailure "expected error"
