{-# LANGUAGE QuasiQuotes #-}
-- | Completion without any callbacks: a compound state whose initial child is
-- final completes as soon as the chart starts, and one event can complete
-- several regions of a parallel state at once.
module Completion where

import Scxml.Statechart (scxml)

[scxml|
<scxml initial="Boot">
  <state id="Boot" initial="Ready">
    <final id="Ready"/>
    <transition event="done.state.Boot" target="Work"/>
  </state>
  <parallel id="Work">
    <state id="Left" initial="L1">
      <state id="L1"><transition event="Next" target="L2"/></state>
      <final id="L2"/>
    </state>
    <state id="Right" initial="R1">
      <state id="R1"><transition event="Next" target="R2"/></state>
      <final id="R2"/>
    </state>
    <transition event="done.state.Work" target="Done"/>
  </parallel>
  <final id="Done"/>
</scxml>
|]

initiateStateMachine :: IO FsmState
notifyStateMachine :: FsmState -> FsmEvent -> IO FsmState

-- | Checks to run, as (label, expected, actual) triples.
spec :: IO [(String, String, String)]
spec = do
  started <- initiateStateMachine
  done <- notifyStateMachine started Next
  pure
    [ ("a state that is complete on entry completes while the chart starts", show (Work L1 R1), show started)
    , ("one event moves every region that handles it, completing the parallel state", show Done, show done)
    ]
