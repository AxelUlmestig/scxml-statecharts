{-# LANGUAGE QuasiQuotes #-}
-- | A chart whose initial states are deliberately not written first, to pin
-- what that does to the generated types. The model keeps a compound state's
-- children with the initial one first, so a compound state cannot name an
-- initial child that is not its own. That order shows up in the generated
-- constructors, and therefore in derived 'Ord'.
-- A chart module should not use an explicit export list, or it will get
-- unused-binding warnings for the generated functions it does not call.
module Reordered where

import Statechart (scxml)

[scxml|
<scxml initial="Job">
  <final id="Done"/>
  <state id="Job" initial="Third">
    <state id="First"><transition event="Go" target="Third"/></state>
    <state id="Second"/>
    <state id="Third"><transition event="Back" target="First"/></state>
  </state>
</scxml>
|]

-- Generated, with the initial child first in each:
--
--   data FsmState = Job Job | Done
--   data Job      = Third | First | Second

initiateStateMachine :: IO FsmState
notifyStateMachine :: FsmState -> FsmEvent -> IO FsmState

-- | Checks to run, as (label, expected, actual) triples.
spec :: IO [(String, String, String)]
spec = do
  started <- initiateStateMachine
  back <- notifyStateMachine started Back
  forth <- notifyStateMachine back Go
  pure
    [ ("the initial child is entered even when written last", show (Job Third), show started)
    , ("transitions between siblings still work", show (Job First), show back)
    , ("and back again", show (Job Third), show forth)
    , -- Third is the initial child so it is the first constructor, which makes
      -- it compare less than the states written before it in the XML.
      ("Ord follows the initial-first constructor order", show GT, show (compare (Job First) (Job Third)))
    , ("Done sorts after Job, though written before it", show LT, show (compare (Job Second) Done))
    , ("serializing is unaffected by the reordering", show ["Job", "Third"], show (serializeStateMachine started))
    ]
