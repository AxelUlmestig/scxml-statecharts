-- | Typed statecharts generated from SCXML.
--
-- @
-- [scxml|
-- <scxml name="Door" initial="Closed">
--   <state id="Closed">
--     <onentry><script>lockBolt</script></onentry>
--     <transition event="Open" target="Opened"/>
--   </state>
--   <state id="Opened"><transition event="Close" target="Closed"/></state>
-- </scxml>
-- |]
--
-- doorStart :: MonadIO m => m DoorState
-- doorStep  :: MonadIO m => DoorState -> DoorEvent -> m DoorState
--
-- lockBolt :: MonadIO m => FsmState -> Maybe FsmEvent -> m (Maybe FsmEvent)
-- lockBolt _ _ = liftIO (putStrLn "clunk") >> pure Nothing
-- @
--
-- The quasiquote generates @FsmState@, @FsmEvent@, a value
-- @fsmChart :: t'Def' FsmState FsmEvent@ and the functions
-- @initiateStateMachine@ and @notifyStateMachine@, which call the callbacks
-- named in @<script>@ elements. State ids and event names are used verbatim as
-- constructor names, so the generated names are fixed and a module holds one
-- chart. Callbacks are defined in the same module, after the quasiquote, and
-- must all live in the same monad, which the type checker enforces.
--
-- Signatures for the two generated functions are optional. They are inferred
-- when the callbacks are in a concrete monad; when the callbacks are
-- polymorphic, @initiateStateMachine@ takes no arguments and so needs either
-- a signature or @NoMonomorphismRestriction@.
--
-- See "Statechart.TH" for exactly what is generated, and "Statechart.Run" for
-- running a chart without the generated functions.
module Statechart
  ( -- * Defining charts
    -- | The quasiquoter, and the record it generates.
    scxml
  , Def (..)
    -- * Storing a state
    -- | Every generated type derives @Show@ and @Read@, which round-trip
    -- exactly and are good for tests and debugging. For a state that outlives
    -- the process, prefer these: the id list is portable, readable, and
    -- rejects a value stored before the chart changed.
  , toStateIds
  , fromStateIds
    -- * Running charts without the generated functions
    -- | The generated functions cover the normal case. These are for tests
    -- and tooling that need the transition structure without the callbacks.
  , initialState
  , stepPure
    -- * The untyped model, for tooling
    -- | Enough to render a chart as a diagram, or compare it with a
    -- definition held elsewhere.
  , Chart (..)
  , StateId
  ) where

import Statechart.Def
import Statechart.Model (Chart (..), StateId)
import Statechart.Run
import Statechart.TH (scxml)
