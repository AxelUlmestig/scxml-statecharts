{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

-- | The engine generated charts run on. The @scxml@ quasiquoter generates one
-- 'Fsm' instance per layer of the chart: one for @FsmState@, one for each
-- compound state's type, and one for each region of a parallel state. Each
-- instance only knows its own children, and 'applyToSubstates' is how the
-- functions here reach the layer below.
--
-- The instances name their callbacks with generated enumerations rather than
-- running them, which keeps this class free of the callbacks' monad. The
-- generated functions map those names to the user's callbacks, so the monad
-- is inferred from the callbacks and never mentioned in an instance.
module Scxml.Statechart.Fsm
  ( Fsm (..)
  , FsmTransition (..)
  , FsmEntry (..)
  , initiate
  , consumeEvents
  , entryAction
  , exitAction
  ) where

import           Control.Monad (when)
import           Data.Foldable (traverse_)
import           Data.Maybe    (catMaybes, maybeToList)
import           Data.Monoid   (All (..))


-- | One step, as the engine reports it. The generated code converts it to the
-- chart's own @FsmTransition@ before a callback sees it, so that a chart needs
-- no imports.
data FsmTransition state event = FsmTransition
  { transitionFrom  :: state
  , transitionTo    :: state
  , transitionEvent :: event
  }

-- | What an entry callback runs in: starting the chart, or a step.
data FsmEntry state event = FsmStart state | FsmStep (FsmTransition state event)

-- | Marks an entry callback, which returns the event it raises, if any.
-- Raised events are queued and processed before the current step returns,
-- like SCXML's @<raise>@. Only here so that a callback with the wrong type
-- gets an error pointing at it.
entryAction :: m (Maybe ev) -> m (Maybe ev)
entryAction = id

-- | Marks an exit callback, which may not raise events. Only here so that a
-- callback with the wrong type gets an error pointing at it.
exitAction :: m () -> m ()
exitAction = id

-- | One layer of a chart. The generated instances live in the chart's module,
-- so that module needs @TypeFamilies@ for the associated types, which the
-- quasiquoter checks for.
class Fsm state event | state -> event where
  type EntryAction state
  type ExitAction state
  initialState      :: state
  nextState         :: event -> state -> Maybe state
  onEntry           :: state -> [EntryAction state]   -- e.g. data FsmEntryAction = FetchStatus | SendReport
  onExit            :: state -> [ExitAction state]
  isFinal           :: state -> Bool
  doneEvent         :: state -> Maybe event
  applyToSubstates  ::
    Monoid result =>
    state ->
    (
      forall substate.
      (Fsm substate event
      , EntryAction state ~ EntryAction substate
      , ExitAction state ~ ExitAction substate
      ) =>
      substate ->
      (substate, result)
    ) ->
    (state, result)

data ConsumeEventResults event entryAction exitAction = ConsumeEventResults
  { eventConsumed            :: Bool
  , onEntryActions           :: [entryAction]
  , onExitActions            :: [exitAction]
  , parallelStatesDoneEvents :: [event]
  }


instance Semigroup (ConsumeEventResults event entryAction exitAction) where
  (<>) cer1 cer2 =
    let ConsumeEventResults eventConsumed1 onEntryActions1 onExitActions1 events1 = cer1
        ConsumeEventResults eventConsumed2 onEntryActions2 onExitActions2 events2 = cer2
    in ConsumeEventResults
      { eventConsumed = eventConsumed1 || eventConsumed2
      , onEntryActions = onEntryActions1 <> onEntryActions2
      , onExitActions = onExitActions1 <> onExitActions2
      , parallelStatesDoneEvents = events1 <> events2
      }

instance Monoid (ConsumeEventResults events entryAction exitAction) where
  mempty = ConsumeEventResults { eventConsumed = False, onEntryActions = [], onExitActions = [], parallelStatesDoneEvents = [] }

-- | initiate a new state machine
initiate ::
  (Monad m, Fsm state event) =>
  (EntryAction state -> FsmEntry state event -> m (Maybe event)) ->
  (ExitAction state -> FsmTransition state event -> m ()) ->
  m state
initiate entry exit = do
  let state = initialState

  onentryEvents <- catMaybes <$> traverse (flip entry (FsmStart state)) (getOnEntryFunctions state)

  -- check if any state or substate are immediately 'done' (all children are in 'final')
  let stateDoneEvents = getAllDoneEvents state

  consumeEvents entry exit (onentryEvents <> stateDoneEvents) state


-- | provide events for a state machine to act on
consumeEvents ::
  (Monad m, Fsm state event) =>
  (EntryAction state -> FsmEntry state event -> m (Maybe event)) ->
  (ExitAction state -> FsmTransition state event -> m ()) ->
  [event] ->
  state ->
  m state
consumeEvents = go 1000
  where
    -- same signature as top level but with added Int for the loop counter
    go :: (Monad m, Fsm state event) => Int -> (EntryAction state -> FsmEntry state event -> m (Maybe event)) -> (ExitAction state -> FsmTransition state event -> m ()) -> [event] -> state -> m state
    go remainingAttempts entry exit events originalState = do
      when (remainingAttempts < 0) $ error "The state machine got stuck in an infinite loop"

      case events of
        [] -> pure originalState
        (event : remainingEvents) -> do
          -- purely move the state machine with one event and gather the effects that it causes
          let (newState, ConsumeEventResults { onEntryActions, onExitActions, parallelStatesDoneEvents }) = moveStateMachine event originalState

          -- run the effects of the onentry and onexit callbacks
          let transitionInfo =
                FsmTransition
                { transitionFrom  = originalState
                , transitionTo    = newState
                , transitionEvent = event
                }

          traverse_ (flip exit transitionInfo) onExitActions
          newEvents <- catMaybes <$> traverse (flip entry (FsmStep transitionInfo)) onEntryActions

          -- run the state machine again with the events from the onentry callbacks and parallel states being all done
          go (remainingAttempts - 1) entry exit (remainingEvents <> newEvents <> parallelStatesDoneEvents) newState

    moveStateMachine :: Fsm state event => event -> state -> (state, ConsumeEventResults event (EntryAction state) (ExitAction state))
    moveStateMachine event state =
      let (state', consumeEventResults) = applyToSubstates state (moveStateMachine event)
      -- if the substates consumed the event then this state won't act on the event
      in if eventConsumed consumeEventResults then
        -- the substates moved so we need to check if we need to emit a 'done' event
        let stateDoneEvent = getStateDoneEvent state'
            consumeEventResults' = consumeEventResults { parallelStatesDoneEvents = parallelStatesDoneEvents consumeEventResults <> stateDoneEvent }
        in (state', consumeEventResults')
      else
        case nextState event state of
          Nothing -> (state, mempty)
          Just newState ->
            -- if all the substate of the new state are `final` then we get the `doneEvent` of the current state
            let cer = ConsumeEventResults True (getOnEntryFunctions newState) (getOnExitFunctions state) (getAllDoneEvents newState)
            in (newState, cer)


getAllDoneEvents :: Fsm state event => state -> [event]
getAllDoneEvents state =
  let (_, events) = applyToSubstates state (\substate -> (substate, getAllDoneEvents substate))
  in events <> getStateDoneEvent state

-- | check if all the immediate substates are `final`, then get this state's done event if that's the case
getStateDoneEvent :: Fsm state event => state -> [event]
getStateDoneEvent state =
  let (_, allSubStatesFinal) = applyToSubstates state (\substate -> (substate, Just (All (isFinal substate))))
  in case allSubStatesFinal of
    Just (All True) -> maybeToList $ doneEvent state
    _               -> []


getOnEntryFunctions :: Fsm state event => state -> [EntryAction state]
getOnEntryFunctions state =
  let (_, substateOnEntry) = applyToSubstates state (\substate -> (substate, getOnEntryFunctions substate))
  in onEntry state <> substateOnEntry


getOnExitFunctions :: Fsm state event => state -> [ExitAction state]
getOnExitFunctions state =
  let (_, substateOnExit) = applyToSubstates state (\substate -> (substate, getOnExitFunctions substate))
  in substateOnExit <> onExit state
