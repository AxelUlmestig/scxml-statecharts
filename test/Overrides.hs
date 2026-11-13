{-# LANGUAGE QuasiQuotes #-}
-- | Two behaviours nothing else covers: a transition on an enclosing state
-- acting as a default that an inner state overrides, and several entry
-- callbacks on one state each raising an event.
module Overrides where

import Control.Monad.Trans.State.Strict (StateT, modify', runStateT)
import Scxml.Statechart (scxml)

[scxml|
<scxml initial="Outer">
  <state id="Outer" initial="Inner">
    <state id="Inner">
      <!-- Overrides Outer's Poke while Inner is active. -->
      <transition event="Poke" target="Middle"/>
    </state>
    <state id="Middle">
      <onentry><script>noteEntry</script><script>raiseYes</script><script>raiseNo</script></onentry>
      <transition event="Yes" target="Yeah"/>
      <transition event="No" target="Nope"/>
    </state>
    <state id="Yeah"/>
    <state id="Nope"/>
    <!-- The default, taken wherever nothing inner handles Poke. -->
    <transition event="Poke" target="Away"/>
  </state>
  <state id="Away"/>
</scxml>
|]

initiateStateMachine :: StateT [String] IO FsmState
notifyStateMachine :: FsmState -> FsmEvent -> StateT [String] IO FsmState

noteEntry, raiseYes, raiseNo :: FsmEntry -> StateT [String] IO (Maybe FsmEvent)
-- A record field per thing the step has, so a callback reads only what it uses.
noteEntry (FsmStart s) = modify' (++ ["entered " ++ show s ++ " at start"]) >> pure Nothing
noteEntry (FsmStep step) = modify' (++ ["entered " ++ show (transitionTo step) ++ " from " ++ show (transitionFrom step)]) >> pure Nothing
raiseYes _ = modify' (++ ["raising Yes"]) >> pure (Just Yes)
raiseNo _ = modify' (++ ["raising No"]) >> pure (Just No)

-- | Checks to run, as (label, expected, actual) triples.
spec :: IO [(String, String, String)]
spec = do
  -- Poke while Inner is active: the inner transition wins, so we do not leave
  -- Outer. Middle's three entry callbacks all run, and the first event raised
  -- decides where we land; the second arrives in Yeah, which ignores it.
  (afterPoke, lg) <- runStateT (initiateStateMachine >>= \s -> notifyStateMachine s Poke) []
  -- Poke again from Yeah, which has no Poke of its own, so Outer's applies.
  (afterAgain, _) <- runStateT (notifyStateMachine afterPoke Poke) []
  pure
    [ ("an inner transition overrides the enclosing default", show (Outer Yeah), show afterPoke)
    , ("every entry callback runs, in document order"
      , show ["entered Outer Middle from Outer Inner", "raising Yes", "raising No"]
      , show lg
      )
    , ("the enclosing default applies where nothing inner handles the event"
      , show Away
      , show afterAgain
      )
    ]
