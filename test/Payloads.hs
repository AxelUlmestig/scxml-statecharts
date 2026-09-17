{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- | Events that carry data. The @event@ attribute names the event and then the
-- Haskell types its constructor holds, so @event="Order Item Int"@ declares
-- @Order Item Int@. Selection still happens on the name alone: the payload
-- goes to the callbacks and never decides which transition fires.
module Payloads where

import Control.Monad.Trans.State.Strict (StateT, modify', runStateT)
import Data.Text (Text)
import qualified Data.Text as T

import Scxml.Statechart (scxml)

[scxml|
<scxml initial="Idle">
  <state id="Idle">
    <onentry><script>arrive</script></onentry>
    <!-- Items is a type alias: a list cannot be written in the attribute,
         which separates one payload field from the next by a space. -->
    <transition event="Order Items Int" target="Checking"/>
  </state>

  <state id="Checking">
    <!-- Reads the payload of the event that got here and decides by raising
         one of the two events leading out, one of which carries data too. -->
    <onentry><script>check</script></onentry>
    <transition event="Ok" target="Shipping"/>
    <transition event="Reject Reason" target="Refused"/>
  </state>

  <state id="Shipping" initial="Packing">
    <state id="Packing">
      <!-- A module-qualified payload type. -->
      <transition event="Ship Data.Text.Text" target="Sent"/>
    </state>
    <final id="Sent"/>
    <transition event="done.state.Shipping" target="Idle"/>
  </state>

  <state id="Refused">
    <onentry><script>note</script></onentry>
    <!-- The same event again, declared with the same payload. Both
         transitions reach the one Order constructor. -->
    <transition event="Order Items Int" target="Checking"/>
  </state>
</scxml>
|]

-- Generated:
--
--   data FsmState = Idle | Checking | Shipping Shipping | Refused
--   data Shipping = Packing | Sent
--   data FsmEvent = Order Items Int | Ok | Reject Reason | Ship Text
--                 | DoneShipping
--
-- An event carries data, so FsmEvent derives Show, Read and Eq only: Enum and
-- Bounded need every constructor nullary, and Ord would demand an instance of
-- every payload type.

type M = StateT [Text] IO

initiateStateMachine :: M FsmState
notifyStateMachine :: FsmState -> FsmEvent -> M FsmState

-- The payload types, written after the quasiquote: like callback names, they
-- are resolved once the generated declarations are spliced in.
newtype Item = Item Text
  deriving (Show, Read, Eq)

-- A payload type is one type constructor, so a list gets a name of its own.
type Items = [Item]

newtype Reason = Reason Text
  deriving (Show, Read, Eq)

arrive, check, note :: FsmState -> Maybe FsmEvent -> M (Maybe FsmEvent)
arrive _ ev = do
  say ("idle after " <> maybe "start" (T.pack . show) ev)
  pure Nothing

check _ ev = case ev of
  Just (Order items n)
    | n > 0 -> do
        say ("checking " <> T.pack (show n) <> " x " <> named items)
        pure (Just Ok)
    | otherwise -> pure (Just (Reject (Reason ("nothing ordered of " <> named items))))
  _ -> pure (Just (Reject (Reason "no order")))
  where
    named items = T.intercalate " + " [what | Item what <- items]

note _ ev = do
  say (case ev of Just (Reject (Reason why)) -> "refused: " <> why; _ -> "refused")
  pure Nothing

say :: Text -> M ()
say t = modify' (++ [t])

-- | Start the chart and feed events. Returns the final state and the log.
run :: [FsmEvent] -> IO (FsmState, [Text])
run evs = runStateT (initiateStateMachine >>= go evs) []
  where
    go [] s = pure s
    go (e : rest) s = notifyStateMachine s e >>= go rest

-- | Checks to run, as (label, expected, actual) triples.
spec :: IO [(String, String, String)]
spec = do
  (accepted, acceptedLog) <- run [Order [Item "book"] 2]
  (refused, refusedLog) <- run [Order [Item "book"] 0]
  (shipped, shippedLog) <- run [Order [Item "book"] 2, Ship "trk-1"]
  (again, _) <- run [Order [Item "book"] 0, Order [Item "pen"] 1]
  pure
    [ ("a payload reaches the entry callback of the state the event causes"
      , show ["idle after start", "checking 2 x book" :: Text]
      , show acceptedLog
      )
    , ("and decides what that callback raises", show (Shipping Packing), show accepted)
    , -- The Reject raised by Checking's callback carries a Reason, which the
      -- callback of the state it leads to reads back.
      ("a raised event carries its payload to the next callback"
      , show ["idle after start", "refused: nothing ordered of book" :: Text]
      , show refusedLog
      )
    , ("a raised event still selects by name alone", show Refused, show refused)
    , ("done.state events fire as before alongside events that carry data"
      , show Idle
      , show shipped
      )
    , ("a done.state event carries nothing, and reaches the callback after it"
      , show ["idle after start", "checking 2 x book", "idle after DoneShipping" :: Text]
      , show shippedLog
      )
    , ("the same event on two transitions is one constructor"
      , show (Shipping Packing)
      , show again
      )
    , ("an event shows and reads back with its payload"
      , show (Order [Item "book"] 2)
      , show (read (show (Order [Item "book"] 2)) :: FsmEvent)
      )
    , ("the state type is unaffected by payloads"
      , show ["Packing", "Shipping" :: Text]
      , show (serializeStateMachine (Shipping Packing))
      )
    ]
