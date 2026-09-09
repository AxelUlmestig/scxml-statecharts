{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Control.Monad (unless, when)
import Control.Monad.Trans.State.Strict (StateT, gets, modify', runStateT)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure)

-- The library's entire public API.
import Scxml.Statechart (scxml)
import qualified Overrides
import qualified Reordered

-- An order process: compound states, a parallel state that completes via
-- SCXML's automatic done.state event, a choice state whose entry callback
-- decides where to go by raising an event, a self-transition used for
-- polling, and effects named in the SCXML. State ids and event names are
-- Haskell constructor names, used verbatim. The name attribute is optional
-- metadata and does not affect the generated names.
[scxml|
<scxml xmlns="http://www.w3.org/2005/07/scxml" version="1.0" name="order-v1" initial="Draft">
  <state id="Draft">
    <transition event="Submit" target="Validating"/>
    <!-- Several event names on one element is shorthand for several
         transitions with the same target. -->
    <transition event="Discard Abandon" target="Cancelled"/>
  </state>

  <state id="Validating">
    <onentry><script>validate</script></onentry>
    <transition event="Valid" target="Processing"/>
    <transition event="Invalid" target="Rejected"/>
  </state>

  <state id="Processing" initial="Authorizing">
    <onentry><script>reserveStock</script></onentry>
    <onexit><script>releaseStock</script></onexit>
    <state id="Authorizing">
      <onentry><script>checkPrepayment</script></onentry>
      <!-- Polling: re-enter this state to re-run its entry callback. -->
      <transition event="Poll" target="Authorizing"/>
      <transition event="PaymentAuthorized" target="Fulfilment"/>
    </state>
    <parallel id="Fulfilment">
      <state id="Shipping" initial="Packing">
        <state id="Packing">
          <transition event="Packed" target="Shipped"/>
        </state>
        <final id="Shipped"/>
      </state>
      <state id="Invoicing" initial="Unpaid">
        <state id="Unpaid">
          <transition event="Paid" target="Settled"/>
        </state>
        <final id="Settled"/>
      </state>
      <!-- Only Fulfilment may react to its own completion, and it can only
           reach a sibling, so it moves to a final state of Processing. That
           completes Processing, whose own done event carries it further out. -->
      <transition event="done.state.Fulfilment" target="Fulfilled"/>
    </parallel>
    <final id="Fulfilled"/>
    <!-- Leaving Processing is declared on Processing: transitions never
         cross levels, so these apply anywhere inside it. -->
    <transition event="PaymentDeclined" target="Rejected"/>
    <transition event="done.state.Processing" target="Completed"/>
    <transition event="Cancel" target="Cancelled"/>
  </state>

  <final id="Completed">
    <!-- A callback named in src rather than in the content, the spelling the
         Postgres implementation of these charts uses. One or the other, not
         both. -->
    <onentry><script src="notifyCustomer"/></onentry>
  </final>
  <final id="Rejected"/>
  <final id="Cancelled"/>
</scxml>
|]

-- The quasiquote above generates:
--
--   data FsmState   = Draft | Validating | Processing Processing
--                   | Completed | Rejected | Cancelled
--   data Processing = Authorizing | Fulfilment Shipping Invoicing | Fulfilled
--   data Shipping   = Packing | Shipped
--   data Invoicing  = Unpaid | Settled
--   data FsmEvent   = Submit | Discard | Abandon | Valid | Invalid | Poll
--                   | PaymentAuthorized | Packed | Paid | DoneFulfilment
--                   | PaymentDeclined | DoneProcessing | Cancel
--                   | DoneShipping | DoneInvoicing
--   initiateStateMachine, notifyStateMachine   -- signatures below are ours
--   serializeStateMachine   :: FsmState -> [Text]
--   deserializeStateMachine :: [Text] -> Maybe FsmState

-- | The "datamodel": whatever the callbacks need lives in the monad.
data Shop = Shop
  { items    :: [Text]
  , prepaid  :: Bool
  , reserved :: Int
  , log_     :: [Text]
  }
  deriving (Show)

type M = StateT Shop IO

initiateStateMachine :: M FsmState
notifyStateMachine :: FsmState -> FsmEvent -> M FsmState

-- Callbacks named in the SCXML. Entry callbacks return m (Maybe FsmEvent),
-- raising an event with Just; exit callbacks return m (). All in one monad,
-- or it does not compile.

-- | A choice state's entry callback: decide by raising an event.
validate :: FsmState -> Maybe FsmEvent -> M (Maybe FsmEvent)
validate _ _ = do
  ok <- gets (not . null . items)
  pure (Just (if ok then Valid else Invalid))

-- | Poll something on entry; move on immediately if it is already settled.
-- Re-entered by the Poll self-transition, which is how polling replaces a
-- transition script.
checkPrepayment :: FsmState -> Maybe FsmEvent -> M (Maybe FsmEvent)
checkPrepayment _ _ = do
  say "checked prepayment"
  paid <- gets prepaid
  pure (if paid then Just PaymentAuthorized else Nothing)

-- Entry callbacks that raise nothing still say so, with pure Nothing.
reserveStock, notifyCustomer :: FsmState -> Maybe FsmEvent -> M (Maybe FsmEvent)
reserveStock _ _ = do
  n <- gets (length . items)
  modify' (\s -> s {reserved = n})
  say "reserved stock"
  pure Nothing
-- Entry callbacks see the state entered and the event that caused it.
notifyCustomer s ev = do
  say ("notified customer: " <> tshow s <> " after " <> maybe "start" tshow ev)
  pure Nothing

-- Exit callbacks see the state being left, and cannot raise.
releaseStock :: FsmState -> Maybe FsmEvent -> M ()
releaseStock s _ = modify' (\s' -> s' {reserved = 0}) >> say ("released stock leaving " <> tshow s)

say :: Text -> M ()
say t = modify' (\s -> s {log_ = log_ s ++ [t]})

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Deliver one event from a given state, discarding the datamodel.
stepFrom :: Shop -> FsmState -> FsmEvent -> IO FsmState
stepFrom shop s e = fst <$> runStateT (notifyStateMachine s e) shop

-- | Start the chart and feed events. Returns the final state and the datamodel.
runEvents :: Shop -> [FsmEvent] -> IO (FsmState, Shop)
runEvents shop evs = runStateT (initiateStateMachine >>= go evs) shop
  where
    go [] s = pure s
    go (e : rest) s = notifyStateMachine s e >>= go rest

shopWith :: [Text] -> Shop
shopWith is = Shop {items = is, prepaid = False, reserved = 0, log_ = []}

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check :: (Eq a, Show a) => String -> a -> a -> IO ()
      check label expected actual =
        unless (expected == actual) $ do
          putStrLn ("FAIL " ++ label ++ "\n  expected: " ++ show expected ++ "\n  actual:   " ++ show actual)
          modifyIORef failures (+ 1)

  -- Happy path. Submit passes through Validating (its callback raises Valid),
  -- both regions reach final states, done.state.Fulfilment fires, and the
  -- callbacks run in SCXML order with the state and event they observe.
  (end, shop) <- runEvents (shopWith ["book"]) [Submit, PaymentAuthorized, Paid, Packed]
  check "happy path reaches Completed" Completed end
  -- Completion climbs one level at a time: Fulfilment finishing moves it to
  -- the final state Fulfilled, which completes Processing, whose own done
  -- event leaves it. All inside one call, so the caller sees only Completed.
  check "callbacks in order, with state and event"
    [ "reserved stock"
    , "checked prepayment"
    , "released stock leaving Processing Fulfilled"
    , "notified customer: Completed after DoneProcessing"
    ]
    (log_ shop)
  check "release resets the reservation" 0 (reserved shop)

  -- The choice state decides from the datamodel: no items, so Invalid is
  -- raised and nothing else runs.
  (end2, shopEmpty) <- runEvents (shopWith []) [Submit]
  check "empty order is rejected" Rejected end2
  check "no effects for a rejected order" [] (log_ shopEmpty)

  -- The choice state is transient: one Submit lands in Processing.
  (end2b, _) <- runEvents (shopWith ["book"]) [Submit]
  check "validating never rests" (Processing Authorizing) end2b

  -- Polling via a self-transition, the replacement for a transition script.
  -- Re-entering Authorizing re-runs its entry callback, which now sees the
  -- payment as settled and raises the event that moves the chart on.
  (end2c, shopPoll) <- runStateT
    ( do
        s0 <- initiateStateMachine
        s1 <- notifyStateMachine s0 Submit
        s2 <- notifyStateMachine s1 Poll
        modify' (\sh -> sh {prepaid = True}) -- the third party settles
        notifyStateMachine s2 Poll
    )
    (shopWith ["book"])
  check "polling moves on once the check succeeds" (Processing (Fulfilment Packing Unpaid)) end2c
  check "each poll re-runs the entry callback"
    ["reserved stock", "checked prepayment", "checked prepayment", "checked prepayment"]
    (log_ shopPoll)

  -- Moving a transition up a level widens where it applies: PaymentDeclined
  -- is declared on Processing, so it now rejects from inside Fulfilment too,
  -- where before (declared on Authorizing) it was ignored.
  (end2d, _) <- runEvents (shopWith ["book"]) [Submit, PaymentAuthorized, PaymentDeclined]
  check "a transition on the enclosing state applies anywhere inside it" Rejected end2d
  (end2e, _) <- runEvents (shopWith ["book"]) [Submit, PaymentDeclined]
  check "and still applies at the level it used to be on" Rejected end2e

  -- A transition on the parent applies anywhere inside it, and runs its onexit.
  (end3, shop3) <- runEvents (shopWith ["book"]) [Submit, PaymentAuthorized, Packed, Cancel]
  check "cancel from inside Fulfilment" Cancelled end3
  check "cancel releases stock"
    ["reserved stock", "checked prepayment", "released stock leaving Processing (Fulfilment Shipped Unpaid)"]
    (log_ shop3)

  -- An event with no transition in the current state is ignored: same state,
  -- no effects.
  (end4, shop4) <- runEvents (shopWith ["book"]) [Submit, Packed, Paid, Cancel, Cancel]
  check "unhandled events leave the state alone" Cancelled end4
  check "unhandled events run nothing"
    ["reserved stock", "checked prepayment", "released stock leaving Processing Authorizing"]
    (log_ shop4)

  -- An entry callback raising an event: processed before the step returns.
  (end5, shop5) <- runEvents (shopWith ["book"]) {prepaid = True} [Submit]
  check "raised event is processed in the same step" (Processing (Fulfilment Packing Unpaid)) end5
  check "raised event effects" ["reserved stock", "checked prepayment"] (log_ shop5)

  -- Regions are independent; one region completing does not complete the parallel.
  (end6, _) <- runEvents (shopWith ["book"]) [Submit, PaymentAuthorized, Packed]
  check "one region done" (Processing (Fulfilment Shipped Unpaid)) end6

  -- Behaviour reachable only through the generated functions, since the
  -- library exports nothing but the quasiquoter.
  (started, _) <- runStateT initiateStateMachine (shopWith ["book"])
  check "the chart starts in its initial state" Draft started
  doneFired <- stepFrom (shopWith ["book"]) (Processing (Fulfilment Shipped Unpaid)) Paid
  check "the last region completing fires the done event" Completed doneFired
  ignored <- stepFrom (shopWith ["book"]) Draft Paid
  check "an event with no transition here leaves the state alone" Draft ignored
  stayed <- stepFrom (shopWith ["book"]) (Processing Authorizing) Poll
  check "a self-transition re-enters without moving" (Processing Authorizing) stayed
  viaDiscard <- stepFrom (shopWith ["book"]) Draft Discard
  viaAbandon <- stepFrom (shopWith ["book"]) Draft Abandon
  check "several events on one transition all reach its target"
    (Cancelled, Cancelled) (viaDiscard, viaAbandon)
  check "all events, in document order"
    [ Submit, Discard, Abandon, Valid, Invalid, Poll, PaymentAuthorized, Packed
    , Paid, DoneFulfilment, PaymentDeclined, DoneProcessing, Cancel
    , DoneShipping, DoneInvoicing ]
    [minBound .. maxBound :: FsmEvent]

  -- Serialization. Show/Read round-trips exactly; the id list is the portable
  -- form, and rejects anything that is not a configuration of this chart.
  let deep = Processing (Fulfilment Shipped Unpaid)
  check "Read round-trips" deep (read (show deep))
  check "state ids" ["Fulfilment", "Invoicing", "Processing", "Shipped", "Shipping", "Unpaid"]
    (serializeStateMachine deep)
  check "id round-trip, nested" (Just deep) (deserializeStateMachine (serializeStateMachine deep))
  check "id round-trip, atomic" (Just Draft) (deserializeStateMachine (serializeStateMachine Draft))
  check "id order and duplicates do not matter" (Just deep)
    (deserializeStateMachine (reverse (serializeStateMachine deep) ++ ["Processing"]))
  check "an id list that merely starts valid is rejected" Nothing
    (deserializeStateMachine ["Draft", "Processing"])
  check "an incomplete configuration is rejected" Nothing
    (deserializeStateMachine ["Processing"])
  check "an unknown id is rejected" Nothing (deserializeStateMachine ["Archived"])
  check "an empty list is rejected" Nothing (deserializeStateMachine [])

  -- A second chart, in its own module since the generated names are fixed.
  reordered <- Reordered.spec
  overrides <- Overrides.spec
  mapM_ (\(label, expected, actual) -> check label expected actual) (reordered ++ overrides)

  n <- readIORef failures
  when (n > 0) exitFailure
  putStrLn "all checks passed"
