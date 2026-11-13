-- | The evaluator. One recursive pass over the chart tree per event.
--
-- Because a transition may only target a sibling of its source, a transition
-- never moves anything outside its parent. That collapses most of the general
-- SCXML machinery: there are no least common ancestors to find, no exit sets
-- to filter out of the whole configuration, and no conflicts to resolve
-- between transitions at different depths. A state only ever rearranges its
-- own children, so the whole algorithm is a walk down and back up, and nothing
-- needs looking up by id.
--
-- A configuration is the set of all active states, ancestors included, as in
-- SCXML. The typed state generated for a chart is an isomorphic view of a
-- legal one.
module Scxml.Statechart.Interpret
  ( Configuration
  , Callbacks (..)
  , doneEventName
  , start
  , macrostep
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Scxml.Statechart.Model

type Configuration = Set StateId

-- | How the evaluator reaches the callbacks, and everything it needs to know
-- about the event type to do so. "Scxml.Statechart.Run" wraps the typed
-- 'Scxml.Statechart.Run.Hooks' into one of these.
--
-- The evaluator selects transitions by name and never looks inside an event,
-- but it carries the event itself from the caller through to the callbacks,
-- so an event may hold a payload the chart knows nothing about.
data Callbacks m ev = Callbacks
  { runEntry :: Text -> Maybe (Configuration, ev) -> Configuration -> m (Maybe ev)
    -- ^ run the named entry callback, given the configuration the step leaves
    -- with the event that caused it ('Nothing' during 'start'), and the
    -- configuration the step settles in; returns an event to raise
  , runExit :: Text -> Configuration -> ev -> Configuration -> m ()
    -- ^ run the named exit callback, given the configuration the step leaves,
    -- the event that caused it and the configuration it settles in. Exit
    -- callbacks only run inside a step, so all three are always there.
  , eventNameOf :: ev -> Text
    -- ^ the name a transition matches on
  , doneEvent :: StateId -> ev
    -- ^ the event a state raises on completing, which the evaluator
    -- synthesises rather than receiving, and which therefore carries nothing
  }

-- | The event SCXML raises when a state completes.
doneEventName :: StateId -> Text
doneEventName s = T.pack "done.state." <> s

-- Entering ------------------------------------------------------------------

-- | The result of entering a state: its subtree's configuration, the states
-- entered with the states each entry completes, and whether the subtree is
-- now in a final state. A completed state is carried as its id, since the
-- event it raises is built by 'doneEvent' only when it reaches the queue.
data Entered = Entered
  { enConfig  :: Configuration
  , enEntered :: [(Node, [StateId])] -- ^ outermost first
  , enFinal   :: Bool
  }

-- | Entering a final child is what completes its parent, so the parent's done
-- event is raised just after that child's own entry callbacks.
completing :: StateId -> Node -> Entered -> Entered
completing parent target e@Entered {enEntered}
  | not (isFinal target) = e
  | otherwise = e {enEntered = attach enEntered, enFinal = True}
  where
    attach ((h, ds) : rest) = (h, ds ++ [parent]) : rest
    attach [] = []

-- | Enter a state and everything default entry into it implies.
enter :: Node -> Entered
enter n@Node {nodeId, nodeKind} = case nodeKind of
  Atomic -> leaf False
  Final -> leaf True
  Compound (c :| _) ->
    let Entered {enConfig, enEntered, enFinal} = completing nodeId c (enter c)
     in Entered
          { enConfig = Set.insert nodeId enConfig
          , enEntered = (n, []) : enEntered
          , enFinal
          }
  Parallel rs ->
    let belows = NE.toList (fmap enter rs)
        allFinal = all enFinal belows
     in Entered
          { enConfig = Set.insert nodeId (Set.unions (map enConfig belows))
          , enEntered = (n, [nodeId | allFinal]) : concatMap enEntered belows
          , enFinal = allFinal
          }
  where
    leaf isFin =
      Entered {enConfig = Set.singleton nodeId, enEntered = [(n, [])], enFinal = isFin}

isFinal :: Node -> Bool
isFinal Node {nodeKind} = nodeKind == Final

-- | The active states of a subtree, innermost first, which is the order exit
-- callbacks run in.
exiting :: Configuration -> Node -> [Node]
exiting cfg n = concatMap (exiting cfg) (activeChildren cfg n) ++ [n]

-- | The children of a state that are currently active: one for a compound
-- state, all of them for a parallel state, none for a leaf.
activeChildren :: Configuration -> Node -> [Node]
activeChildren cfg Node {nodeKind} = case nodeKind of
  Compound kids -> maybe [] pure (activeChild cfg kids)
  Parallel rs -> NE.toList rs
  _ -> []

activeChild :: Configuration -> NonEmpty Node -> Maybe Node
activeChild cfg kids = listToMaybe (NE.filter (\Node {nodeId} -> Set.member nodeId cfg) kids)

-- | A named child. The parser has already checked that every transition
-- target is one of its source's siblings, so this cannot fail for a chart the
-- quasiquoter built.
childNamed :: StateId -> NonEmpty Node -> Node
childNamed tgt kids = case NE.filter (\Node {nodeId} -> nodeId == tgt) kids of
  t : _ -> t
  [] -> error ("Statechart: unknown transition target " ++ T.unpack tgt)

-- Offering an event ---------------------------------------------------------

-- | What a subtree reports after being offered an event.
data Reply = Reply
  { rpMove :: Maybe StateId
    -- ^ 'Just' when this state itself has a transition for the event. Its
    -- parent performs the switch, since the target is one of the parent's
    -- children.
  , rpConfig :: Configuration -- ^ meaningful only when 'rpMove' is 'Nothing'
  , rpExited :: [Node] -- ^ innermost first
  , rpEntered :: [(Node, [StateId])] -- ^ outermost first
  , rpConsumed :: Bool
  , rpFinal :: Bool
  }

-- | Offer an event to a subtree. Children are asked first, and a state only
-- acts on the event if nothing below it did, so the innermost transition wins
-- and a transition on an enclosing state behaves as a default.
offer :: Configuration -> Text -> Node -> Reply
offer cfg ev n@Node {nodeId, nodeKind, nodeTransitions} = case nodeKind of
  Atomic -> own
  Final -> own
  Compound kids -> case activeChild cfg kids of
    Nothing -> own
    Just active ->
      let below@Reply {rpConsumed} = offer cfg ev active
       in if rpConsumed then absorb kids active below else own
  Parallel rs ->
    let belows = NE.toList (fmap (offer cfg ev) rs)
     in if any rpConsumed belows then absorbRegions belows else own
  where
    -- This state's own transition, for its parent to perform.
    own = case Map.lookup ev nodeTransitions of
      Just tgt -> stay {rpMove = Just tgt, rpConsumed = True}
      Nothing -> stay
    stay =
      Reply
        { rpMove = Nothing
        , rpConfig = subtree cfg n
        , rpExited = []
        , rpEntered = []
        , rpConsumed = False
        , rpFinal = inFinalState cfg n
        }

    -- A compound state whose active child either moved to a sibling or
    -- settled internally. Either way this state stays put.
    absorb kids active below@Reply {rpMove} = case rpMove of
      Nothing ->
        let Reply {rpConfig, rpExited, rpEntered} = below
         in stay
              { rpConfig = Set.insert nodeId rpConfig
              , rpExited
              , rpEntered
              , rpConsumed = True
              , rpFinal = isFinal active
              }
      Just tgt ->
        let target = childNamed tgt kids
            Entered {enConfig, enEntered, enFinal} = completing nodeId target (enter target)
         in stay
              { rpConfig = Set.insert nodeId enConfig
              , rpExited = exiting cfg active
              , rpEntered = enEntered
              , rpConsumed = True
              , rpFinal = enFinal
              }

    -- A parallel state: regions cannot have transitions, so none of them can
    -- move, and their subtrees merge unchanged apart from what settled inside.
    absorbRegions belows =
      let allFinal = all rpFinal belows
          justCompleted = allFinal && not (inFinalState cfg n)
       in stay
            { rpConfig = Set.insert nodeId (Set.unions (map rpConfig belows))
            , rpExited = concatMap rpExited belows
            , rpEntered =
                concatMap rpEntered belows
                  ++ [(n, [nodeId]) | justCompleted]
            , rpConsumed = True
            , rpFinal = allFinal
            }


-- | The active states of a subtree, as a set.
subtree :: Configuration -> Node -> Configuration
subtree cfg n = Set.fromList (map nodeId (exiting cfg n))

-- | Whether a state counts as completed: a final state is, a compound state is
-- when its active child is final, and a parallel state is when every region
-- is. Only a parallel state's own completion consults this.
inFinalState :: Configuration -> Node -> Bool
inFinalState cfg Node {nodeKind} = case nodeKind of
  Final -> True
  Atomic -> False
  Compound kids -> maybe False isFinal (activeChild cfg kids)
  Parallel rs -> all (inFinalState cfg) (NE.toList rs)

-- Running -------------------------------------------------------------------

-- | Enter the chart's initial state, then process whatever that raises.
start :: Monad m => Chart -> Callbacks m ev -> m Configuration
start ch@Chart {chartRoot} cbs = do
  let Entered {enConfig, enEntered} = enter (NE.head chartRoot)
  raised <- runEntries cbs Nothing enConfig enEntered
  runToCompletion ch cbs enConfig raised

-- | Process one external event. 'Nothing' if no transition was enabled for it.
macrostep :: Monad m => Chart -> Callbacks m ev -> Configuration -> ev -> m (Maybe Configuration)
macrostep ch cbs cfg ev = do
  r <- microstep ch cbs cfg ev
  case r of
    Nothing -> pure Nothing
    Just (cfg', raised) -> Just <$> runToCompletion ch cbs cfg' raised

-- | Process raised events in order until the queue is empty. One that no
-- transition handles is dropped.
runToCompletion :: Monad m => Chart -> Callbacks m ev -> Configuration -> [ev] -> m Configuration
runToCompletion ch cbs = go (0 :: Int)
  where
    go _ cfg [] = pure cfg
    go n cfg (e : rest)
      | n > 1000 = error "Statechart: raised events do not terminate (a callback keeps raising an event that leads back to it)"
      | otherwise = do
          r <- microstep ch cbs cfg e
          case r of
            Nothing -> go (n + 1) cfg rest
            Just (cfg', raised) -> go (n + 1) cfg' (rest ++ raised)

-- | One event, one pass. The chart root behaves as a compound state: exactly
-- one of its children is active, and it has no transitions of its own.
microstep :: Monad m => Chart -> Callbacks m ev -> Configuration -> ev -> m (Maybe (Configuration, [ev]))
microstep Chart {chartRoot} cbs@Callbacks {eventNameOf} cfg ev =
  case activeChild cfg chartRoot of
    Nothing -> pure Nothing
    Just active ->
      let below@Reply {rpConsumed, rpMove} = offer cfg (eventNameOf ev) active
       in if not rpConsumed
            then pure Nothing
            else do
              let (cfg', exited, entered) = case rpMove of
                    Nothing ->
                      let Reply {rpConfig, rpExited, rpEntered} = below
                       in (rpConfig, rpExited, rpEntered)
                    Just tgt ->
                      let Entered {enConfig, enEntered} = enter (childNamed tgt chartRoot)
                       in (enConfig, exiting cfg active, enEntered)
              mapM_ (runExits cbs cfg ev cfg') exited
              raised <- runEntries cbs (Just (cfg, ev)) cfg' entered
              pure (Just (cfg', raised))

-- | Every callback of a step sees the whole step: the configuration it leaves,
-- the event that caused it, and the configuration it settles in. Exit
-- callbacks see the new configuration too, though they run before it has
-- been entered.
runExits :: Monad m => Callbacks m ev -> Configuration -> ev -> Configuration -> Node -> m ()
runExits Callbacks {runExit} cfg ev cfg' Node {nodeOnExit} =
  mapM_ (\a -> runExit a cfg ev cfg') nodeOnExit

-- | Entry callbacks all get the configuration the step settles in, even
-- though they run outermost first.
runEntries :: Monad m => Callbacks m ev -> Maybe (Configuration, ev) -> Configuration -> [(Node, [StateId])] -> m [ev]
runEntries Callbacks {runEntry, doneEvent} cause cfg' = fmap concat . mapM one
  where
    one (Node {nodeOnEntry}, dones) = do
      raised <- mapM (\a -> runEntry a cause cfg') nodeOnEntry
      pure ([r | Just r <- raised] ++ map doneEvent dones)
