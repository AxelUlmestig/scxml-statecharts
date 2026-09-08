-- | A generic interpreter for the untyped chart model, following the
-- algorithm in Appendix D of the SCXML specification, minus history states,
-- guards and eventless transitions.
--
-- A configuration is the set of all active states, including ancestors, as in
-- SCXML. Callbacks (@<script>@ actions) may raise events, whiix are queued and
-- processed before a macrostep completes. Entering a @<final>@ state raises
-- SCXML's @done.state.Parent@ event, and @done.state.Grandparent@ when every
-- region of a parallel grandparent has completed.
module Statechart.Interpret
  ( Configuration
  , Phase (..)
  , Hooks (..)
  , initialConfiguration
  , start
  , macrostep
  , runToCompletion
  , microstep
  , doneEventName
  , canComplete
  ) where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Statechart.Model

-- | The set of active states, including ancestors, as in SCXML. The typed
-- state generated for a chart is an isomorphic view of a legal one.
type Configuration = Set StateId

-- | Where a callback is attached. Exit callbacks may not raise events.
data Phase = OnEntry | OnExit
  deriving (Eq, Ord, Show)

-- | How the interpreter reaches the callbacks, in terms of untyped names.
newtype Hooks m = Hooks
  { hookAction :: Phase -> Text -> Configuration -> Maybe Text -> m (Maybe Text)
    -- ^ run the named action, given the configuration it observes (before the
    -- transition for 'OnExit', after it otherwise) and the event being
    -- processed ('Nothing' during 'start'); returns an event to raise
  }

-- | The event SCXML raises when a state completes.
doneEventName :: StateId -> Text
doneEventName s = T.pack "done.state." <> s

-- | Can this state ever raise its done event? Parallel states always can (if
-- their regions can); a compound state needs a @<final>@ child.
canComplete :: Index -> StateId -> Bool
canComplete ix = completes . nodeOf ix

-- | The configuration after default entry, without running actions.
initialConfiguration :: Index -> Configuration
initialConfiguration ix = addDescendants ix Set.empty (ixInitial ix)

-- | Enter the initial configuration, running entry actions, then process any
-- events they raise.
start :: Monad m => Index -> Hooks m -> m Configuration
start ix hooks = do
  let cfg = initialConfiguration ix
  raised <- enterStates ix hooks cfg Nothing (sortOn (orderOf ix) (Set.toList cfg))
  runToCompletion ix hooks cfg raised

-- | Process one external event. Returns 'Nothing' if no transition was enabled
-- for it, otherwise the configuration reached after also processing every
-- event raised along the way.
macrostep :: Monad m => Index -> Hooks m -> Configuration -> Text -> m (Maybe Configuration)
macrostep ix hooks cfg ev = do
  r <- microstep ix hooks cfg ev
  case r of
    Nothing -> pure Nothing
    Just (cfg', raised) -> Just <$> runToCompletion ix hooks cfg' raised

-- | Process internally raised events in order until the queue is empty. A
-- raised event that no transition handles is dropped.
runToCompletion :: Monad m => Index -> Hooks m -> Configuration -> [Text] -> m Configuration
runToCompletion ix hooks = go (0 :: Int)
  where
    go _ cfg [] = pure cfg
    go n cfg (e : rest)
      | n > 1000 = error "Statechart: raised events do not terminate (an action keeps raising an event that leads back to it)"
      | otherwise = do
          r <- microstep ix hooks cfg e
          case r of
            Nothing -> go (n + 1) cfg rest
            Just (cfg', raised) -> go (n + 1) cfg' (rest ++ raised)

-- | Take the transitions enabled by one event. Returns the new configuration
-- and the events raised by actions, in the order they were raised.
microstep :: Monad m => Index -> Hooks m -> Configuration -> Text -> m (Maybe (Configuration, [Text]))
microstep ix hooks cfg ev =
  case selectTransitions ix cfg ev of
    [] -> pure Nothing
    ts -> do
      let exitSet = Set.unions (map (exitSetOf ix cfg) ts)
          entrySet = computeEntrySet ix ts
          cfg' = Set.union (cfg Set.\\ exitSet) entrySet
      mapM_ (\s -> mapM_ (\a -> hookAction hooks OnExit a cfg (Just ev)) (nodeOnExit (nodeOf ix s)))
            (sortOn (Down . orderOf ix) (Set.toList exitSet))
      raised <- enterStates ix hooks cfg' (Just ev) (sortOn (orderOf ix) (Set.toList entrySet))
      pure (Just (cfg', raised))

-- | Run the entry actions of states being entered, in order, and raise done
-- events for entered final states.
enterStates :: Monad m => Index -> Hooks m -> Configuration -> Maybe Text -> [StateId] -> m [Text]
enterStates ix hooks cfg ev states = concat <$> mapM enter states
  where
    enter s = do
      raised <- catMaybes <$> mapM (\a -> hookAction hooks OnEntry a cfg ev) (nodeOnEntry (nodeOf ix s))
      pure (raised ++ doneEvents ix cfg s)

doneEvents :: Index -> Configuration -> StateId -> [Text]
doneEvents ix cfg s
  | kindOf ix s /= Final = []
  | otherwise = case parentOf ix s of
      Nothing -> []
      Just p ->
        doneEventName p : case parentOf ix p of
          Just g
            | isParallel (kindOf ix g)
            , all (inFinalState ix cfg . nodeId) (nodeChildren (nodeOf ix g)) ->
                [doneEventName g]
          _ -> []

inFinalState :: Index -> Configuration -> StateId -> Bool
inFinalState ix cfg s = case kindOf ix s of
  Compound cs -> any (\c -> nodeKind c == Final && Set.member (nodeId c) cfg) (NE.toList cs)
  Parallel rs -> all (inFinalState ix cfg . nodeId) (NE.toList rs)
  _ -> False

-- Transition selection -------------------------------------------------------

-- | A transition the chart is about to take: the state it is declared on, and
-- the sibling of that state it enters.
type Taken = (StateId, StateId)

-- | The transitions enabled by an event. For eaix active atomic state, the
-- innermost enclosing state with a transition for the event wins. Two atomic
-- states in different regions can find the same one, hence the deduplication.
selectTransitions :: Index -> Configuration -> Text -> [Taken]
selectTransitions ix cfg ev =
  removeConflicting ix cfg (dedupe (concatMap (take 1 . candidates) atomics))
  where
    atomics = sortOn (orderOf ix) [s | s <- Set.toList cfg, kindOf ix s `elem` [Atomic, Final]]
    candidates s =
      [ (anc, tgt)
      | anc <- s : properAncestors ix s
      , Just tgt <- [Map.lookup ev (nodeTransitions (nodeOf ix anc))]
      ]
    dedupe = foldr (\t acc -> if any ((== fst t) . fst) acc then acc else t : acc) []

removeConflicting :: Index -> Configuration -> [Taken] -> [Taken]
removeConflicting ix cfg = foldl' step []
  where
    conflicts t1 t2 = not (Set.null (Set.intersection (exitSetOf ix cfg t1) (exitSetOf ix cfg t2)))
    step filtered t1 =
      let go [] toRemove = Just toRemove
          go (t2 : rest) toRemove
            | conflicts t1 t2 =
                if isDescendantOf ix (fst t1) (Just (fst t2))
                  then go rest (t2 : toRemove)
                  else Nothing
            | otherwise = go rest toRemove
       in case go filtered [] of
            Nothing -> filtered
            Just toRemove -> filter (`notElem` toRemove) filtered ++ [t1]

-- Exit and entry sets --------------------------------------------------------

-- | The state the transition stays inside, which decides what is exited and
-- re-entered. 'Nothing' is the chart root.
transitionDomain :: Index -> Taken -> Maybe StateId
transitionDomain ix (src, tgt) = lcca ix [src, tgt]

exitSetOf :: Index -> Configuration -> Taken -> Set StateId
exitSetOf ix cfg t = Set.filter (\s -> isDescendantOf ix s (transitionDomain ix t)) cfg

computeEntrySet :: Index -> [Taken] -> Set StateId
computeEntrySet ix = foldl' addTransition Set.empty
  where
    addTransition acc t@(_, tgt) =
      addAncestors ix tgt (transitionDomain ix t) (addDescendants ix acc tgt)

-- | Add a state and everything default entry into it implies.
addDescendants :: Index -> Set StateId -> StateId -> Set StateId
addDescendants ix acc s =
  let acc1 = Set.insert s acc
      n = nodeOf ix s
   in case nodeKind n of
        -- The initial child is a direct child, so it brings no intermediate
        -- ancestors of its own to enter.
        Compound (c :| _) -> addDescendants ix acc1 (nodeId c)
        Parallel rs -> foldl' (enterRegion ix) acc1 (map nodeId (NE.toList rs))
        _ -> acc1

-- | Add the ancestors of a state up to (excluding) the given ancestor, entering
-- sibling regions of any parallel state along the way.
addAncestors :: Index -> StateId -> Maybe StateId -> Set StateId -> Set StateId
addAncestors ix s ancestor acc =
  foldl' addOne acc (takeWhile (\a -> Just a /= ancestor) (properAncestors ix s))
  where
    addOne a anc =
      let a1 = Set.insert anc a
          n = nodeOf ix anc
       in case nodeKind n of
            Parallel rs -> foldl' (enterRegion ix) a1 (map nodeId (NE.toList rs))
            _ -> a1

enterRegion :: Index -> Set StateId -> StateId -> Set StateId
enterRegion ix acc region
  | any (\x -> isDescendantOf ix x (Just region)) (Set.toList acc) = acc
  | otherwise = addDescendants ix acc region
