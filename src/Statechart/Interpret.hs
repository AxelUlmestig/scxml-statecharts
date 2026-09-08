-- | A generic interpreter for the untyped t'Chart' model, following the
-- algorithm in Appendix D of the SCXML specification, minus history states,
-- guards and eventless transitions.
--
-- A configuration is the set of all active states, including ancestors, as in
-- SCXML. Callbacks (@<script>@ actions) may raise events, which are queued and
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
import qualified Data.List.NonEmpty as NE
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
canComplete :: Chart -> StateId -> Bool
canComplete ch s = case kindOf ch s of
  Parallel _ -> True
  Compound _ _ -> any (\c -> kindOf ch c == Final) (nodeChildren (nodeOf ch s))
  _ -> False

-- | The configuration after default entry, without running actions.
initialConfiguration :: Chart -> Configuration
initialConfiguration ch = addDescendants ch Set.empty (chartInitial ch)

-- | Enter the initial configuration, running entry actions, then process any
-- events they raise.
start :: Monad m => Chart -> Hooks m -> m Configuration
start ch hooks = do
  let cfg = initialConfiguration ch
  raised <- enterStates ch hooks cfg Nothing (sortOn (orderOf ch) (Set.toList cfg))
  runToCompletion ch hooks cfg raised

-- | Process one external event. Returns 'Nothing' if no transition was enabled
-- for it, otherwise the configuration reached after also processing every
-- event raised along the way.
macrostep :: Monad m => Chart -> Hooks m -> Configuration -> Text -> m (Maybe Configuration)
macrostep ch hooks cfg ev = do
  r <- microstep ch hooks cfg ev
  case r of
    Nothing -> pure Nothing
    Just (cfg', raised) -> Just <$> runToCompletion ch hooks cfg' raised

-- | Process internally raised events in order until the queue is empty. A
-- raised event that no transition handles is dropped.
runToCompletion :: Monad m => Chart -> Hooks m -> Configuration -> [Text] -> m Configuration
runToCompletion ch hooks = go (0 :: Int)
  where
    go _ cfg [] = pure cfg
    go n cfg (e : rest)
      | n > 1000 = error "Statechart: raised events do not terminate (an action keeps raising an event that leads back to it)"
      | otherwise = do
          r <- microstep ch hooks cfg e
          case r of
            Nothing -> go (n + 1) cfg rest
            Just (cfg', raised) -> go (n + 1) cfg' (rest ++ raised)

-- | Take the transitions enabled by one event. Returns the new configuration
-- and the events raised by actions, in the order they were raised.
microstep :: Monad m => Chart -> Hooks m -> Configuration -> Text -> m (Maybe (Configuration, [Text]))
microstep ch hooks cfg ev =
  case selectTransitions ch cfg ev of
    [] -> pure Nothing
    ts -> do
      let exitSet = Set.unions (map (exitSetOf ch cfg) ts)
          entrySet = computeEntrySet ch ts
          cfg' = Set.union (cfg Set.\\ exitSet) entrySet
      mapM_ (\s -> mapM_ (\a -> hookAction hooks OnExit a cfg (Just ev)) (nodeOnExit (nodeOf ch s)))
            (sortOn (Down . orderOf ch) (Set.toList exitSet))
      raised <- enterStates ch hooks cfg' (Just ev) (sortOn (orderOf ch) (Set.toList entrySet))
      pure (Just (cfg', raised))

-- | Run the entry actions of states being entered, in order, and raise done
-- events for entered final states.
enterStates :: Monad m => Chart -> Hooks m -> Configuration -> Maybe Text -> [StateId] -> m [Text]
enterStates ch hooks cfg ev states = concat <$> mapM enter states
  where
    enter s = do
      raised <- catMaybes <$> mapM (\a -> hookAction hooks OnEntry a cfg ev) (nodeOnEntry (nodeOf ch s))
      pure (raised ++ doneEvents ch cfg s)

doneEvents :: Chart -> Configuration -> StateId -> [Text]
doneEvents ch cfg s
  | kindOf ch s /= Final = []
  | otherwise = case nodeParent (nodeOf ch s) of
      Nothing -> []
      Just p ->
        doneEventName p : case nodeParent (nodeOf ch p) of
          Just g | isParallel (kindOf ch g), all (inFinalState ch cfg) (nodeChildren (nodeOf ch g)) -> [doneEventName g]
          _ -> []

inFinalState :: Chart -> Configuration -> StateId -> Bool
inFinalState ch cfg s = case kindOf ch s of
  Compound _ _ -> any (\c -> kindOf ch c == Final && Set.member c cfg) (nodeChildren (nodeOf ch s))
  Parallel _ -> all (inFinalState ch cfg) (nodeChildren (nodeOf ch s))
  _ -> False

-- Transition selection -------------------------------------------------------

selectTransitions :: Chart -> Configuration -> Text -> [Transition]
selectTransitions ch cfg ev =
  removeConflicting ch cfg (dedupe (concatMap (take 1 . candidates) atomics))
  where
    atomics = sortOn (orderOf ch) [s | s <- Set.toList cfg, kindOf ch s `elem` [Atomic, Final]]
    candidates s = [t | anc <- s : properAncestors ch s, t <- nodeTransitions (nodeOf ch anc), ev `elem` trEvents t]
    dedupe = foldr (\t acc -> if any ((== trOrder t) . trOrder) acc then acc else t : acc) []

removeConflicting :: Chart -> Configuration -> [Transition] -> [Transition]
removeConflicting ch cfg = foldl' step []
  where
    conflicts t1 t2 = not (Set.null (Set.intersection (exitSetOf ch cfg t1) (exitSetOf ch cfg t2)))
    step filtered t1 =
      let go [] toRemove = Just toRemove
          go (t2 : rest) toRemove
            | conflicts t1 t2 =
                if isDescendantOf ch (trSource t1) (Just (trSource t2))
                  then go rest (t2 : toRemove)
                  else Nothing
            | otherwise = go rest toRemove
       in case go filtered [] of
            Nothing -> filtered
            Just toRemove -> filter (`notElem` toRemove) filtered ++ [t1]

-- Exit and entry sets --------------------------------------------------------

-- | The state the transition stays inside, which decides what is exited and
-- re-entered. 'Nothing' is the chart root.
transitionDomain :: Chart -> Transition -> Maybe StateId
transitionDomain ch t = lcca ch (trSource t : trTargets t)

exitSetOf :: Chart -> Configuration -> Transition -> Set StateId
exitSetOf ch cfg t = Set.filter (\s -> isDescendantOf ch s (transitionDomain ch t)) cfg

computeEntrySet :: Chart -> [Transition] -> Set StateId
computeEntrySet ch = foldl' addTransition Set.empty
  where
    addTransition acc t =
      let dom = transitionDomain ch t
          acc1 = foldl' (addDescendants ch) acc (trTargets t)
       in foldl' (\a s -> addAncestors ch s dom a) acc1 (trTargets t)

-- | Add a state and everything default entry into it implies.
addDescendants :: Chart -> Set StateId -> StateId -> Set StateId
addDescendants ch acc s =
  let acc1 = Set.insert s acc
      n = nodeOf ch s
   in case nodeKind n of
        -- The initial child is a direct child, so it brings no intermediate
        -- ancestors of its own to enter.
        Compound _ c -> addDescendants ch acc1 c
        Parallel rs -> foldl' (enterRegion ch) acc1 (NE.toList rs)
        _ -> acc1

-- | Add the ancestors of a state up to (excluding) the given ancestor, entering
-- sibling regions of any parallel state along the way.
addAncestors :: Chart -> StateId -> Maybe StateId -> Set StateId -> Set StateId
addAncestors ch s ancestor acc =
  foldl' addOne acc (takeWhile (\a -> Just a /= ancestor) (properAncestors ch s))
  where
    addOne a anc =
      let a1 = Set.insert anc a
          n = nodeOf ch anc
       in case nodeKind n of
            Parallel rs -> foldl' (enterRegion ch) a1 (NE.toList rs)
            _ -> a1

enterRegion :: Chart -> Set StateId -> StateId -> Set StateId
enterRegion ch acc region
  | any (\x -> isDescendantOf ch x (Just region)) (Set.toList acc) = acc
  | otherwise = addDescendants ch acc region
