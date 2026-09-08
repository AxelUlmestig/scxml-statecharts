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
module Statechart.Interpret
  ( Configuration
  , Phase (..)
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

import Statechart.Model

type Configuration = Set StateId

-- | Where a callback is attached. Exit callbacks may not raise events.
data Phase = OnEntry | OnExit
  deriving (Eq, Ord, Show)

-- | How the evaluator reaches the callbacks, in the only terms it knows:
-- state ids and event names. "Statechart.Run" wraps the typed
-- 'Statechart.Run.Hooks' into one of these.
newtype Callbacks m = Callbacks
  { runCallback :: Phase -> Text -> Configuration -> Maybe Text -> m (Maybe Text)
    -- ^ run the named callback, given the configuration it observes and the
    -- event being processed ('Nothing' during 'start'); returns an event to raise
  }

-- | The event SCXML raises when a state completes.
doneEventName :: StateId -> Text
doneEventName s = T.pack "done.state." <> s

-- Entering ------------------------------------------------------------------

-- | The result of entering a state: its subtree's configuration, the states
-- entered with the done events each entry implies, and whether the subtree is
-- now in a final state.
data Entered = Entered
  { enConfig  :: Configuration
  , enEntered :: [(Node, [Text])] -- ^ outermost first
  , enFinal   :: Bool
  }

-- | Entering a final child is what completes its parent, so the parent's done
-- event is raised just after that child's own entry callbacks.
completing :: StateId -> Node -> Entered -> Entered
completing parent target e
  | nodeKind target /= Final = e
  | otherwise = e {enEntered = attach (enEntered e), enFinal = True}
  where
    attach ((h, ds) : rest) = (h, ds ++ [doneEventName parent]) : rest
    attach [] = []

-- | Enter a state and everything default entry into it implies.
enter :: Node -> Entered
enter n = case nodeKind n of
  Atomic -> leaf False
  Final -> leaf True
  Compound (c :| _) ->
    let below = completing (nodeId n) c (enter c)
     in Entered
          { enConfig = Set.insert (nodeId n) (enConfig below)
          , enEntered = (n, []) : enEntered below
          , enFinal = enFinal below
          }
  Parallel rs ->
    let belows = fmap enter rs
        allFinal = all enFinal belows
        dones = [doneEventName (nodeId n) | allFinal]
     in Entered
          { enConfig = Set.insert (nodeId n) (Set.unions (fmap enConfig (NE.toList belows)))
          , enEntered = (n, dones) : concatMap enEntered (NE.toList belows)
          , enFinal = allFinal
          }
  where
    leaf isFin =
      Entered {enConfig = Set.singleton (nodeId n), enEntered = [(n, [])], enFinal = isFin}

-- | The active states of a subtree, innermost first, which is the order exit
-- callbacks run in.
exiting :: Configuration -> Node -> [Node]
exiting cfg n = concatMap (exiting cfg) (activeChildren cfg n) ++ [n]

-- | The children of a state that are currently active: one for a compound
-- state, all of them for a parallel state, none for a leaf.
activeChildren :: Configuration -> Node -> [Node]
activeChildren cfg n = case nodeKind n of
  Compound kids -> maybe [] pure (activeChild cfg kids)
  Parallel rs -> NE.toList rs
  _ -> []

activeChild :: Configuration -> NonEmpty Node -> Maybe Node
activeChild cfg kids = listToMaybe (NE.filter (\c -> Set.member (nodeId c) cfg) kids)

-- | A named child. The parser has already checked that every transition
-- target is one of its source's siblings, so this cannot fail for a chart the
-- quasiquoter built.
childNamed :: StateId -> NonEmpty Node -> Node
childNamed tgt kids = case NE.filter ((== tgt) . nodeId) kids of
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
  , rpEntered :: [(Node, [Text])] -- ^ outermost first
  , rpConsumed :: Bool
  , rpFinal :: Bool
  }

-- | Offer an event to a subtree. Children are asked first, and a state only
-- acts on the event if nothing below it did, so the innermost transition wins
-- and a transition on an enclosing state behaves as a default.
offer :: Configuration -> Text -> Node -> Reply
offer cfg ev n = case nodeKind n of
  Atomic -> own
  Final -> own
  Compound kids -> case activeChild cfg kids of
    Nothing -> own
    Just active ->
      let below = offer cfg ev active
       in if rpConsumed below then absorb kids active below else own
  Parallel rs ->
    let belows = fmap (offer cfg ev) rs
     in if any rpConsumed belows then absorbRegions belows else own
  where
    -- This state's own transition, for its parent to perform.
    own = case Map.lookup ev (nodeTransitions n) of
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
    absorb kids active below = case rpMove below of
      Nothing ->
        stay
          { rpConfig = Set.insert (nodeId n) (rpConfig below)
          , rpExited = rpExited below
          , rpEntered = rpEntered below
          , rpConsumed = True
          , rpFinal = nodeKind active == Final
          }
      Just tgt ->
        let target = childNamed tgt kids
            entered = completing (nodeId n) target (enter target)
         in stay
              { rpConfig = Set.insert (nodeId n) (enConfig entered)
              , rpExited = exiting cfg active
              , rpEntered = enEntered entered
              , rpConsumed = True
              , rpFinal = enFinal entered
              }

    -- A parallel state: regions cannot have transitions, so none of them can
    -- move, and their subtrees merge unchanged apart from what settled inside.
    absorbRegions belows =
      let allFinal = all rpFinal belows
          justCompleted = allFinal && not (inFinalState cfg n)
       in stay
            { rpConfig = Set.insert (nodeId n) (Set.unions (fmap rpConfig (NE.toList belows)))
            , rpExited = concatMap rpExited (NE.toList belows)
            , rpEntered =
                concatMap rpEntered (NE.toList belows)
                  ++ [(n, [doneEventName (nodeId n)]) | justCompleted]
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
inFinalState cfg n = case nodeKind n of
  Final -> True
  Atomic -> False
  Compound kids -> maybe False ((== Final) . nodeKind) (activeChild cfg kids)
  Parallel rs -> all (inFinalState cfg) (NE.toList rs)

-- Running -------------------------------------------------------------------

-- | Enter the chart's initial state, then process whatever that raises.
start :: Monad m => Chart -> Callbacks m -> m Configuration
start ch cbs = do
  let entered = enter (NE.head (chartRoot ch))
      cfg = enConfig entered
  raised <- runEntries cbs cfg Nothing (enEntered entered)
  runToCompletion ch cbs cfg raised

-- | Process one external event. 'Nothing' if no transition was enabled for it.
macrostep :: Monad m => Chart -> Callbacks m -> Configuration -> Text -> m (Maybe Configuration)
macrostep ch cbs cfg ev = do
  r <- microstep ch cbs cfg ev
  case r of
    Nothing -> pure Nothing
    Just (cfg', raised) -> Just <$> runToCompletion ch cbs cfg' raised

-- | Process raised events in order until the queue is empty. One that no
-- transition handles is dropped.
runToCompletion :: Monad m => Chart -> Callbacks m -> Configuration -> [Text] -> m Configuration
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
microstep :: Monad m => Chart -> Callbacks m -> Configuration -> Text -> m (Maybe (Configuration, [Text]))
microstep ch cbs cfg ev =
  case activeChild cfg (chartRoot ch) of
    Nothing -> pure Nothing
    Just active ->
      let below = offer cfg ev active
       in if not (rpConsumed below)
            then pure Nothing
            else do
              let (cfg', exited, entered) = case rpMove below of
                    Nothing -> (rpConfig below, rpExited below, rpEntered below)
                    Just tgt ->
                      let e = enter (childNamed tgt (chartRoot ch))
                       in (enConfig e, exiting cfg active, enEntered e)
              mapM_ (runExits cbs cfg ev) exited
              raised <- runEntries cbs cfg' (Just ev) entered
              pure (Just (cfg', raised))

-- | Exit callbacks see the state being left, so they get the old configuration.
runExits :: Monad m => Callbacks m -> Configuration -> Text -> Node -> m ()
runExits cbs cfg ev n =
  mapM_ (\a -> runCallback cbs OnExit a cfg (Just ev)) (nodeOnExit n)

-- | Entry callbacks see the configuration the step settles in, so they all get
-- the new one even though they run outermost first.
runEntries :: Monad m => Callbacks m -> Configuration -> Maybe Text -> [(Node, [Text])] -> m [Text]
runEntries cbs cfg ev = fmap concat . mapM one
  where
    one (n, dones) = do
      raised <- mapM (\a -> runCallback cbs OnEntry a cfg ev) (nodeOnEntry n)
      pure ([r | Just r <- raised] ++ dones)
