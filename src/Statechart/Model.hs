{-# LANGUAGE DeriveLift #-}
-- | The untyped statechart model. This is what the SCXML parser produces and
-- what the generic interpreter runs on. The Template Haskell layer generates
-- typed wrappers around it.
module Statechart.Model
  ( StateId
  , Kind (..)
  , Transition (..)
  , Node (..)
  , Chart (..)
  , nodeChildren
  , childrenOfKind
  , isParallel
  , nodeOf
  , kindOf
  , orderOf
  , properAncestors
  , isDescendantOf
  , lcca
  , allNodes
  , allTransitions
  ) where

import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.Syntax (Lift)

-- | A state's @id@ attribute, which is also its Haskell constructor name.
type StateId = Text

-- | What kind of state a node is, together with its children, so that a
-- state cannot have the wrong ones:
--
-- * 'Atomic' is a leaf and 'Final' is terminal, so neither has children.
-- * 'Compound' has children of which exactly one is active, and carries the
--   one that entering it leads to.
-- * 'Parallel' has regions, all of which are active at once, so there is no
--   initial one to choose.
--
-- The parser still enforces what the type cannot: that a compound state's
-- initial child is one of its own children, that a final state has no
-- transitions, and that every id named here exists in the chart.
data Kind
  = Atomic
  | Compound (NonEmpty StateId) StateId
  | Parallel (NonEmpty StateId)
  | Final
  deriving (Eq, Ord, Show, Lift)

-- | Child states of a kind, in document order.
childrenOfKind :: Kind -> [StateId]
childrenOfKind Atomic = []
childrenOfKind Final = []
childrenOfKind (Compound cs _) = NE.toList cs
childrenOfKind (Parallel rs) = NE.toList rs

-- | Whether every child is active at once.
isParallel :: Kind -> Bool
isParallel (Parallel _) = True
isParallel _ = False

-- | One @<transition>@ element.
data Transition = Transition
  { trSource   :: StateId
  , trEvents   :: [Text]     -- ^ event names this transition matches (never empty)
  , trTargets  :: [StateId]  -- ^ the states entered (never empty)
  , trOrder    :: Int        -- ^ document order, unique per chart
  }
  deriving (Eq, Ord, Show, Lift)

-- | One state, with its place in the hierarchy resolved.
data Node = Node
  { nodeId          :: StateId
  , nodeKind        :: Kind
  , nodeParent      :: Maybe StateId -- ^ 'Nothing' for children of @<scxml>@
  , nodeTransitions :: [Transition]  -- ^ document order
  , nodeOnEntry     :: [Text]        -- ^ names of @<onentry><script>@ actions
  , nodeOnExit      :: [Text]        -- ^ names of @<onexit><script>@ actions
  , nodeOrder       :: Int           -- ^ pre-order index in the document
  }
  deriving (Eq, Show, Lift)

-- | A whole chart: every state by id, plus the default entry targets of the
-- @<scxml>@ root. This is what the quasiquoter lifts into the generated
-- t'Statechart.Def.Def' and what the interpreter runs.
data Chart = Chart
  { chartName         :: Maybe Text
  , chartRootChildren :: [StateId]
  , chartInitial      :: StateId     -- ^ the child of @<scxml>@ entered first
  , chartNodes        :: Map StateId Node
  }
  deriving (Eq, Show, Lift)

-- | Child states in document order, taken from the node's 'Kind'.
nodeChildren :: Node -> [StateId]
nodeChildren = childrenOfKind . nodeKind

-- | Look up a state. Calls 'error' on an unknown id, which the parser's
-- validation rules out for any chart it accepted.
nodeOf :: Chart -> StateId -> Node
nodeOf ch s = case Map.lookup s (chartNodes ch) of
  Just n  -> n
  Nothing -> error ("Statechart: unknown state id " ++ T.unpack s)

-- | The kind of a state.
kindOf :: Chart -> StateId -> Kind
kindOf ch = nodeKind . nodeOf ch

-- | A state's position in document order, which decides the order entry and
-- exit callbacks run in.
orderOf :: Chart -> StateId -> Int
orderOf ch = nodeOrder . nodeOf ch

-- | Ancestors of a state, nearest first, excluding the @<scxml>@ root.
properAncestors :: Chart -> StateId -> [StateId]
properAncestors ch s = case nodeParent (nodeOf ch s) of
  Nothing -> []
  Just p  -> p : properAncestors ch p

-- | Is the first state a proper descendant of the second? 'Nothing' denotes
-- the @<scxml>@ root, of which every state is a descendant.
isDescendantOf :: Chart -> StateId -> Maybe StateId -> Bool
isDescendantOf _ _ Nothing   = True
isDescendantOf ch s (Just a) = a `elem` properAncestors ch s

-- | Least common compound ancestor. 'Nothing' means the root.
lcca :: Chart -> [StateId] -> Maybe StateId
lcca _ [] = Nothing
lcca ch (s : rest) =
  find (\a -> all (\x -> isDescendantOf ch x (Just a)) rest) (properAncestors ch s)

-- | All nodes in document order.
allNodes :: Chart -> [Node]
allNodes = sortOn nodeOrder . Map.elems . chartNodes

-- | All transitions in document order.
allTransitions :: Chart -> [Transition]
allTransitions = sortOn trOrder . concatMap nodeTransitions . allNodes
