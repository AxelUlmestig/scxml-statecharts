{-# LANGUAGE DeriveLift #-}
-- | The untyped statechart model. This is what the SCXML parser produces and
-- what the generic interpreter runs on. The Template Haskell layer generates
-- typed wrappers around it.
--
-- The chart is a tree: a state owns its children, so a dangling child or a
-- disagreeing parent cannot be represented. Everything relational is derived
-- into an t'Index' instead of stored, so it cannot disagree with the tree
-- either.
module Statechart.Model
  ( -- * The tree
    StateId
  , Kind (..)
  , Node (..)
  , Chart (..)
  , nodeChildren
  , childrenOfKind
  , initialChild
  , isParallel
  , completes
  , chartStates
    -- * The derived index
  , Index
  , index
  , ixInitial
  , nodeOf
  , kindOf
  , orderOf
  , parentOf
  , properAncestors
  , isDescendantOf
  , lcca
  ) where

import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.Syntax (Lift)

-- | A state's @id@ attribute, which is also its Haskell constructor name.
type StateId = Text

-- | What kind of state a node is, together with the children it owns:
--
-- * 'Atomic' is a leaf and 'Final' is terminal, so neither has children.
-- * 'Compound' has children of which exactly one is active. The first is the
--   one entering it leads to, so a compound state cannot lack an initial
--   child or name one that is not its own.
-- * 'Parallel' has regions, all of which are active at once, so there is no
--   initial one to choose. Their order is document order.
data Kind
  = Atomic
  | Compound (NonEmpty Node)
  | Parallel (NonEmpty Node)
  | Final
  deriving (Eq, Show, Lift)

-- | One state and everything inside it.
data Node = Node
  { nodeId          :: StateId
  , nodeKind        :: Kind
  , nodeTransitions :: Map Text StateId
    -- ^ event name to the sibling state it enters. At most one transition per
    -- event, so nothing has to break a tie.
  , nodeOnEntry     :: [Text]        -- ^ names of @<onentry><script>@ callbacks
  , nodeOnExit      :: [Text]        -- ^ names of @<onexit><script>@ callbacks
  }
  deriving (Eq, Show, Lift)

-- | A whole chart. This is what the quasiquoter lifts into the generated code.
data Chart = Chart
  { chartName   :: Maybe Text
  , chartRoot   :: NonEmpty Node -- ^ children of @<scxml>@; the first is entered
  , chartEvents :: [Text]        -- ^ every event a transition names, in document order
  }
  deriving (Eq, Show, Lift)

-- | Children of a state, the first being its initial child when it has one.
nodeChildren :: Node -> [Node]
nodeChildren = childrenOfKind . nodeKind

-- | Children of a kind, the first being its initial child when it has one.
childrenOfKind :: Kind -> [Node]
childrenOfKind Atomic = []
childrenOfKind Final = []
childrenOfKind (Compound cs) = NE.toList cs
childrenOfKind (Parallel rs) = NE.toList rs

-- | The child that entering a compound state leads to.
initialChild :: Kind -> Maybe Node
initialChild (Compound (c :| _)) = Just c
initialChild _ = Nothing

-- | Whether every child is active at once.
isParallel :: Kind -> Bool
isParallel (Parallel _) = True
isParallel _ = False

-- | Can this state ever raise its @done.state@ event? A parallel state can,
-- once every region is final; a compound state needs a @<final>@ child.
completes :: Node -> Bool
completes n = case nodeKind n of
  Parallel _ -> True
  Compound cs -> any ((== Final) . nodeKind) (NE.toList cs)
  _ -> False

-- | Every state in the chart, in document order. Needs no index: it is the
-- pre-order walk of the tree.
chartStates :: Chart -> [Node]
chartStates = concatMap preorder . NE.toList . chartRoot
  where
    preorder n = n : concatMap preorder (nodeChildren n)

-- | The lookups the interpreter needs, derived from the tree once. Parent and
-- document order live here rather than in 'Node' so that they cannot
-- contradict the tree.
--
-- Only the interpreter needs this. The parser and the code generator walk the
-- tree, where a state carries its own children and its siblings are in hand.
data Index = Index
  { ixChart   :: Chart
  , ixNodes   :: Map StateId Node
  , ixParents :: Map StateId StateId
  , ixOrders  :: Map StateId Int
  }

-- | Build the index. Document order is the pre-order walk of the tree, which
-- gives what callback ordering needs: a state before everything inside it, and
-- the regions of a parallel state in the order they are written.
index :: Chart -> Index
index ch =
  Index
    { ixChart = ch
    , ixNodes = Map.fromList [(nodeId n, n) | n <- ordered]
    , ixParents = Map.fromList (concatMap (parents Nothing) roots)
    , ixOrders = Map.fromList (zip (map nodeId ordered) [0 ..])
    }
  where
    roots = NE.toList (chartRoot ch)
    ordered = chartStates ch
    parents p n =
      [(nodeId n, q) | Just q <- [p]] ++ concatMap (parents (Just (nodeId n))) (nodeChildren n)

-- | The state the chart starts in. Cheap enough to read from the tree, so it
-- is not cached alongside the maps.
ixInitial :: Index -> StateId
ixInitial = nodeId . NE.head . chartRoot . ixChart

nodeOf :: Index -> StateId -> Node
nodeOf ix s = case Map.lookup s (ixNodes ix) of
  Just n -> n
  Nothing -> error ("Statechart: unknown state id " ++ T.unpack s)

kindOf :: Index -> StateId -> Kind
kindOf ix = nodeKind . nodeOf ix

-- | A state's position in document order, which decides the order entry and
-- exit callbacks run in.
orderOf :: Index -> StateId -> Int
orderOf ix s = Map.findWithDefault (error ("Statechart: unknown state id " ++ T.unpack s)) s (ixOrders ix)

-- | The enclosing state, or 'Nothing' for a child of @<scxml>@.
parentOf :: Index -> StateId -> Maybe StateId
parentOf ix s = Map.lookup s (ixParents ix)

-- | Ancestors of a state, nearest first, excluding the @<scxml>@ root.
properAncestors :: Index -> StateId -> [StateId]
properAncestors ix s = case parentOf ix s of
  Nothing -> []
  Just p -> p : properAncestors ix p

-- | Is the first state a proper descendant of the second? 'Nothing' denotes
-- the @<scxml>@ root, of which every state is a descendant.
isDescendantOf :: Index -> StateId -> Maybe StateId -> Bool
isDescendantOf _ _ Nothing = True
isDescendantOf ix s (Just a) = a `elem` properAncestors ix s

-- | Least common compound ancestor. 'Nothing' means the root.
lcca :: Index -> [StateId] -> Maybe StateId
lcca _ [] = Nothing
lcca ix (s : rest) =
  find (\a -> all (\x -> isDescendantOf ix x (Just a)) rest) (properAncestors ix s)
