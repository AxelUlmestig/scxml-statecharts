{-# LANGUAGE DeriveLift #-}
-- | The untyped statechart model. This is what the SCXML parser produces and
-- what the evaluator runs on. The Template Haskell layer generates typed
-- wrappers around it.
--
-- The chart is a tree and nothing else: a state owns its children, so a
-- dangling child cannot be represented, and there is no parent link or
-- document index to disagree with the structure. Since a transition may only
-- target a sibling, every consumer works by walking the tree, so none of that
-- would have a reader anyway.
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
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.Text (Text)
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
