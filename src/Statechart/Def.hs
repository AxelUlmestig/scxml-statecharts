-- | The typed chart definition that generated code produces.
module Statechart.Def (Def (..)) where

import Data.Set (Set)
import Data.Text (Text)

import Statechart.Model (Index, StateId)

-- | Ties a chart's generated types together with the untyped chart the
-- interpreter runs. @s@ is the state type and @ev@ the event type. The @scxml@
-- quasiquoter generates one of these per chart.
data Def s ev = Def
  { defIndex         :: Index
    -- ^ the chart, indexed for lookup. Built once from the lifted tree.
  , defEventName     :: ev -> Text
  , defEventFromName :: Text -> Maybe ev
    -- ^ total: 'Nothing' means the generated event type has no constructor
    -- for that name, which the generator makes impossible for names the
    -- interpreter can produce
  , defToConfig      :: s -> Set StateId
    -- ^ the set of active state ids described by a typed state
  , defFromConfig    :: Set StateId -> Maybe s
    -- ^ rebuild the typed state from a configuration produced by the interpreter
  }
