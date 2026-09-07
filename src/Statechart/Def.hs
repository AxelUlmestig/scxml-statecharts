-- | The typed chart definition that generated code produces.
module Statechart.Def (Def (..)) where

import Data.Set (Set)
import Data.Text (Text)

import Statechart.Model (Chart, StateId)

-- | Ties a chart's generated types together with the untyped t'Chart' the
-- interpreter runs. @s@ is the state type and @ev@ the event type. The @scxml@
-- quasiquoter generates one of these per chart.
data Def s ev = Def
  { defChart         :: Chart
  , defEventName     :: ev -> Text
  , defEventFromName :: Text -> ev
  , defToConfig      :: s -> Set StateId
    -- ^ the set of active state ids described by a typed state
  , defFromConfig    :: Set StateId -> Maybe s
    -- ^ rebuild the typed state from a configuration produced by the interpreter
  }
