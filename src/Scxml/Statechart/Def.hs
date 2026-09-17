-- | The typed chart definition that generated code produces.
module Scxml.Statechart.Def (Def (..)) where

import Data.Set (Set)
import Data.Text (Text)

import Scxml.Statechart.Model (Chart, StateId)

-- | Ties a chart's generated types together with the untyped chart the
-- interpreter runs. @s@ is the state type and @ev@ the event type. The @scxml@
-- quasiquoter generates one of these per chart.
data Def s ev = Def
  { defChart      :: Chart
  , defEventName  :: ev -> Text
    -- ^ the name a transition matches on. An event's payload never takes part
    -- in selection, so this is all the interpreter needs; the event itself is
    -- carried along beside it.
  , defDoneEvent  :: StateId -> ev
    -- ^ the constructor for a state's @done.state@ event, which the
    -- interpreter raises itself and which therefore carries no payload.
    -- Total: the generator emits one for every state that can complete, which
    -- is every state the interpreter can pass here.
  , defToConfig   :: s -> Set StateId
    -- ^ the set of active state ids described by a typed state
  , defFromConfig :: Set StateId -> Maybe s
    -- ^ rebuild the typed state from a configuration produced by the interpreter
  }
