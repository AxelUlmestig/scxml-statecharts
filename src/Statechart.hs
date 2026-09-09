-- | Typed statecharts generated from SCXML. Write the chart as SCXML in a
-- 'scxml' quasiquote at the top level of a module and get the state and event
-- types, and the functions that run them, generated into that module. See
-- 'scxml' below for a worked example.
--
-- A compound state becomes a sum type and a parallel state a product, so a
-- value of the generated @FsmState@ is exactly one legal configuration:
-- illegal states are unrepresentable and @case@ is exhaustive. State ids and
-- event names are used verbatim as constructor names, so the generated names
-- are fixed and a module holds one chart. Callbacks are defined in the same
-- module, after the quasiquote, and must all live in the same monad, which the
-- type checker enforces.
--
-- Signatures for the two generated functions are optional. They are inferred
-- when the callbacks are in a concrete monad; when the callbacks are
-- polymorphic, @initiateStateMachine@ takes no arguments and so needs either
-- a signature or @NoMonomorphismRestriction@.
--
-- Two more functions are generated for storing a state outside Haskell:
--
-- > serializeStateMachine   :: FsmState -> [Text]
-- > deserializeStateMachine :: [Text] -> Maybe FsmState
--
-- This module exports only the quasiquoter. Everything a chart needs is
-- generated into your own module, so there is nothing else to import.
module Statechart (scxml) where

import Statechart.TH (scxml)
