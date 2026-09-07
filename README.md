# scxml-statecharts

Define a [statechart](https://statecharts.dev/) in SCXML inside a Haskell
module and get typed states, events and a step function out of it.

```haskell
{-# LANGUAGE QuasiQuotes #-}
module Order.Fsm where
import Statechart
import Control.Monad.Trans.State.Strict (StateT, gets, modify')

[scxml|
<scxml name="order-v1" initial="Draft">
  <state id="Draft">
    <transition event="Submit" target="Validating"/>
  </state>
  <state id="Validating">
    <onentry><script>validate</script></onentry>
    <transition event="Valid" target="Processing"/>
    <transition event="Invalid" target="Rejected"/>
  </state>
  <state id="Processing" initial="Authorizing">
    <onentry><script>reserveStock</script></onentry>
    <onexit><script>releaseStock</script></onexit>
    <state id="Authorizing">
      <onentry><script>checkPrepayment</script></onentry>
      <transition event="Poll" target="Authorizing"/>
      <transition event="PaymentAuthorized" target="Fulfilment"/>
    </state>
    <parallel id="Fulfilment">
      <state id="Shipping" initial="Packing">
        <state id="Packing"><transition event="Packed" target="Shipped"/></state>
        <final id="Shipped"/>
      </state>
      <state id="Invoicing" initial="Unpaid">
        <state id="Unpaid"><transition event="Paid" target="Settled"/></state>
        <final id="Settled"/>
      </state>
      <transition event="done.state.Fulfilment" target="Completed"/>
    </parallel>
    <transition event="Cancel" target="Cancelled"/>
  </state>
  <final id="Completed"><onentry><script>notifyCustomer</script></onentry></final>
  <final id="Rejected"/>
  <final id="Cancelled"/>
</scxml>
|]

-- Signatures for the generated functions are optional; they go *after* the
-- quasiquote, like everything else that refers to the generated types.
initiateStateMachine :: StateT Shop IO FsmState
notifyStateMachine   :: FsmState -> FsmEvent -> StateT Shop IO FsmState

-- The callbacks named in <script>. They must all be in one monad, and the
-- type checker enforces it. Each sees the state it observes and the event
-- being processed (Nothing during initiateStateMachine).
validate, reserveStock, notifyCustomer
  :: FsmState -> Maybe FsmEvent -> StateT Shop IO (Maybe FsmEvent)
validate _ _ = gets (\shop -> Just (if null (items shop) then Invalid else Valid))

releaseStock :: FsmState -> Maybe FsmEvent -> StateT Shop IO ()
...
```

The quasiquote generates fixed names, so **one chart per module**:

```haskell
data FsmState   = Draft | Validating | Processing Processing | Completed | Rejected | Cancelled
data Processing = Authorizing | Fulfilment Shipping Invoicing
data Shipping   = Packing | Shipped
data Invoicing  = Unpaid | Settled
data FsmEvent   = Submit | Valid | Invalid | Poll | PaymentAuthorized | Packed | Paid
                | DoneFulfilment | Cancel | DoneShipping | DoneInvoicing
fsmChart :: Def FsmState FsmEvent
initiateStateMachine   -- enter the initial state, running its entry callbacks
notifyStateMachine     -- deliver one event
```

Import the module qualified (`import qualified Order.Fsm as Order`) and every
chart in the codebase presents the same API: `Order.notifyStateMachine`,
`Shipment.notifyStateMachine`. A compound state's type has the same name as
its constructor, which Haskell allows since types and constructors live in
separate namespaces.

Names are used verbatim: what you read in the XML is what you type in
Haskell. State ids and event names must therefore be valid constructor names
(`PaymentAuthorized`, not `payment.authorized`); the quasiquoter rejects
anything else at compile time. The one exception is SCXML's automatic
completion event `done.state.X`, which becomes `DoneX`. The `name` attribute
is optional metadata, kept in `chartName (defChart fsmChart)` for logging and
persistence, and does not affect the generated names.

### Why plain functions and not a class

`Def FsmState FsmEvent` is a first-class value holding everything a
`StateMachine` class would provide, so generic code (a persistence layer, a
test harness, a renderer) takes a `Def` as an argument instead of a
constraint. Fixed names already give the uniform API a class would, and plain
functions leave the monad free: `initiateStateMachine :: MonadIO m => m FsmState`
works exactly as well as a concrete `StateT Shop IO`. A class instance would
have to name one monad in its head, and the quasiquoter has no way to know
which one you want.

## Callbacks

`<script>name</script>` inside `<onentry>` or `<onexit>` names a Haskell
function defined in the same module (after the quasiquote).

```haskell
-- onentry: may decide where to go next by raising an event
name :: FsmState -> Maybe FsmEvent -> m (Maybe FsmEvent)
-- onexit: cleanup only, enforced by the generated code
name :: FsmState -> Maybe FsmEvent -> m ()
```

There is one signature per phase, so an entry callback that raises nothing
still ends in `pure Nothing`.

Entry callbacks receive the state being entered; exit callbacks receive the
state being left. The event is the one being processed, or `Nothing` during
`initiateStateMachine`.

An entry callback returning `Just event` raises that event (SCXML's
`<raise>`). Raised events are queued and processed before
`notifyStateMachine` returns, so an `<onentry>` that inspects data and decides
the chart should move on can do so directly. This replaces SCXML's `cond`
guards, which are deliberately unsupported: a decision becomes a state
(`Validating` above) whose entry callback raises one of the events leading out
of it. That keeps the branching visible in the chart as named events, gives
the decision a state you can observe, and puts the criterion in Haskell where
the data is. A callback can raise at most one event, so contradictory
decisions can't be expressed.

Scripts on transitions are also unsupported. Since a callback receives the
triggering event, an `<onentry>` on the target can do anything a transition
script could, and it does it for every path into that state rather than one.
To act on an event without leaving a state, target the state itself: the
`Poll` self-transition above re-enters `Authorizing` and so re-runs
`checkPrepayment`, which is the polling pattern.

Anything the callbacks need (a database handle, an inventory, the payload of
the current event) lives in the monad, which plays the role of the SCXML
datamodel. `StateT Shop IO` above, `ReaderT Env (ExceptT E IO)`, or a
polymorphic `MonadIO m` all work.

## Validation

The quasiquoter is intolerant by design: a chart that compiles is a chart that
runs. Rejected at compile time, with the position in the XML where available:

- **Anything that is not well-formed XML.** The parser is strict, so a missing
  or mismatched closing tag cannot quietly nest one state inside another:

  ```
  scxml: document is not well-formed XML: 5:1 (91)-5:9 (99):
  Expected end element for: <state>, but received: <scxml>
  ```

- **Transitions that cross levels.** A transition must target a sibling of
  its source. Reaching into another state's interior would bypass its
  `initial` declaration; escaping outward would hide which enclosing state is
  being left. To leave an enclosing state, declare the transition on that
  state, where it applies anywhere inside it:

  ```
  scxml: transition from Authorizing to Rejected crosses levels: Authorizing
  is inside Processing, but Rejected is at the chart root. A transition must
  target a sibling of its source; to leave Processing, declare the transition
  on Processing instead
  ```

  Moving a transition up widens where it applies, which is the trade: on
  `Authorizing`, a declined payment only mattered while authorizing; on
  `Processing`, it applies anywhere inside. To keep it narrow, transition to a
  sibling state whose `<onentry>` raises the event that leaves, the same
  pattern that replaces `cond`.

- **Transitions on a region of a `<parallel>`.** Sibling regions are active at
  the same time, so leaving one would leave the others behind, producing a
  configuration the state type cannot represent. Declare the transition on the
  `<parallel>` itself, or on a state inside the region.

- **Unknown state ids, event names or callback names**, ids and event names
  that are not Haskell constructor names, transitions with no event or no
  target, duplicate ids, `cond`, `type="internal"`, `done.state.X` naming a
  state that can never complete, a `<parallel>` with no regions, an atomic
  state with an `initial`, and unsupported executable content.

This is deliberately stricter than SCXML. Loosening a rule later is a
compatible change; tightening one is not.

## Semantics

`notifyStateMachine` selects the transitions enabled by the event, runs
`<onexit>` callbacks of exited states (innermost first), then `<onentry>`
callbacks of entered states (outermost first). It then processes the
raised-event queue the same way until it is empty, and returns.

An event with no matching transition in the current state is ignored, as in
SCXML: the state is returned unchanged and nothing runs. A poll result that
arrives after the chart has moved on is the typical case. A raised event
nothing handles is dropped too. Cycles of raised events are cut off after 1000
iterations with an error.

Entering a `<final>` state raises `done.state.Parent`, and `done.state.G`
when every region of a parallel grandparent `G` has reached a final state.
This is how a parallel state completes; `done.state.Fulfilment` above.

## Storing a state

Every generated type derives `Show`, `Read`, `Eq` and `Ord`, so `Show`/`Read`
round-trip exactly and are convenient in tests. For a state that outlives the
process, such as one parked in a database between AWS Lambda invocations, use
the state-id list instead:

```haskell
toStateIds   :: Def s ev -> s -> [StateId]
fromStateIds :: Eq s => Def s ev -> [StateId] -> Maybe s

toStateIds fsmChart (Processing (Fulfilment Shipped Unpaid))
  == ["Fulfilment","Invoicing","Processing","Shipped","Shipping","Unpaid"]
```

This is SCXML's own notion of a chart's state, which makes it portable to
another implementation of the same chart, readable in a log, and queryable as
a text array or a JSON array in Postgres.

The array is a set, so nothing positional leaks into it. Order and duplicates
in the input do not matter, and reordering the regions of a `<parallel>` in
the SCXML does not change it: the Haskell field order flips, so
`Fulfilment Shipped Unpaid` becomes `Fulfilment Unpaid Shipped`, but both
serialize to the same array and each chart loads the other's output. Positional
formats such as `Show` do not survive that edit.

Two things not to persist: `Ord` comparisons on states, and `fromEnum` on
events. Both are positional, so adding a state or an event changes them.

`fromStateIds` returns `Maybe` and validates by round-tripping, so an
incomplete set, an unknown id, or a list that merely starts like a valid
configuration are all rejected rather than decoded into some other state.
That matters when a chart is redeployed while states are in flight: a value
stored under the old chart fails loudly, and you migrate it deliberately
instead of discovering later that it silently changed meaning.

JSON is two lines in your own module, so the library does not depend on
`aeson`:

```haskell
instance ToJSON FsmState where
  toJSON = toJSON . toStateIds fsmChart
instance FromJSON FsmState where
  parseJSON v = parseJSON v >>= maybe (fail "stale FsmState") pure . fromStateIds fsmChart
```

### How name resolution works

Generated code refers to `reserveStock` and friends by name. A top-level
splice and the declarations following it form one declaration group in GHC,
so those functions may be defined after the quasiquote (they have to be, if
they mention the generated types). Only declarations *before* the quasiquote
cannot see the generated names.

### Signatures for the generated functions are optional

The quasiquoter cannot write them, because it does not know the monad. It
knows the callbacks only by the names in the XML, and those functions are in
the same declaration group, so they are not type-checked when the splice
runs. `m` comes from them, not from the chart.

You rarely need to write the signatures anyway. When the callbacks are in a
concrete monad, both generated functions are inferred and GHC only asks for
signatures under `-Wmissing-signatures`:

```haskell
notifyStateMachine   :: FsmState -> FsmEvent -> StateT Int IO FsmState  -- inferred
initiateStateMachine :: StateT Int IO FsmState                          -- inferred
```

When the callbacks are polymorphic in their monad, `initiateStateMachine`
takes no arguments and so hits the monomorphism restriction, reported as an
ambiguous type variable. Either give that one binding a signature, or put
`{-# LANGUAGE NoMonomorphismRestriction #-}` on the module; both leave the
rest to inference.

### Callbacks may carry different constraints

They do not all need the same constraint, and each keeps the one it declares.
The generated functions call every callback at a single `m`, so their
constraints union there:

```haskell
announce        :: MonadIO m => FsmState -> Maybe FsmEvent -> m (Maybe FsmEvent)
pureBookkeeping :: Monad m   => FsmState -> Maybe FsmEvent -> m ()
-- inferred, the union of the two:
notifyStateMachine :: MonadIO m => FsmState -> FsmEvent -> m FsmState
```

So `MonadIO` does win for the chart as a whole, but only at that call site.
`pureBookkeeping` keeps its `Monad m` and stays usable elsewhere at a pure
monad such as `Identity`. Declaring the weakest constraint each callback
needs therefore still pays: it documents the effect it has, and keeps the
function reusable outside the chart.

## Type mapping

| SCXML                   | Haskell                                                           |
|-------------------------|-------------------------------------------------------------------|
| atomic / final state    | nullary constructor                                               |
| compound state          | constructor carrying a sum type of the same name                  |
| parallel state          | constructor with one field per compound region                    |
| event name              | constructor of `FsmEvent`                                         |
| `done.state.X`          | constructor `DoneX`                                               |
| the active configuration | `toStateIds` / `fromStateIds`, for storage                       |
| `<script>name</script>` in `<onentry>` | `name :: FsmState -> Maybe FsmEvent -> m (Maybe FsmEvent)` |
| `<script>name</script>` in `<onexit>`  | `name :: FsmState -> Maybe FsmEvent -> m ()`               |

A value of `FsmState` is exactly one legal configuration, so illegal
configurations are unrepresentable and `case` on it is exhaustive.

## Architecture

- `Statechart.Model`: untyped chart (nodes, transitions, document order).
- `Statechart.Parse`: SCXML to `Chart`, with validation (unique ids, known
  targets, initial states are descendants).
- `Statechart.Interpret`: the SCXML Appendix D algorithm on sets of active
  state ids: transition selection with conflict resolution, exit and entry
  sets via the least common compound ancestor, the raised-event queue, and
  `done.state` events.
- `Statechart.Run`: `start`/`step` over a `Def` plus a `Hooks` dispatcher.
- `Statechart.TH`: the `scxml` quasiquoter. Generates the types,
  `toConfig`/`fromConfig` conversions, the `Def` record, and the two
  wrappers whose hooks dispatch to the callbacks named in the SCXML. All
  semantics live in the interpreter; the generated code is only the typed
  shell around it. A fully static transition table per (state, event) is a
  possible later optimisation.

## Deliberately unsupported

- `cond` guards and eventless transitions: raise an event from an entry
  callback instead (see Callbacks).
- `<script>` on a transition, and transitions without a target: use the
  target's `<onentry>`, or a self-transition to act without leaving a state.
- Transitions into another state's interior, and `type="internal"`: see
  Validation.
- Static `<raise event="..."/>`: return the event from a callback instead.
- Event names that aren't Haskell constructor names.
- More than one chart per module (the generated names are fixed).

## Not yet supported

- `<history>` states, wildcard event descriptors (`error.*`, `*`).
- Executable content other than `<script>functionName</script>` (`<assign>`,
  `<send>`, `<if>`, `<log>`) is rejected.
- A `<parallel>` directly inside a `<parallel>`.


## Building

```
cabal build
cabal test
```
