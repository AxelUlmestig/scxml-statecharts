# Changelog

## 0.2.0.0 -- unreleased

- **Breaking.** The `event` attribute now names one event and then the Haskell
  types its constructor holds, so `event="Order Items Int"` declares
  `Order Items Int`. SCXML reads the attribute as a space-separated list of
  event descriptors, and that shorthand is gone: `event="A B"` no longer means
  two transitions with one target, which is written as two `<transition>`
  elements instead. This is the one place the parser knowingly differs from
  the specification.
- The event reaching a callback is now the one the caller passed in rather
  than a value rebuilt from its name, so whatever payload it carries survives
  the trip, and an event a callback raises carries its own. The evaluator is
  parameterised over the event type and still selects transitions by name
  alone, so a payload never decides where the chart goes; that stays with the
  events a callback raises. `Def` trades `defEventFromName :: Text -> Maybe ev`
  for a total `defDoneEvent :: StateId -> ev`, since `done.state` events are
  the only ones the evaluator synthesises and they carry nothing.
- A payload type is one type constructor, optionally module-qualified,
  resolved after the quasiquote like a callback name, so it may be defined
  below it. `Maybe Int`, `[Int]` and tuples cannot be told apart from separate
  fields in an attribute whose parts are separated by spaces, so they go
  through a type alias; writing one directly is rejected with a message naming
  the way in. Allowing them later is a compatible change.
- Every transition naming an event must declare the same payload for it, since
  they all reach the one generated constructor, and a disagreement is a
  compile error naming both places.
- **Breaking.** An event that carries data costs `FsmEvent` its derived `Ord`,
  `Enum` and `Bounded`: `Ord` would demand an instance of every payload type,
  and the other two need every constructor nullary. A chart whose events carry
  nothing derives all of them as before.
- The Nix dev shell gained zlib, which `xml-conduit` reaches through
  `conduit-extra` and `streaming-commons`. Without it `cabal build` compiled
  everything and then failed at the link with `cannot find -lz`, and every
  Template Haskell splice warned about `libz.so`.

## 0.1.0.0 -- 2026-09-09

First release.

- `scxml` declaration quasiquoter generating `FsmState`, `FsmEvent`,
  `fsmChart`, `initiateStateMachine` and `notifyStateMachine`.
- Hierarchy (compound states as sum types), parallel regions (as products),
  `<onentry>` and `<onexit>` callbacks named in the XML, entry callbacks that
  raise events, and SCXML `done.state.X` completion events.
- Unmatched events leave the state unchanged, as in SCXML.
- Compile-time validation: strict XML (via `xml-conduit`), state ids, event
  names, callback names, a level rule requiring a transition to target a
  sibling of its source, and a rule forbidding transitions on a region of a
  `<parallel>`. `initial` is required on every compound state and must name a
  direct child; SCXML's "first child in document order" default is not
  supported.
- The evaluator is a single recursive pass over the tree. Because a transition
  may only target a sibling, a state only ever rearranges its own children, so
  there are no least common ancestors, exit-set filters over the whole
  configuration, or conflicts between transitions at different depths. The
  derived index is gone, along with lookup by id, parent links and document
  order. Innermost-wins is now explicit: children are asked first and a state
  acts only if nothing below it did.
- A `<final>` state is rejected as a direct region of a `<parallel>`. SCXML
  does not allow it, and it used to report the whole parallel complete before
  the other regions had run.
- The chart is a tree: a state owns its children as `Node`s, and a compound
  state keeps its initial child first, so a dangling child, a disagreeing
  parent and an initial child that is not one of its own are all
  unrepresentable. Parent and document order are derived into an index rather
  than stored. A compound state's initial child is therefore its first
  generated constructor, which changes derived `Ord` for charts that do not
  write the initial state first.
  Only the interpreter needs the derived index; the parser and the generator
  walk the tree, so a transition target is checked against the source's
  siblings rather than by looking up parents.
- `done.state.X` may only be handled on `X` itself, so completion climbs one
  level at a time through `<final>` states and validation is entirely local to
  a state and its neighbours.
- A transition on an enclosing state acts as a default that an inner state can
  override, since the innermost matching transition wins and only it is taken.
  Documented rather than warned about: a Template Haskell warning becomes an
  error under `-Werror` and cannot be exempted by flag.
- The public API is the `scxml` quasiquoter alone. `serializeStateMachine` and
  `deserializeStateMachine` are generated into the calling module, so a chart
  needs no other import. The remaining modules are not exposed.
- Transitions are a map from event name to target, so two transitions on one
  state for the same event are unrepresentable, and document order never
  decides which transition is taken. A transition naming more than one target
  is rejected; both previously compiled and silently produced a wrong state.
- `Kind` carries each state's children, so an atomic or final state with
  children, a compound state without an initial child, and a `<parallel>`
  without regions are all unrepresentable rather than merely rejected.
- `Show`/`Read` on every generated type, and `toStateIds`/`fromStateIds` for
  storing a state outside Haskell as the SCXML configuration.
