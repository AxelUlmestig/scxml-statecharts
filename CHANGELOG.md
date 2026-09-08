# Changelog

## 0.1.0.0 -- unreleased

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
