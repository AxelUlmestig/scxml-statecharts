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
- `Kind` carries each state's children, so an atomic or final state with
  children, a compound state without an initial child, and a `<parallel>`
  without regions are all unrepresentable rather than merely rejected.
- `Show`/`Read` on every generated type, and `toStateIds`/`fromStateIds` for
  storing a state outside Haskell as the SCXML configuration.
