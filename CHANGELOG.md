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
  `<parallel>`.
- `Show`/`Read` on every generated type, and `toStateIds`/`fromStateIds` for
  storing a state outside Haskell as the SCXML configuration.
