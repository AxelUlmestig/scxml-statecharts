{-# LANGUAGE TemplateHaskell #-}
-- | The @scxml@ quasiquoter. Used at the top level of a module:
--
-- @
-- [scxml| <scxml initial="Draft"> ... </scxml> |]
-- @
--
-- The generated names are fixed, so a module holds one chart:
--
-- * @data FsmState@: one constructor per child of @<scxml>@, named exactly as
--   the state id. A compound state becomes a constructor carrying a sum type
--   of the same name as the state; a parallel state becomes a constructor
--   with one field per compound region; atomic and final states are nullary.
-- * @data FsmEvent@: one constructor per event name, verbatim, plus @DoneX@
--   for SCXML's automatic @done.state.X@ completion events.
-- * @fsmChart :: Def FsmState FsmEvent@, for "Statechart.Run".
-- * @serializeStateMachine :: FsmState -> [Text]@ and
--   @deserializeStateMachine :: [Text] -> Maybe FsmState@, which store a state
--   as the set of active state ids and read it back, rejecting anything that
--   is not a configuration of this chart.
-- * @initiateStateMachine@ and @notifyStateMachine@, which run the chart
--   calling the callbacks named in @<script>@ elements. Callbacks are looked
--   up by name in the module containing the quasiquote (they may be defined
--   after it) and must share one monad, which the type checker enforces:
--
-- @
-- initiateStateMachine :: m FsmState
-- notifyStateMachine   :: FsmState -> FsmEvent -> m FsmState
--
-- entryCallback :: FsmState -> Maybe FsmEvent -> m (Maybe FsmEvent)
-- exitCallback  :: FsmState -> Maybe FsmEvent -> m ()
-- @
--
-- Every generated type derives @Show@, @Read@, @Eq@ and @Ord@, and the event
-- type also derives @Enum@ and @Bounded@. For storing a state outside
-- Haskell, prefer 'Statechart.Run.toStateIds' over @Show@.
module Statechart.TH (scxml) where

import Control.Monad (forM, unless)
import Data.Char (isAlphaNum, isLower, isUpper)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift)

import Statechart.Def
import qualified Statechart.Interpret as I
import Statechart.Model
import Statechart.Parse
import qualified Statechart.Run as Run

-- | The declaration quasiquoter described in this module's documentation.
-- Usable only at the top level of a module.
scxml :: QuasiQuoter
scxml =
  QuasiQuoter
    { quoteExp = const unsupported
    , quotePat = const unsupported
    , quoteType = const unsupported
    , quoteDec = generate
    }
  where
    unsupported = fail "scxml: only usable as a top-level declaration, e.g. [scxml| <scxml ...> |]"

-- | A compound-like node (the root or a @<state>@ with children) becomes its
-- own data type with helpers converting to and from configurations.
data Group = Group
  { gType     :: Name
  , gChildren :: [Node]
  , gTo       :: Name
  , gFrom     :: Name
  }

generate :: String -> Q [Dec]
generate src = do
  ch <- orFail (parseScxml src)
  -- Everything here is a walk of the tree: a node carries its own children,
  -- so nothing needs looking up by id.
  let states = chartStates ch
  let stateT = mkName "FsmState"
      eventT = mkName "FsmEvent"
      defName = mkName "fsmChart"
      startName = mkName "initiateStateMachine"
      stepName = mkName "notifyStateMachine"
      toIdsName = mkName "serializeStateMachine"
      fromIdsName = mkName "deserializeStateMachine"
      -- A compound state's type has the same name as its constructor; Haskell
      -- keeps types and constructors in separate namespaces.
      nameFor sid = mkName (T.unpack sid)
      compounds = [n | n <- states, Compound _ <- [nodeKind n]]

  -- Callback names
  let entryActions = nub (concatMap nodeOnEntry states)
      exitActions = nub (concatMap nodeOnExit states)
  mapM_ (orFail . checkVarName) (nub (entryActions ++ exitActions))

  -- Events: those named in transitions, in document order, then done events of
  -- states that can complete but that no transition mentions.
  let referenced = chartEvents ch
      doneEvents = [I.doneEventName (nodeId n) | n <- states, completes n]
      events = referenced ++ filter (`notElem` referenced) doneEvents
      eventCon e = case T.stripPrefix (T.pack "done.state.") e of
        Just sid -> (mkName ("Done" ++ T.unpack sid), "completion event " ++ show (T.unpack e))
        Nothing -> (mkName (T.unpack e), "event " ++ show (T.unpack e))
      eventCons = map eventCon events

  groups <- forM (Nothing : map Just compounds) $ \g -> do
    let ty = maybe stateT (nameFor . nodeId) g
    to <- newName ("toCfg_" ++ nameBase ty)
    from <- newName ("fromCfg_" ++ nameBase ty)
    let children = maybe (NE.toList (chartRoot ch)) nodeChildren g
    pure (g, Group ty children to from)
  let groupMap = Map.fromList [(nodeId n, grp) | (Just n, grp) <- groups]
  rootGroup <- case groups of
    (_, g) : _ -> pure g
    [] -> fail "scxml: internal error, no root group"
  let groupOf sid = case Map.lookup sid groupMap of
        Just grp -> pure grp
        Nothing -> fail ("scxml: internal error, no group for " ++ T.unpack sid)

  -- Every generated constructor and type, with its origin, so clashes give a
  -- readable error instead of "Multiple declarations".
  let stateCons =
        [ (nameFor (nodeId n), "state " ++ show (T.unpack (nodeId n)))
        | n <- concatMap (gChildren . snd) groups
        ]
      typeNames =
        [(stateT, "the state type"), (eventT, "the event type")]
          ++ [(gType grp, "compound state " ++ show (T.unpack (nodeId n))) | (Just n, grp) <- groups]
  checkClashes "constructor" (stateCons ++ eventCons)
  checkClashes "type" typeNames

  stateDecs <- concat <$> mapM (groupDecs nameFor groupOf . snd) groups
  eventDec <-
    dataD (cxt []) eventT [] Nothing [normalC c [] | (c, _) <- eventCons]
      [derivClause Nothing (map conT (if null eventCons then [''Show, ''Read, ''Eq, ''Ord] else [''Show, ''Read, ''Eq, ''Ord, ''Enum, ''Bounded]))]

  let eventNameE
        | null eventCons = [| \_ -> error "eventName: chart has no events" |]
        | otherwise = lamCaseE [match (conP c []) (normalB (lift e)) [] | ((c, _), e) <- zip eventCons events]
      eventFromNameE = do
        t <- newName "t"
        lam1E (varP t) $
          foldr
            (\((c, _), e) rest -> [| if $(varE t) == $(lift e) then Just $(conE c) else $rest |])
            [| Nothing |]
            (zip eventCons events)

  defSig <- sigD defName [t| Def $(conT stateT) $(conT eventT) |]
  defDec <-
    valD (varP defName)
      (normalB
        [| Def
             { defChart = $(lift ch)
             , defEventName = $eventNameE
             , defEventFromName = $eventFromNameE
             , defToConfig = $(varE (gTo rootGroup))
             , defFromConfig = $(varE (gFrom rootGroup))
             } |])
      []

  -- Hooks dispatching to the callbacks named in the SCXML. Entry callbacks
  -- return m (Maybe FsmEvent), exit callbacks m (). Inlined into each
  -- generated function rather than shared, so a polymorphic monad does not
  -- hit the monomorphism restriction.
  -- Underscore-prefixed so that a chart with no callbacks of some phase does
  -- not emit an unused-match warning in the user's module.
  let hooksE = do
        phase <- newName "_phase"
        name <- newName "_name"
        st <- newName "_st"
        ev <- newName "_ev"
        let call fn = [| $(varE (mkName (T.unpack fn))) $(varE st) $(varE ev) |]
            entryChain =
              foldr (\fn rest -> [| if $(varE name) == $(lift fn) then Run.entryAction $(call fn) else $rest |])
                    [| pure Nothing |] entryActions
            exitChain =
              foldr (\fn rest -> [| if $(varE name) == $(lift fn) then Run.exitAction $(call fn) else $rest |])
                    [| pure () |] exitActions
        body <- caseE (varE phase)
          [ match (conP 'Run.OnExit []) (normalB [| $exitChain >> pure Nothing |]) []
          , match (conP 'Run.OnEntry []) (normalB entryChain) []
          ]
        [| Run.Hooks $(lamE [varP phase, varP name, varP st, varP ev] (pure body)) |]
  startDec <- valD (varP startName) (normalB [| Run.start $(varE defName) $hooksE |]) []
  stepDec <- do
    st <- newName "st"
    ev <- newName "ev"
    funD stepName [clause [varP st, varP ev] (normalB [| Run.stepOrStay $(varE defName) $hooksE $(varE st) $(varE ev) |]) []]

  -- Storing a state outside Haskell, as the set of active state ids. Generated
  -- rather than exported so that a chart needs no imports beyond the
  -- quasiquoter itself.
  let toCfg = varE (gTo rootGroup)
      fromCfg = varE (gFrom rootGroup)
  toIdsSig <- sigD toIdsName [t| $(conT stateT) -> [Text] |]
  toIdsDec <- do
    x <- newName "st"
    funD toIdsName [clause [varP x] (normalB [| Set.toAscList ($toCfg $(varE x)) |]) []]
  fromIdsSig <- sigD fromIdsName [t| [Text] -> Maybe $(conT stateT) |]
  fromIdsDec <- do
    ids <- newName "ids"
    given <- newName "given"
    st <- newName "st"
    let body =
          [| let $(varP given) = Set.fromList $(varE ids)
              in case $fromCfg $(varE given) of
                   -- Round-trip so that a set which merely starts like a valid
                   -- one, or is missing part of a configuration, is rejected.
                   Just $(varP st) | $toCfg $(varE st) == $(varE given) -> Just $(varE st)
                   _ -> Nothing |]
    funD fromIdsName [clause [varP ids] (normalB body) []]

  pure (stateDecs ++ [eventDec, defSig, defDec, startDec, stepDec, toIdsSig, toIdsDec, fromIdsSig, fromIdsDec])

-- | Data type plus configuration conversions for one compound-like node.
groupDecs :: (StateId -> Name) -> (StateId -> Q Group) -> Group -> Q [Dec]
groupDecs nameFor groupOf grp = do
  shapes <- mapM childShape (gChildren grp)
  let dataDec =
        dataD (cxt []) (gType grp) [] Nothing
          [normalC con [bangType (bang noSourceUnpackedness noSourceStrictness) (conT f) | f <- fields] | (con, fields, _, _) <- shapes]
          [derivClause Nothing (map conT [''Show, ''Read, ''Eq, ''Ord])]
      toDec = do
        x <- newName "x"
        alts <- forM shapes $ \(con, fields, toE, _) -> do
          vars <- mapM (const (newName "r")) fields
          match (conP con (map varP vars)) (normalB (toE vars)) []
        funD (gTo grp) [clause [varP x] (normalB (caseE (varE x) (map pure alts))) []]
      fromDec = do
        cfg <- newName "cfg"
        let body =
              foldr
                (\(n, (_, _, _, rebuild)) rest -> [| if Set.member $(lift (nodeId n)) $(varE cfg) then $(rebuild cfg) else $rest |])
                [| Nothing |]
                (zip (gChildren grp) shapes)
        funD (gFrom grp) [clause [varP cfg] (normalB body) []]
  sequence
    [ dataDec
    , sigD (gTo grp) [t| $(conT (gType grp)) -> Set.Set Text |]
    , toDec
    , sigD (gFrom grp) [t| Set.Set Text -> Maybe $(conT (gType grp)) |]
    , fromDec
    ]
  where
    -- For one child: (constructor, field types, config-of-fields, rebuild-from-config)
    childShape :: Node -> Q (Name, [Name], [Name] -> Q Exp, Name -> Q Exp)
    childShape node = do
      let sid = nodeId node
          con = nameFor sid
      case nodeKind node of
        Compound _ -> do
          sub <- groupOf sid
          pure
            ( con
            , [gType sub]
            , \vs -> case vs of
                [v] -> [| Set.insert $(lift sid) ($(varE (gTo sub)) $(varE v)) |]
                _ -> fail "scxml: internal error, compound state expects exactly one field"
            , \cfg -> [| fmap $(conE con) ($(varE (gFrom sub)) $(varE cfg)) |]
            )
        Parallel regionNodes -> do
          regions <- forM (NE.toList regionNodes) $ \rn -> case nodeKind rn of
            Compound _ -> Just <$> groupOf (nodeId rn)
            Parallel _ ->
              fail $
                "scxml: <parallel> " ++ T.unpack (nodeId rn) ++ " is directly inside another <parallel>, which is not supported."
                  ++ " Flatten it: its regions can become regions of the outer <parallel>, since all of them are active at once either way."
                  ++ " The only thing flattening loses is a done.state event for the inner one on its own."
            _ -> pure Nothing
          let regionIds = map nodeId (NE.toList regionNodes)
              fieldTypes = [gType g | Just g <- regions]
              toE vs =
                let go [] _ = []
                    go ((r, Nothing) : rest) vars = [| Set.singleton $(lift r) |] : go rest vars
                    go ((r, Just g) : rest) (v : vars) = [| Set.insert $(lift r) ($(varE (gTo g)) $(varE v)) |] : go rest vars
                    go _ [] = error "scxml: internal error, region/field mismatch"
                 in [| Set.insert $(lift sid) (Set.unions $(listE (go (zip regionIds regions) vs))) |]
              rebuild cfg = foldl (\acc g -> [| $acc <*> $(varE (gFrom g)) $(varE cfg) |]) [| pure $(conE con) |] [g | Just g <- regions]
          pure (con, fieldTypes, toE, rebuild)
        _ ->
          pure
            ( con
            , []
            , \_ -> [| Set.singleton $(lift sid) |]
            , \_ -> [| Just $(conE con) |]
            )

checkClashes :: String -> [(Name, String)] -> Q ()
checkClashes what named = do
  let byName = Map.fromListWith (++) [(nameBase n, [origin]) | (n, origin) <- named]
      clashes = [(n, os) | (n, os) <- Map.toList byName, length os > 1]
  unless (null clashes) $
    fail $ unlines $
      ("scxml: generated " ++ what ++ " names clash; rename one of the SCXML identifiers:")
        : ["  " ++ n ++ " would be generated for " ++ commaList (reverse os) | (n, os) <- clashes]
  where
    commaList [a, b] = a ++ " and " ++ b
    commaList xs = foldr1 (\a b -> a ++ ", " ++ b) xs

-- | Callback names in @<script>@ must be Haskell variable names, optionally
-- qualified (@Inventory.reserve@).
checkVarName :: Text -> Either String ()
checkVarName raw
  | ok = Right ()
  | otherwise = Left ("<script> callback " ++ show (T.unpack raw) ++ " is not a Haskell function name (expected something like reserveStock or Inventory.reserve)")
  where
    segments = T.splitOn (T.pack ".") raw
    ok = not (null segments) && all isModulePart (init segments) && isVar (last segments)
    isVar t = case T.unpack t of
      c : cs -> (isLower c || c == '_') && all (\x -> isAlphaNum x || x == '_' || x == '\'') cs
      [] -> False
    isModulePart t = case T.unpack t of
      c : cs -> isUpper c && all (\x -> isAlphaNum x || x == '_' || x == '\'') cs
      [] -> False

orFail :: Either String a -> Q a
orFail = either (fail . ("scxml: " ++)) pure
