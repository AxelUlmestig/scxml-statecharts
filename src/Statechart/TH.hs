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
  , gChildren :: [StateId]
  , gTo       :: Name
  , gFrom     :: Name
  }

generate :: String -> Q [Dec]
generate src = do
  ch <- orFail (parseScxml src)
  let stateT = mkName "FsmState"
      eventT = mkName "FsmEvent"
      defName = mkName "fsmChart"
      startName = mkName "initiateStateMachine"
      stepName = mkName "notifyStateMachine"
      -- A compound state's type has the same name as its constructor; Haskell
      -- keeps types and constructors in separate namespaces.
      nameFor sid = mkName (T.unpack sid)
      compounds = [nodeId n | n <- allNodes ch, Compound _ _ <- [nodeKind n]]

  -- Callback names
  let entryActions = nub (concatMap nodeOnEntry (allNodes ch))
      exitActions = nub (concatMap nodeOnExit (allNodes ch))
  mapM_ (orFail . checkVarName) (nub (entryActions ++ exitActions))

  -- Events: those named in transitions, in document order, then done events of
  -- states that can complete but that no transition mentions.
  let referenced = nub (concatMap trEvents (allTransitions ch))
      doneEvents = [I.doneEventName (nodeId n) | n <- allNodes ch, I.canComplete ch (nodeId n)]
      events = referenced ++ filter (`notElem` referenced) doneEvents
      eventCon e = case T.stripPrefix (T.pack "done.state.") e of
        Just sid -> (mkName ("Done" ++ T.unpack sid), "completion event " ++ show (T.unpack e))
        Nothing -> (mkName (T.unpack e), "event " ++ show (T.unpack e))
      eventCons = map eventCon events

  groups <- forM (Nothing : map Just compounds) $ \g -> do
    let ty = maybe stateT nameFor g
    to <- newName ("toCfg_" ++ nameBase ty)
    from <- newName ("fromCfg_" ++ nameBase ty)
    let children = maybe (chartRootChildren ch) (nodeChildren . nodeOf ch) g
    pure (g, Group ty children to from)
  let groupMap = Map.fromList [(sid, grp) | (Just sid, grp) <- groups]
  rootGroup <- case groups of
    (_, g) : _ -> pure g
    [] -> fail "scxml: internal error, no root group"
  let groupOf sid = case Map.lookup sid groupMap of
        Just grp -> pure grp
        Nothing -> fail ("scxml: internal error, no group for " ++ T.unpack sid)

  -- Every generated constructor and type, with its origin, so clashes give a
  -- readable error instead of "Multiple declarations".
  let stateCons = [(nameFor sid, "state " ++ show (T.unpack sid)) | sid <- concatMap (gChildren . snd) groups]
      typeNames =
        [(stateT, "the state type"), (eventT, "the event type")]
          ++ [(gType grp, "compound state " ++ show (T.unpack sid)) | (Just sid, grp) <- groups]
  checkClashes "constructor" (stateCons ++ eventCons)
  checkClashes "type" typeNames

  stateDecs <- concat <$> mapM (groupDecs ch nameFor groupOf . snd) groups
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
            (\((c, _), e) rest -> [| if $(varE t) == $(lift e) then $(conE c) else $rest |])
            [| error ("eventFromName: unknown event " ++ T.unpack $(varE t)) |]
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
  let hooksE = do
        phase <- newName "phase"
        name <- newName "name"
        st <- newName "st"
        ev <- newName "ev"
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

  pure (stateDecs ++ [eventDec, defSig, defDec, startDec, stepDec])

-- | Data type plus configuration conversions for one compound-like node.
groupDecs :: Chart -> (StateId -> Name) -> (StateId -> Q Group) -> Group -> Q [Dec]
groupDecs ch nameFor groupOf grp = do
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
                (\(sid, (_, _, _, rebuild)) rest -> [| if Set.member $(lift sid) $(varE cfg) then $(rebuild cfg) else $rest |])
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
    childShape :: StateId -> Q (Name, [Name], [Name] -> Q Exp, Name -> Q Exp)
    childShape sid = do
      let con = nameFor sid
      case kindOf ch sid of
        Compound _ _ -> do
          sub <- groupOf sid
          pure
            ( con
            , [gType sub]
            , \vs -> case vs of
                [v] -> [| Set.insert $(lift sid) ($(varE (gTo sub)) $(varE v)) |]
                _ -> fail "scxml: internal error, compound state expects exactly one field"
            , \cfg -> [| fmap $(conE con) ($(varE (gFrom sub)) $(varE cfg)) |]
            )
        Parallel _ -> do
          regions <- forM (nodeChildren (nodeOf ch sid)) $ \r -> case kindOf ch r of
            Compound _ _ -> Just <$> groupOf r
            Parallel _ -> fail ("scxml: a <parallel> directly inside a <parallel> (" ++ T.unpack r ++ ") is not supported yet")
            _ -> pure Nothing
          let regionIds = nodeChildren (nodeOf ch sid)
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
