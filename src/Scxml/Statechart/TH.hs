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
--   with one field per region, typed like a compound state for a compound
--   region and as a one-constructor type of the region's name for an atomic
--   one; atomic and final states are nullary.
-- * @data FsmEvent@: one constructor per event name, verbatim, carrying the
--   payload types written after the name in the @event@ attribute, plus a
--   nullary @DoneX@ for SCXML's automatic @done.state.X@ completion events
--   of every state that can complete, other than a region, whose completion
--   nothing could react to.
-- * @data FsmTransition@: one step of the chart, a record of
--   @transitionFrom :: FsmState@, @transitionEvent :: FsmEvent@ and
--   @transitionTo :: FsmState@. What an exit callback receives.
-- * @data FsmEntry = FsmStart FsmState | FsmStep FsmTransition@: what an
--   entry callback receives. @FsmStart@ carries the initial state while
--   @initiateStateMachine@ enters it, which leaves no state and has no event.
-- * An instance of "Scxml.Statechart.Fsm"'s @Fsm@ class for @FsmState@ and
--   for each compound state's and atomic region's type, which is what the
--   generated functions run on. A region is entered and left exactly when its
--   parallel state is, so the region's own callbacks run with the parallel
--   state's: on entry the parallel state's, then its regions' in document
--   order, then whatever is inside the regions; on exit the reverse. Callbacks
--   are named in those instances by internal enumerations, one constructor per
--   callback.
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
-- entryCallback :: FsmEntry -> m (Maybe FsmEvent)
-- exitCallback  :: FsmTransition -> m ()
-- @
--
-- Every generated state type derives @Show@, @Read@, @Eq@ and @Ord@, and the
-- event type also derives @Enum@ and @Bounded@ as long as no event carries a
-- payload: @Ord@ would demand an instance of every payload type, and the
-- other two need every constructor nullary. @FsmTransition@ and @FsmEntry@
-- derive @Show@, @Read@ and @Eq@, which every event type has. For storing a state outside
-- Haskell, prefer the generated @serializeStateMachine@ over @Show@.
module Scxml.Statechart.TH (scxml) where

import           Control.Monad              (filterM, forM, forM_, unless)
import           Data.Char                  (isAlphaNum, isLower, isUpper)
import           Data.List                  (find, intercalate, nub)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (lift)

import qualified Scxml.Statechart.Fsm       as Fsm
import           Scxml.Statechart.Model
import           Scxml.Statechart.Parse

-- | Turns SCXML into a statechart. Usable only at the top level of a module,
-- where it declares the state and event types and the functions that run the
-- chart. A light switch, whose two states each announce themselves on entry:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- > module LightSwitch where
-- >
-- > import Scxml.Statechart (scxml)
-- >
-- > [scxml|
-- > <scxml initial="Off">
-- >   <state id="Off">
-- >     <onentry><script>report</script></onentry>
-- >     <transition event="Flip" target="On"/>
-- >   </state>
-- >   <state id="On">
-- >     <onentry><script>report</script></onentry>
-- >     <transition event="Flip" target="Off"/>
-- >   </state>
-- > </scxml>
-- > |]
-- >
-- > report :: FsmEntry -> IO (Maybe FsmEvent)
-- > report (FsmStart st) = print st >> pure Nothing
-- > report (FsmStep step) = print (transitionTo step) >> pure Nothing
--
-- State ids and event names are used verbatim, so that declares @FsmState@
-- with constructors @Off@ and @On@, @FsmEvent@ with @Flip@, and:
--
-- > initiateStateMachine :: IO FsmState
-- > notifyStateMachine   :: FsmState -> FsmEvent -> IO FsmState
--
-- Callbacks are ordinary functions in the same module, written after the
-- quasiquote, and are found by the name in @\<script\>@. An exit callback
-- receives an @FsmTransition@: the state left, the event that caused the
-- step, and the state the step settles in. An entry callback receives an
-- @FsmEntry@, which is @FsmStep@ with that same record, or @FsmStart@ with
-- the initial state while @initiateStateMachine@ enters it:
--
-- > ghci> off <- initiateStateMachine
-- > Off
-- > ghci> on <- notifyStateMachine off Flip
-- > On
--
-- The callbacks all share one monad, which the type checker enforces. @IO@ is
-- enough to print; when a chart has to carry something along, put the
-- callbacks in a @StateT@ over it and the generated functions follow suit:
--
-- > record :: FsmEntry -> StateT [FsmState] IO (Maybe FsmEvent)
-- > record entry = modify (entered :) >> pure Nothing
-- >   where entered = case entry of FsmStart st -> st; FsmStep step -> transitionTo step
-- >
-- > -- initiateStateMachine :: StateT [FsmState] IO FsmState
-- > -- notifyStateMachine   :: FsmState -> FsmEvent -> StateT [FsmState] IO FsmState
--
-- Returning @Just event@ instead of @Nothing@ raises that event, which is how
-- a callback decides where the chart goes next.
--
-- An event may carry data, by writing the Haskell types its constructor holds
-- after its name. @\<transition event="Order Item Int" target="Checking"/\>@
-- declares @Order Item Int@, and the callbacks that transition runs receive
-- the value the caller passed in, payload and all, as @transitionEvent@. A payload
-- type is one type constructor, optionally qualified (@Int@,
-- @Order.LineItem@); like a callback name it is resolved after the
-- quasiquote, so it may be defined below it. Transitions are still selected
-- by event name alone, so a payload never decides where the chart goes; that
-- stays with the events a callback raises.
--
-- The generated names are fixed, so a module holds one chart.
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
  -- The instances generated below have two parameters and associated types,
  -- which GHC checks against the extensions of the module they are spliced
  -- into. Without this it would report each missing one once per instance.
  do
    missing <- filterM (fmap not . isExtEnabled) [MultiParamTypeClasses, TypeFamilies]
    unless (null missing) $
      fail $
        "scxml: the module needs " ++ unwords (map show missing)
          ++ " for the instances the chart runs on. Add {-# LANGUAGE " ++ intercalate ", " (map show missing) ++ " #-}."

  ch <- orFail (parseScxml src)

  (eventsPart, eventCon, eventCount) <- generateEvents ch
  (actionsPart, actions@Actions {aEntryT, aExitT, aEntry, aExit}) <- generateActions ch
  let ctx =
        Ctx
          { cEntryActT = aEntryT
          , cExitActT = aExitT
          , cEventCon = eventCon
          , cEntryCon = \fn -> maybe (error "scxml: internal error, unknown entry callback") id (lookup fn aEntry)
          , cExitCon = \fn -> maybe (error "scxml: internal error, unknown exit callback") id (lookup fn aExit)
          , cEventCount = eventCount
          }
  (statesPart, root) <- generateStates ch ctx
  runningPart <- generateRunning actions
  serializationPart <- generateSerialization root

  -- Nothing reaches the user's module until this returns, so a clash between
  -- the parts' names is caught here, with a readable error instead of GHC's
  -- "Multiple declarations". The order of the parts is the order a clash
  -- lists where its names came from; the declarations' order does not matter.
  let Part {partDecs, partCons, partTypes} = mconcat [runningPart, statesPart, eventsPart, actionsPart, serializationPart]
  checkClashes "constructor" partCons
  checkClashes "type" partTypes
  pure partDecs

-- | What one part of the chart generates: its declarations, and the
-- constructors and types among them, each with where it came from, for
-- 'generate' to check for clashes across all parts.
data Part = Part
  { partDecs  :: [Dec]
  , partCons  :: [(Name, String)]
  , partTypes :: [(Name, String)]
  }

instance Semigroup Part where
  Part decs cons types <> Part decs' cons' types' = Part (decs ++ decs') (cons ++ cons') (types ++ types')

instance Monoid Part where
  mempty = Part [] [] []

stateT, eventT :: Name
stateT = mkName "FsmState"
eventT = mkName "FsmEvent"

-- | The constructor of a state, and the type of a compound state or atomic
-- region. The two share a name; Haskell keeps types and constructors in
-- separate namespaces.
nameFor :: StateId -> Name
nameFor = mkName . T.unpack

-- | Every region of every parallel state.
chartRegions :: Chart -> [Node]
chartRegions ch = [r | Node {nodeKind = Parallel rs} <- chartStates ch, r <- NE.toList rs]

-- | @FsmEvent@: the events named in transitions, in document order, then the
-- done events of states that can complete but that no transition mentions. A
-- region cannot have transitions, so nothing could react to its done event.
-- Also returns each event's constructor by its SCXML name, and how many
-- events there are.
generateEvents :: Chart -> Q (Part, Text -> Q Name, Int)
generateEvents ch = do
  let referenced = chartEvents ch
      completing = [n | n <- chartStates ch, completes n, nodeId n `notElem` map nodeId (chartRegions ch)]
      doneEvents = [Event (T.pack "done.state." <> nodeId) [] | Node {nodeId} <- completing]
      events = referenced ++ filter ((`notElem` map eventName referenced) . eventName) doneEvents
      -- (constructor, payload type names, what it came from, wire name)
      eventInfo Event {eventName = name, eventFields} =
        case T.stripPrefix (T.pack "done.state.") name of
          Just sid -> (mkName ("Done" ++ T.unpack sid), [], "completion event " ++ show (T.unpack name), name)
          Nothing -> (mkName (T.unpack name), eventFields, "event " ++ show (T.unpack name), name)
      eventInfos = map eventInfo events
      -- Enum and Bounded need every constructor nullary, and Ord would make
      -- the whole chart fail to compile over a payload type that has no
      -- instance, so an event carrying data costs all three.
      eventDerivs
        | null eventInfos = [''Show, ''Read, ''Eq, ''Ord]
        | any (\(_, fs, _, _) -> not (null fs)) eventInfos = [''Show, ''Read, ''Eq]
        | otherwise = [''Show, ''Read, ''Eq, ''Ord, ''Enum, ''Bounded]
  -- An event spelled like a completion event is almost always meant as one,
  -- and the clash it would cause says nothing about done.state.
  forM_ [(name, nodeId) | Event {eventName = name} <- referenced, Node {nodeId} <- completing, name == T.pack "Done" <> nodeId] $ \(name, sid) ->
    fail $
      "scxml: event " ++ show (T.unpack name) ++ " has the name generated for " ++ T.unpack sid
        ++ "'s completion event. To react to " ++ T.unpack sid ++ " completing, write event=\"done.state."
        ++ T.unpack sid ++ "\"; otherwise rename the event."
  eventDec <-
    dataD (cxt []) eventT [] Nothing
      [ normalC c [bangType (bang noSourceUnpackedness noSourceStrictness) (conT (mkName (T.unpack f))) | f <- fs]
      | (c, fs, _, _) <- eventInfos
      ]
      [derivClause Nothing (map conT eventDerivs)]
  let cons = Map.fromList [(name, c) | (c, _, _, name) <- eventInfos]
      conOf name = maybe (fail ("scxml: internal error, no constructor for event " ++ T.unpack name)) pure (Map.lookup name cons)
  pure
    ( Part [eventDec] [(c, origin) | (c, _, origin, _) <- eventInfos] [(eventT, "the event type")]
    , conOf
    , length eventInfos
    )

-- | The two enumerations that name callbacks in the instances, one
-- constructor per callback, paired with the callback's name.
data Actions = Actions
  { aEntryT :: Name
  , aExitT  :: Name
  , aEntry  :: [(Text, Name)]
  , aExit   :: [(Text, Name)]
  }

-- | The generated functions map each action constructor to the callback of
-- that name. That keeps the callbacks' monad out of the instances, so it is
-- inferred rather than declared. The names come from newName, so they cannot
-- clash with anything.
generateActions :: Chart -> Q (Part, Actions)
generateActions ch = do
  let entryActions = nub (concatMap nodeOnEntry (chartStates ch))
      exitActions = nub (concatMap nodeOnExit (chartStates ch))
  mapM_ (orFail . checkVarName) (nub (entryActions ++ exitActions))
  entryActT <- newName "FsmEntryAction"
  exitActT <- newName "FsmExitAction"
  entryCons <- forM entryActions $ \fn -> (,) fn <$> newName ("OnEntry_" ++ mangle fn)
  exitCons <- forM exitActions $ \fn -> (,) fn <$> newName ("OnExit_" ++ mangle fn)
  let enumDec ty cons = dataD (cxt []) ty [] Nothing [normalC c [] | (_, c) <- cons] []
  decs <- sequence [enumDec entryActT entryCons, enumDec exitActT exitCons]
  pure (Part decs [] [], Actions entryActT exitActT entryCons exitCons)

-- | @FsmState@ and a type for each compound state and atomic region, each with
-- its configuration conversions and its 'Fsm.Fsm' instance. Also returns the
-- root's group, whose conversions serialization builds on.
generateStates :: Chart -> Ctx -> Q (Part, Group)
generateStates ch ctx = do
  let compounds = [n | n <- chartStates ch, Compound _ <- [nodeKind n]]
      atomicRegions = [r | r@Node {nodeKind = Atomic} <- chartRegions ch]
  -- An atomic region is a field of its parallel state like any other region,
  -- so it gets a type whose only constructor is itself. Its callbacks run with
  -- the parallel state's, so they are left out of that type's instance.
  groups <- forM (Nothing : map Just (compounds ++ atomicRegions)) $ \g -> do
    let ty = maybe stateT (nameFor . nodeId) g
    to <- newName ("toCfg_" ++ nameBase ty)
    from <- newName ("fromCfg_" ++ nameBase ty)
    let children = case g of
          Nothing -> NE.toList (chartRoot ch)
          Just r@Node {nodeKind = Atomic} -> [r {nodeOnEntry = [], nodeOnExit = []}]
          Just n -> nodeChildren n
    pure (g, Group ty children to from)
  let groupMap = Map.fromList [(nodeId, grp) | (Just Node {nodeId}, grp) <- groups]
  root <- case groups of
    (_, g) : _ -> pure g
    []         -> fail "scxml: internal error, no root group"
  let groupOf sid = case Map.lookup sid groupMap of
        Just grp -> pure grp
        Nothing -> fail ("scxml: internal error, no group for " ++ T.unpack sid)
  typeDecs <- concat <$> mapM (groupDecs groupOf . snd) groups
  instances <- mapM (groupInstance ctx . snd) groups
  let cons =
        [ (nameFor nodeId, "state " ++ show (T.unpack nodeId))
        | (_, Group {gChildren}) <- groups
        , Node {nodeId} <- gChildren
        ]
      types =
        (stateT, "the state type")
          : [ (gType, what ++ show (T.unpack nodeId))
            | (Just Node {nodeId, nodeKind}, Group {gType}) <- groups
            , let what = if nodeKind == Atomic then "region " else "compound state "
            ]
  pure (Part (typeDecs ++ instances) cons types, root)

-- | What callbacks receive, and the functions the user calls to run the chart.
generateRunning :: Actions -> Q Part
generateRunning Actions {aEntry, aExit} = do
  let transitionT = mkName "FsmTransition"
      entryT = mkName "FsmEntry"
      startCon = mkName "FsmStart"
      stepCon = mkName "FsmStep"
      fromField = mkName "transitionFrom"
      eventField = mkName "transitionEvent"
      toField = mkName "transitionTo"
      startName = mkName "initiateStateMachine"
      stepName = mkName "notifyStateMachine"
  -- What callbacks receive. A record rather than positional arguments, so a
  -- callback names what it reads and ignores the rest. An exit callback only
  -- runs inside a step, so it gets the record itself; an entry callback may
  -- also run while the chart starts, which leaves nothing and has no event.
  transitionDec <- do
    let field n t = varBangType n (bangType (bang noSourceUnpackedness noSourceStrictness) t)
    dataD (cxt []) transitionT [] Nothing
      [ recC transitionT
          [ field fromField (conT stateT)
          , field eventField (conT eventT)
          , field toField (conT stateT)
          ]
      ]
      [derivClause Nothing (map conT [''Show, ''Read, ''Eq])]
  entryDec <- do
    let plain t = bangType (bang noSourceUnpackedness noSourceStrictness) (conT t)
    dataD (cxt []) entryT [] Nothing
      [normalC startCon [plain stateT], normalC stepCon [plain transitionT]]
      [derivClause Nothing (map conT [''Show, ''Read, ''Eq])]

  -- The dispatchers are inlined into each function rather than shared, so a
  -- polymorphic monad does not hit the monomorphism restriction. What a
  -- callback receives is converted to the chart's own FsmEntry and
  -- FsmTransition, so that a chart needs no imports; that conversion is also
  -- what tells the engine which state type it is running, since the callback
  -- names are shared by every layer and so cannot. A chart with no callbacks
  -- gets the same from the annotations below.
  let transitionOf t =
        [| $(conE transitionT) (Fsm.transitionFrom $t) (Fsm.transitionEvent $t) (Fsm.transitionTo $t) |]
      entryOf e =
        [| case $e of
             Fsm.FsmStart st  -> $(conE startCon) st
             Fsm.FsmStep step -> $(conE stepCon) $(transitionOf [| step |]) |]
      -- Underscore-prefixed so that a chart with no callbacks of some phase
      -- does not emit an unused-match warning in the user's module.
      dispatch cons fallback argOf
        | null cons = [| \_ _ -> $fallback |]
        | otherwise = do
            action <- newName "_action"
            raw <- newName "_raw"
            arg <- newName "arg"
            lamE [varP action, varP raw] $
              letE [valD (varP arg) (normalB (argOf (varE raw))) []] $
                caseE (varE action)
                  [ match (conP c []) (normalB [| $(varE (mkName (T.unpack fn))) $(varE arg) |]) []
                  | (fn, c) <- cons
                  ]
      entryD = dispatch aEntry [| pure Nothing |] entryOf
      exitD = dispatch aExit [| pure () |] transitionOf
  startDec <-
    valD (varP startName)
      (normalB [| fmap (\st -> st :: $(conT stateT)) (Fsm.initiate $entryD $exitD) |])
      []
  stepDec <- do
    st <- newName "st"
    ev <- newName "ev"
    funD stepName
      [ clause [varP st, varP ev]
          (normalB [| Fsm.consumeEvents $entryD $exitD [$(varE ev)] ($(varE st) :: $(conT stateT)) |])
          []
      ]
  pure
    ( Part
        [transitionDec, entryDec, startDec, stepDec]
        [(transitionT, "the transition type"), (startCon, "the entry type"), (stepCon, "the entry type")]
        [(transitionT, "the transition type"), (entryT, "the entry type")]
    )

-- | Storing a state outside Haskell, as the set of active state ids. Generated
-- rather than exported so that a chart needs no imports beyond the
-- quasiquoter itself.
generateSerialization :: Group -> Q Part
generateSerialization Group {gTo, gFrom} = do
  let toIdsName = mkName "serializeStateMachine"
      fromIdsName = mkName "deserializeStateMachine"
      toCfg = varE gTo
      fromCfg = varE gFrom
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
  pure (Part [toIdsSig, toIdsDec, fromIdsSig, fromIdsDec] [] [])

-- | Data type plus configuration conversions for one compound-like node.
groupDecs :: (StateId -> Q Group) -> Group -> Q [Dec]
groupDecs groupOf Group {gType, gChildren, gTo, gFrom} = do
  shapes <- mapM childShape gChildren
  let dataDec =
        dataD (cxt []) gType [] Nothing
          [normalC con [bangType (bang noSourceUnpackedness noSourceStrictness) (conT f) | f <- fields] | (con, fields, _, _) <- shapes]
          [derivClause Nothing (map conT [''Show, ''Read, ''Eq, ''Ord])]
      toDec = do
        x <- newName "x"
        alts <- forM shapes $ \(con, fields, toE, _) -> do
          vars <- mapM (const (newName "r")) fields
          match (conP con (map varP vars)) (normalB (toE vars)) []
        funD gTo [clause [varP x] (normalB (caseE (varE x) (map pure alts))) []]
      fromDec = do
        cfg <- newName "cfg"
        let body =
              foldr
                (\(Node {nodeId}, (_, _, _, rebuild)) rest -> [| if Set.member $(lift nodeId) $(varE cfg) then $(rebuild cfg) else $rest |])
                [| Nothing |]
                (zip gChildren shapes)
        funD gFrom [clause [varP cfg] (normalB body) []]
  sequence
    [ dataDec
    , sigD gTo [t| $(conT gType) -> Set.Set Text |]
    , toDec
    , sigD gFrom [t| Set.Set Text -> Maybe $(conT gType) |]
    , fromDec
    ]
  where
    -- For one child: (constructor, field types, config-of-fields, rebuild-from-config)
    childShape :: Node -> Q (Name, [Name], [Name] -> Q Exp, Name -> Q Exp)
    -- The groups below are the children's, so their fields are renamed rather
    -- than punned, which would shadow this group's own.
    childShape Node {nodeId = sid, nodeKind} = do
      let con = nameFor sid
      case nodeKind of
        Compound _ -> do
          Group {gType = subType, gTo = subTo, gFrom = subFrom} <- groupOf sid
          pure
            ( con
            , [subType]
            , \vs -> case vs of
                [v] -> [| Set.insert $(lift sid) ($(varE subTo) $(varE v)) |]
                _ -> fail "scxml: internal error, compound state expects exactly one field"
            , \cfg -> [| fmap $(conE con) ($(varE subFrom) $(varE cfg)) |]
            )
        Parallel regionNodes -> do
          regions <- forM (NE.toList regionNodes) $ \Node {nodeId = rid, nodeKind = regionKind} -> case regionKind of
            Parallel _ ->
              fail $
                "scxml: <parallel> " ++ T.unpack rid ++ " is directly inside another <parallel>, which is not supported."
                  ++ " Flatten it: its regions can become regions of the outer <parallel>, since all of them are active at once either way."
            _ -> (,) rid <$> groupOf rid
          -- An atomic region's own conversion already holds its id, which
          -- inserting again leaves alone.
          let toE vs =
                [| Set.insert $(lift sid) (Set.unions $(listE [[| Set.insert $(lift r) ($(varE subTo) $(varE v)) |] | ((r, Group {gTo = subTo}), v) <- zip regions vs])) |]
              rebuild cfg = foldl (\acc Group {gFrom = subFrom} -> [| $acc <*> $(varE subFrom) $(varE cfg) |]) [| pure $(conE con) |] (map snd regions)
          pure (con, [subType | (_, Group {gType = subType}) <- regions], toE, rebuild)
        _ ->
          pure
            ( con
            , []
            , \_ -> [| Set.singleton $(lift sid) |]
            , \_ -> [| Just $(conE con) |]
            )

-- | What generating an instance needs to know about the rest of the chart.
data Ctx = Ctx
  { cEntryActT  :: Name
  , cExitActT   :: Name
  , cEventCon   :: Text -> Q Name
  , cEntryCon   :: Text -> Name
  , cExitCon    :: Text -> Name
  , cEventCount :: Int
  }

-- | The instance head and associated types every layer shares.
fsmInstance :: Ctx -> Name -> [Q Dec] -> Q Dec
fsmInstance Ctx {cEntryActT, cExitActT} ty methods =
  instanceD (cxt []) [t| Fsm.Fsm $(conT ty) $(conT eventT) |] $
    tySynInstD (tySynEqn Nothing [t| Fsm.EntryAction $(conT ty) |] (conT cEntryActT))
      : tySynInstD (tySynEqn Nothing [t| Fsm.ExitAction $(conT ty) |] (conT cExitActT))
      : methods

method :: Name -> [Q Pat] -> Q Exp -> Q Dec
method name args body = funD name [clause args (normalB body) []]

-- | A case with the given alternatives and, when they do not cover all @total@
-- constructors, a wildcard for the rest. Generated code lands in the user's
-- module, so it must neither leave a match incomplete nor add a redundant one.
caseCovering :: Q Exp -> Int -> [(Q Pat, Q Exp)] -> Q Exp -> Q Exp
caseCovering scrut total alts dflt
  | null alts = dflt
  | otherwise =
      caseE scrut $
        [match p (normalB e) [] | (p, e) <- alts]
          ++ [match wildP (normalB dflt) [] | length alts < total]

-- | The 'Fsm.Fsm' instance of one layer: the root, a compound state's type, or
-- an atomic region's.
groupInstance :: Ctx -> Group -> Q Dec
groupInstance ctx@Ctx {cEventCon, cEntryCon, cExitCon, cEventCount} Group {gType, gChildren} = do
  st <- newName "_st"
  ev <- newName "_ev"
  f <- newName "_f"
  initial <- case gChildren of
    c : _ -> pure c
    []    -> fail "scxml: internal error, a group without children"
  let con = nameFor . nodeId
      plain n = recP (con n) []
      perChild alts dflt = caseCovering (varE st) (length gChildren) alts dflt
      sibling tgt = case find ((== tgt) . nodeId) gChildren of
        Just n -> pure n
        Nothing -> fail ("scxml: internal error, unknown transition target " ++ T.unpack tgt)
      -- How many layers below a child holds: its compound state's, or one per
      -- region.
      fieldCount n = case nodeKind n of
        Compound _  -> 1
        Parallel rs -> length rs
        _           -> 0
      regionsOf n = case nodeKind n of
        Parallel rs -> NE.toList rs
        _           -> []
      -- A region is entered and left exactly when its parallel state is, so
      -- its callbacks run with the parallel state's: after them on entry,
      -- before them on exit.
      entryActions n = nodeOnEntry n ++ concatMap nodeOnEntry (regionsOf n)
      exitActions n = concatMap nodeOnExit (regionsOf n) ++ nodeOnExit n
      enter n = foldl appE (conE (con n)) (replicate (fieldCount n) [| Fsm.initialState |])
      transitionsOf n = do
        alts <- forM (Map.toList (nodeTransitions n)) $ \(name, tgt) -> do
          c <- cEventCon name
          target <- sibling tgt
          pure (recP c [], [| Just $(enter target) |])
        pure (caseCovering (varE ev) cEventCount alts [| Nothing |])
      actions conOf names = listE [conE (conOf a) | a <- names]
  nexts <- sequence [(,) (plain n) <$> transitionsOf n | n <- gChildren, not (Map.null (nodeTransitions n))]
  substates <- forM [n | n <- gChildren, fieldCount n > 0] $ \n -> do
    parts <- forM [1 .. fieldCount n] $ \_ -> (,,) <$> newName "x" <*> newName "x'" <*> newName "r"
    pure
      ( conP (con n) [varP x | (x, _, _) <- parts]
      , letE [valD (tupP [varP x', varP r]) (normalB [| $(varE f) $(varE x) |]) [] | (x, x', r) <- parts] $
          tupE
            [ foldl appE (conE (con n)) [varE x' | (_, x', _) <- parts]
            , foldr1 (\a b -> [| $a <> $b |]) [varE r | (_, _, r) <- parts]
            ]
      )
  fsmInstance ctx gType
    [ method 'Fsm.initialState [] (enter initial)
    , method 'Fsm.nextState [varP ev, varP st] (perChild nexts [| Nothing |])
    , method 'Fsm.onEntry [varP st] (perChild [(plain n, actions cEntryCon (entryActions n)) | n <- gChildren, not (null (entryActions n))] [| [] |])
    , method 'Fsm.onExit [varP st] (perChild [(plain n, actions cExitCon (exitActions n)) | n <- gChildren, not (null (exitActions n))] [| [] |])
    , method 'Fsm.isFinal [varP st] (perChild [(plain n, [| True |]) | n@Node {nodeKind = Final} <- gChildren] [| False |])
    , method 'Fsm.doneEvent [varP st] (perChild [(plain n, [| Just $(conE (doneCon n)) |]) | n <- gChildren, completes n] [| Nothing |])
    , method 'Fsm.applyToSubstates [varP st, varP f] (perChild substates [| ($(varE st), mempty) |])
    ]

-- | The constructor of a state's @done.state@ event, as the event type names it.
doneCon :: Node -> Name
doneCon Node {nodeId} = mkName ("Done" ++ T.unpack nodeId)

-- | A callback name made into part of a constructor name: @Inventory.reserve@
-- becomes @Inventory_reserve@.
mangle :: Text -> String
mangle = map (\c -> if c == '.' then '_' else c) . T.unpack

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
    commaList xs     = foldr1 (\a b -> a ++ ", " ++ b) xs

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
