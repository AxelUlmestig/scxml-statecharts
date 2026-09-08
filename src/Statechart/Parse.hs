-- | Parse SCXML into the untyped t'Chart' model.
--
-- Supported: @<state>@, @<parallel>@, @<final>@, @<transition>@ (event,
-- target), @initial@ attributes and @<initial>@ elements, and
-- @<script>@ inside @<onentry>@ and @<onexit>@ whose content is the name of a
-- Haskell function to run.
--
-- Deliberately unsupported: @cond@ guards, eventless transitions and
-- transitions without a target (make the decision in an @<onentry>@ callback
-- that raises an event instead), @<script>@ on a transition (put it in the
-- @<onentry>@ of the target, which receives the triggering event), and
-- @type="internal"@ (it can only differ from an external transition for a
-- target inside the source, which the level rule below forbids).
--
-- A transition must target a sibling of its source: events never cross
-- levels. To leave an enclosing state, put the transition on that state.
-- @initial@ follows the same rule and is required on every compound state:
-- it must name a direct child.
--
-- Not yet supported: @<history>@, wildcard event descriptors, other
-- executable content (@<assign>@, @<raise>@, @<send>@, ...).
--
-- State ids and event names are used verbatim as Haskell constructor and type
-- names, so they must be valid ones (@PaymentAuthorized@, not
-- @payment.authorized@). The one exception is SCXML's automatic
-- @done.state.X@ event, which becomes the constructor @DoneX@. The @name@
-- attribute on @<scxml>@ is optional metadata, kept in 'chartName' for
-- logging and persistence; it does not affect the generated names.
module Statechart.Parse (parseScxml) where

import Control.Monad (ap, forM_, unless, when)
import Data.Char (isAlphaNum, isUpper)
import Data.List (group, intercalate, sort, stripPrefix)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, listToMaybe)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.XML (Element, Name (nameLocalName))
import qualified Text.XML as X

import Statechart.Model
import Statechart.Interpret (canComplete)

-- A tiny state+error monad, used to hand out document-order indices.
-- A state+error monad handing out document-order indices and collecting
-- event names in the order they are first seen.
data S = S {sNext :: Int, sEvents :: [Text]}

newtype P a = P {runP :: S -> Either String (a, S)}

instance Functor P where
  fmap f (P g) = P $ \st -> fmap (\(a, st') -> (f a, st')) (g st)

instance Applicative P where
  pure a = P $ \st -> Right (a, st)
  (<*>) = ap

instance Monad P where
  P g >>= k = P $ \st -> g st >>= \(a, st') -> runP (k a) st'

throwP :: String -> P a
throwP msg = P $ \_ -> Left msg

liftE :: Either String a -> P a
liftE = either throwP pure

next :: P Int
next = P $ \st -> Right (sNext st, st {sNext = sNext st + 1})

-- | Record an event name at its first occurrence in the document.
seeEvent :: Text -> P ()
seeEvent e = P $ \st ->
  Right ((), if e `elem` sEvents st then st else st {sEvents = sEvents st ++ [e]})

-- | Parse and validate an SCXML document. The 'Left' case is a message meant
-- to be shown to whoever wrote the XML; the quasiquoter reports it as a
-- compile error.
parseScxml :: String -> Either String Chart
parseScxml src = do
  root <- parseXml src
  unless (localName root == "scxml") $
    Left ("root element must be <scxml>, found <" ++ localName root ++ ">")
  (nodes, finalState) <- runP (concat <$> mapM (buildNode Nothing) (stateChildren root)) (S 0 [])
  let rootChildren = [nodeId n | n <- nodes, isNothing (nodeParent n)]
  when (null rootChildren) $ Left "<scxml> contains no states"
  initial <- initialOf "<scxml>" root rootChildren
  let dups = [d | (d : _ : _) <- group (sort (map nodeId nodes))]
      whose n = maybe "the chart root" (\p -> "a child of " ++ T.unpack p) (nodeParent n)
      describe d =
        show (T.unpack d) ++ " is used by "
          ++ intercalate " and " [whose n | n <- nodes, nodeId n == d]
  unless (null dups) $
    Left $
      "duplicate state ids: " ++ intercalate "; " (map describe dups)
        ++ ". State ids must be unique across the whole chart, whatever their parents:"
        ++ " SCXML ids are XML IDs, and each one becomes a Haskell constructor"
  let ch =
        Chart
          { chartName = T.pack <$> attr "name" root
          , chartRootChildren = rootChildren
          , chartInitial = initial
          , chartEvents = sEvents finalState
          , chartNodes = Map.fromList [(nodeId n, n) | n <- nodes]
          }
  validate ch
  pure ch

-- | Parse strictly: anything that is not well-formed XML is rejected, so a
-- typo cannot quietly become a different chart.
parseXml :: String -> Either String Element
parseXml src = case X.parseText X.def (TL.pack src) of
  Right doc -> Right (X.documentRoot doc)
  Left err ->
    Left ("document is not well-formed XML: " ++ tidy (unwords (words (show err))))
  where
    -- xml-conduit prints namespace-qualified Name records and Event
    -- constructors, which are noise in a compile error.
    tidy = replace "EventEndElement (" "" . replace ">)" ">"
         . replace "EventEndDocument" "the end of the document" . tidyNames
    tidyNames [] = []
    tidyNames str@(c : cs) = case stripPrefix "Name {nameLocalName = \"" str of
      Just rest ->
        let (nm, rest') = break (== '"') rest
         in case dropWhile (/= '}') rest' of
              '}' : rest'' -> "<" ++ nm ++ ">" ++ tidyNames rest''
              _ -> str
      Nothing -> c : tidyNames cs
    replace from to = go
      where
        go [] = []
        go str@(c : cs) = case stripPrefix from str of
          Just rest -> to ++ go rest
          Nothing -> c : go cs

localName :: Element -> String
localName = T.unpack . nameLocalName . X.elementName

-- Match attributes by local name only, ignoring namespaces.
attr :: String -> Element -> Maybe String
attr k el =
  T.unpack
    <$> listToMaybe [v | (n, v) <- Map.toList (X.elementAttributes el), nameLocalName n == T.pack k]

elChildren :: Element -> [Element]
elChildren el = [e | X.NodeElement e <- X.elementNodes el]

strContent :: Element -> String
strContent el = T.unpack (T.concat [t | X.NodeContent t <- X.elementNodes el])

childrenNamed :: [String] -> Element -> [Element]
childrenNamed names el = [c | c <- elChildren el, localName c `elem` names]

stateChildren :: Element -> [Element]
stateChildren = childrenNamed ["state", "parallel", "final", "history"]

hasInitial :: Element -> Bool
hasInitial el = attr "initial" el /= Nothing || not (null (childrenNamed ["initial"] el))

allowedChildren :: [String]
allowedChildren =
  [ "state", "parallel", "final", "history", "transition", "initial"
  , "onentry", "onexit", "datamodel", "invoke", "donedata"
  ]

-- | A transition must target a sibling of its source. Crossing levels is
-- rejected in both directions: reaching into another state's interior would
-- bypass its @initial@ declaration, and escaping outward hides which
-- enclosing state is being left. To leave an enclosing state, declare the
-- transition on that state.
checkLevel :: Chart -> StateId -> StateId -> Either String ()
checkLevel ch src tgt
  | sourceLevel == targetLevel = Right ()
  | otherwise =
      Left $
        "transition from " ++ T.unpack src ++ " to " ++ T.unpack tgt
          ++ " crosses levels: " ++ describe src sourceLevel ++ ", but " ++ describe tgt targetLevel
          ++ ". A transition must target a sibling of its source; "
          ++ advice
  where
    sourceLevel = nodeParent (nodeOf ch src)
    targetLevel = nodeParent (nodeOf ch tgt)
    describe s = maybe (T.unpack s ++ " is at the chart root") (\p -> T.unpack s ++ " is inside " ++ T.unpack p)
    advice = case sourceLevel of
      Just p -> "to leave " ++ T.unpack p ++ ", declare the transition on " ++ T.unpack p ++ " instead"
      Nothing -> "target a state at the chart root, or declare the transition inside " ++ maybe "" T.unpack targetLevel

-- | Ids, event names and the chart name become Haskell constructors verbatim.
checkConName :: String -> String -> Either String ()
checkConName what raw = case raw of
  c : cs | isUpper c && all (\x -> isAlphaNum x || x == '_' || x == '\'') cs -> Right ()
  _ -> Left (what ++ " " ++ show raw ++ " must be a Haskell constructor name (start with an upper-case letter, then letters, digits, _ or ')")

-- | The prefix of SCXML's automatic completion events.
donePrefix :: Text
donePrefix = T.pack "done.state."

-- | The function names in @<script>@ children of an @<onentry>@, @<onexit>@ or
-- @<transition>@ element, in document order.
scriptsOf :: String -> Element -> Either String [Text]
scriptsOf label el = concat <$> mapM one (elChildren el)
  where
    one c
      | localName c == "script" = case words (strContent c) of
          [name] -> Right [T.pack name]
          _ -> Left (label ++ ": <script> must contain exactly one Haskell function name, got " ++ show (strContent c))
      | localName c `elem` ["raise", "if", "foreach", "log", "assign", "send", "cancel"] =
          Left (label ++ ": executable content <" ++ localName c ++ "> is not supported; use <script>functionName</script>")
      | otherwise = Right []

-- | The child state that entering a compound state (or the @<scxml>@ root)
-- leads to. Required, exactly one, and a direct child: entering must not
-- reach into another state's interior, the same rule transitions follow.
initialOf :: String -> Element -> [StateId] -> Either String StateId
initialOf label el children =
  case (attr "initial" el, childrenNamed ["initial"] el) of
    (Just i, []) -> one "initial attribute" i
    (Nothing, [ie]) -> case childrenNamed ["transition"] ie of
      [t] | Just tg <- attr "target" t -> one "<initial> transition target" tg
      _ -> Left (label ++ ": <initial> must contain exactly one <transition target=...>")
    (Nothing, []) ->
      Left $
        label ++ ": needs an initial attribute naming the child state to enter, for example initial="
          ++ show (maybe "..." T.unpack (listToMaybe children))
    (Just _, _ : _) -> Left (label ++ ": has both an initial attribute and an <initial> element")
    (Nothing, _ : _ : _) -> Left (label ++ ": has more than one <initial> element")
  where
    one what s = case map T.pack (words s) of
      [c]
        | c `elem` children -> Right c
        | otherwise ->
            Left $
              label ++ ": " ++ what ++ " " ++ show (T.unpack c) ++ " must name one of its direct child states ("
                ++ intercalate ", " (map T.unpack children)
                ++ "); entering a state may not reach into another state's interior"
      [] -> Left (label ++ ": empty " ++ what)
      cs ->
        Left $
          label ++ ": " ++ what ++ " names several states (" ++ unwords (map T.unpack cs)
            ++ "); exactly one child state is required"

buildNode :: Maybe StateId -> Element -> P [Node]
buildNode parent el = do
  let tag = localName el
  when (tag == "history") $ throwP "<history> states are not supported yet"
  unless (tag `elem` ["state", "parallel", "final"]) $
    throwP ("unexpected element <" ++ tag ++ "> where a state was expected")
  sid <- case attr "id" el of
    Just i -> liftE (checkConName ("<" ++ tag ++ "> id") i) >> pure (T.pack i)
    Nothing -> throwP ("<" ++ tag ++ "> without an id attribute" ++ maybe "" (\p -> " inside " ++ T.unpack p) parent)
  let label = "<" ++ tag ++ " id=\"" ++ T.unpack sid ++ "\">"
  order <- next
  forM_ (elChildren el) $ \c ->
    unless (localName c `elem` allowedChildren) $
      throwP (label ++ ": unexpected child element <" ++ localName c ++ ">")
  -- Walk children in textual order so document-order indices are exactly that.
  (pairs, descendants) <- buildChildren label sid el
  trans <- liftE (transitionMap label pairs)
  onEntry <- liftE (concat <$> mapM (scriptsOf (label ++ " <onentry>")) (childrenNamed ["onentry"] el))
  onExit <- liftE (concat <$> mapM (scriptsOf (label ++ " <onexit>")) (childrenNamed ["onexit"] el))
  let children = [nodeId n | n <- descendants, nodeParent n == Just sid]
  kind <- case (tag, NE.nonEmpty children) of
    ("parallel", Nothing) -> throwP (label ++ ": <parallel> must contain at least one region")
    ("parallel", Just regions) -> do
      when (hasInitial el) $ throwP (label ++ ": <parallel> cannot specify an initial state; every region is entered")
      pure (Parallel regions)
    ("final", _) -> do
      unless (null children) $ throwP (label ++ ": final states cannot contain states")
      unless (null trans) $ throwP (label ++ ": final states cannot have transitions")
      when (hasInitial el) $ throwP (label ++ ": final states cannot specify an initial state")
      pure Final
    (_, Nothing) -> do
      when (hasInitial el) $ throwP (label ++ ": atomic states cannot specify an initial state")
      pure Atomic
    (_, Just kids) -> Compound kids <$> liftE (initialOf label el children)
  let node =
        Node
          { nodeId = sid
          , nodeKind = kind
          , nodeParent = parent
          , nodeTransitions = trans
          , nodeOnEntry = onEntry
          , nodeOnExit = onExit
          , nodeOrder = order
          }
  pure (node : descendants)

-- | Transitions and descendant nodes of an element, numbered in textual order.
buildChildren :: String -> StateId -> Element -> P ([(Text, StateId)], [Node])
buildChildren label sid el = go (elChildren el)
  where
    go [] = pure ([], [])
    go (c : rest)
      | localName c == "transition" = do
          t <- buildTransition label c
          (ts, ns) <- go rest
          pure (t ++ ts, ns)
      | localName c `elem` ["state", "parallel", "final", "history"] = do
          ns0 <- buildNode (Just sid) c
          (ts, ns) <- go rest
          pure (ts, ns0 ++ ns)
      | otherwise = go rest

-- | The (event, target) pairs one @<transition>@ element contributes. SCXML
-- allows several event names on one element, which is only shorthand for
-- several transitions with the same target.
buildTransition :: String -> Element -> P [(Text, StateId)]
buildTransition label el = do
  let events = maybe [] words (attr "event" el)
      targets = map T.pack (maybe [] words (attr "target" el))
  when (attr "cond" el /= Nothing) $
    throwP (label ++ ": cond is not supported; make the decision in an <onentry> callback that raises an event instead")
  unless (null (childrenNamed ["script"] el)) $
    throwP (label ++ ": <script> on a transition is not supported; put it in the <onentry> of the target, which receives the triggering event")
  when (null events) $
    throwP (label ++ ": transition without an event; eventless transitions are not supported, raise an event from a callback instead")
  target <- case targets of
    [t] -> pure t
    [] -> throwP (label ++ ": transition without a target; to act on an event without leaving the state, target the state itself")
    ts ->
      throwP $
        label ++ ": transition names several targets (" ++ unwords (map T.unpack ts)
          ++ "); a transition enters exactly one state, and entering siblings at once is only meaningful inside a <parallel>, whose regions cannot have transitions"
  case attr "type" el of
    Nothing -> pure ()
    Just "external" -> pure ()
    Just "internal" ->
      throwP (label ++ ": type=\"internal\" is not supported; it can only differ from an external transition for a target inside the source, which is not allowed")
    Just other -> throwP (label ++ ": unknown transition type " ++ show other)
  forM_ events $ \e ->
    when ('*' `elem` e) $
      throwP (label ++ ": wildcard event descriptor " ++ show e ++ " is not supported")
  forM_ events $ \e -> case stripPrefix (T.unpack donePrefix) e of
    Just inner -> liftE (checkConName (label ++ " done.state event state") inner)
    Nothing -> liftE (checkConName (label ++ " event") e)
  mapM_ (seeEvent . T.pack) events
  pure [(T.pack e, target) | e <- events]

-- | One transition per event, so selection never has to break a tie.
transitionMap :: String -> [(Text, StateId)] -> Either String (Map.Map Text StateId)
transitionMap label pairs = case dups of
  [] -> Right (Map.fromList pairs)
  (e : _) ->
    Left $
      label ++ ": two transitions for the event " ++ show (T.unpack e) ++ " (to "
        ++ intercalate " and " [T.unpack t | (e', t) <- pairs, e' == e]
        ++ "); with no cond there is nothing to choose between them"
  where
    dups = [e | (e : _ : _) <- group (sort (map fst pairs))]

validate :: Chart -> Either String ()
validate ch = do
  let known s = Map.member s (chartNodes ch)
      check what s = unless (known s) $ Left (what ++ " refers to unknown state " ++ show (T.unpack s))
  forM_ (allNodes ch) $ \n -> do
    case nodeParent n >>= \p -> if isParallel (kindOf ch p) then Just p else Nothing of
      Just p | not (Map.null (nodeTransitions n)) ->
        Left $
          T.unpack (nodeId n) ++ " is a region of the <parallel> " ++ T.unpack p
            ++ " and cannot have transitions: its sibling regions are active at the same time, so leaving it would leave them behind."
            ++ " Declare the transition on " ++ T.unpack p ++ " or on a state inside " ++ T.unpack (nodeId n) ++ "."
      _ -> Right ()
    forM_ (Map.toList (nodeTransitions n)) $ \(e, tgt) -> do
      check ("transition from " ++ T.unpack (nodeId n)) tgt
      checkLevel ch (nodeId n) tgt
      forM_ (T.stripPrefix donePrefix e) $ \target -> do
        check ("done.state event in transition from " ++ T.unpack (nodeId n)) target
        unless (canComplete ch target) $
          Left (T.unpack e ++ " can never fire: " ++ T.unpack target ++ " is not a <parallel> or a <state> with a <final> child")
