{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
-- | A parallel state's regions have callbacks of their own, and a region that
-- is an atomic state is active for as long as the parallel state is but can
-- never complete, so neither can the parallel state holding it.
module Regions where

import Control.Monad.Trans.State.Strict (StateT, modify', runStateT)
import Scxml.Statechart (scxml)

[scxml|
<scxml initial="Idle">
  <state id="Idle">
    <transition event="Go" target="Work"/>
  </state>
  <parallel id="Work">
    <onentry><script>enterWork</script></onentry>
    <onexit><script>exitWork</script></onexit>
    <state id="Upload" initial="Uploading">
      <onentry><script>enterUpload</script></onentry>
      <onexit><script>exitUpload</script></onexit>
      <state id="Uploading">
        <onentry><script>enterUploading</script></onentry>
        <onexit><script>exitUploading</script></onexit>
        <transition event="Uploaded" target="Stored"/>
      </state>
      <final id="Stored"/>
    </state>
    <!-- An atomic region: no field in Work's constructor, but its callbacks
         still run, and it keeps Work from ever completing. -->
    <state id="Log">
      <onentry><script>enterLog</script></onentry>
      <onexit><script>exitLog</script></onexit>
    </state>
    <transition event="Stop" target="Idle"/>
    <transition event="done.state.Work" target="Finished"/>
  </parallel>
  <final id="Finished"/>
</scxml>
|]

-- Generated:
--
--   data FsmState = Idle | Work Upload | Finished
--   data Upload   = Uploading | Stored

type M = StateT [String] IO

initiateStateMachine :: M FsmState
notifyStateMachine :: FsmState -> FsmEvent -> M FsmState

enterWork, enterUpload, enterUploading, enterLog :: FsmEntry -> M (Maybe FsmEvent)
enterWork _ = note "enter Work"
enterUpload _ = note "enter Upload"
enterUploading _ = note "enter Uploading"
enterLog _ = note "enter Log"

exitWork, exitUpload, exitUploading, exitLog :: FsmTransition -> M ()
exitWork _ = modify' (++ ["exit Work"])
exitUpload _ = modify' (++ ["exit Upload"])
exitUploading _ = modify' (++ ["exit Uploading"])
exitLog _ = modify' (++ ["exit Log"])

note :: String -> M (Maybe FsmEvent)
note s = modify' (++ [s]) >> pure Nothing

run :: [FsmEvent] -> IO (FsmState, [String])
run evs = runStateT (initiateStateMachine >>= go evs) []
  where
    go [] s = pure s
    go (e : rest) s = notifyStateMachine s e >>= go rest

-- | Checks to run, as (label, expected, actual) triples.
spec :: IO [(String, String, String)]
spec = do
  (entered, enteredLog) <- run [Go]
  (stored, _) <- run [Go, Uploaded]
  (stopped, stoppedLog) <- run [Go, Uploaded, Stop]
  pure
    [ ("entering a parallel state enters every region", show (Work Uploading), show entered)
    , ("a region's entry callbacks run before its children's, regions in document order"
      , show ["enter Work", "enter Upload", "enter Uploading", "enter Log"]
      , show enteredLog
      )
    , ("an atomic region keeps the parallel state from completing", show (Work Stored), show stored)
    , ("exit callbacks run innermost first, regions in document order"
      , show (enteredLog ++ ["exit Uploading", "exit Upload", "exit Log", "exit Work"])
      , show stoppedLog
      )
    , ("leaving the parallel state", show Idle, show stopped)
    ]
