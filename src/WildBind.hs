-- |
-- Module: WildBind
-- Description: WildBind main module
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module WildBind (
  -- * Common types
  ActionDescription,
  -- * Front-end
  FrontState,
  FrontInput,
  FrontInputDevice(..),
  FrontEvent(..),
  FrontEventSource(..),
  FrontDescriber(..),
  -- * Back-end
  Action(..),
  Binding,
  boundAction,
  boundInputs,
  -- * Entry point
  wildBind
) where

import Data.List ((\\))
import qualified Control.Monad.Trans.State as State
import Control.Monad.IO.Class (liftIO)

import WildBind.Internal.Common (ActionDescription)
import WildBind.Internal.FrontEnd (
  FrontState,
  FrontInput,
  FrontInputDevice(..),
  FrontEvent(..),
  FrontEventSource(..),
  FrontDescriber(..)
  )
import WildBind.Internal.BackEnd (
  Action(..),
  Binding,
  boundAction,
  boundInputs,
  )


type GrabSet i = [i]

updateGrab :: (FrontInputDevice f i) => f -> GrabSet i -> GrabSet i -> IO ()
updateGrab front before after = do
  mapM_ (unsetGrab front) (before \\ after)
  mapM_ (setGrab front) (after \\ before)

-- | Combines the front-end and the 'Binding' and returns the executable.
wildBind :: (FrontInputDevice fi i, FrontEventSource fe s i) => fi -> fe -> Binding s i -> IO ()
wildBind front_input front_event binding =
  State.evalStateT (wildBindWithState front_input front_event) (binding, Nothing)

---

type WBState s i = State.StateT (Binding s i, Maybe s) IO

updateWBState :: (FrontInputDevice f i, FrontState s) => f -> Binding s i -> s -> WBState s i ()
updateWBState front after_binding after_state = do
  (before_binding, before_mstate) <- State.get
  let before_grabset = maybe [] (boundInputs before_binding) before_mstate
  State.put $ (after_binding, Just after_state)
  liftIO $ updateGrab front before_grabset (boundInputs after_binding after_state)

updateFrontState :: (FrontInputDevice f i, FrontState s) => f -> s -> WBState s i ()
updateFrontState front after_state = do
  (cur_binding, _) <- State.get
  updateWBState front cur_binding after_state

updateBinding :: (FrontInputDevice f i, FrontState s) => f -> Binding s i -> WBState s i ()
updateBinding front after_binding = do
  (_, mstate) <- State.get
  case mstate of
    Nothing -> return ()
    Just state -> updateWBState front after_binding state

wildBindWithState :: (FrontInputDevice fi i, FrontEventSource fe s i) => fi -> fe -> WBState s i ()
wildBindWithState front_input front_event = do
  event <- liftIO $ nextEvent front_event
  case event of
    FEChange state ->
      updateFrontState front_input state
    FEInput state input -> do
      updateFrontState front_input state
      (cur_binding, _) <- State.get
      case boundAction cur_binding state input of
        Nothing -> return ()
        Just action -> do
          next_binding <- liftIO $ actDo action
          updateBinding front_input next_binding
  wildBindWithState front_input front_event


