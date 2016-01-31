-- |
-- Module: WildBind.Exec
-- Description: Functions to create executable actions.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module WildBind.Exec
       ( -- * Functions to build executable action
         wildBind,
         wildBind',
         -- * Option for executable
         Option,
         def,
         -- ** Accessor functions for 'Option'
         optBindingHook
       ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import Data.Default.Class (Default(def))
import Data.List ((\\))

import WildBind.Description (ActionDescription)
import WildBind.FrontEnd
  ( FrontEvent(FEChange,FEInput),
    FrontEnd(frontSetGrab, frontUnsetGrab, frontNextEvent)
  )
import WildBind.Binding
  ( Action(actDo, actDescription),
    Binding,
    boundAction,
    boundInputs,
    boundActions
  )

type GrabSet i = [i]

updateGrab :: (Eq i) => FrontEnd s i -> GrabSet i -> GrabSet i -> IO ()
updateGrab f before after = do
  mapM_ (frontUnsetGrab f) (before \\ after)
  mapM_ (frontSetGrab f) (after \\ before)

-- | Combines the 'FrontEnd' and the 'Binding' and returns the executable.
wildBind :: (Ord i) => Binding s i -> FrontEnd s i -> IO ()
wildBind = wildBind' def

-- | Build the executable with 'Option'.
wildBind' :: (Ord i) => Option s i -> Binding s i -> FrontEnd s i -> IO ()
wildBind' opt binding front =
  flip Reader.runReaderT opt $ flip State.evalStateT (binding, Nothing) $ wildBindInContext front

-- | WildBind configuration options.
--
-- You can get the default value of 'Option' by 'def' funcion, and
-- modify its members via accessor functions listed below.
data Option s i =
  Option { optBindingHook :: [(i, ActionDescription)] -> IO ()
           -- ^ An action executed when current binding may be
           -- changed. Default: do nothing.
         }

instance Default (Option s i) where
  def = Option { optBindingHook = const $ return () }


---

-- | Internal state. fst is the current Binding, snd is the current front-end state.
type WBState s i = (Binding s i, Maybe s)

-- | A monad keeping WildBind context.
type WBContext s i = State.StateT (WBState s i) (Reader.ReaderT (Option s i) IO)

askOption :: WBContext s i (Option s i)
askOption = lift $ Reader.ask

boundDescriptions :: Binding s i -> s -> [(i, ActionDescription)]
boundDescriptions b s = fmap (\(i, act) -> (i, actDescription act)) $ boundActions b s

updateWBState :: (Eq i) => FrontEnd s i -> Binding s i -> s -> WBContext s i ()
updateWBState front after_binding after_state = do
  (before_binding, before_mstate) <- State.get
  let before_grabset = maybe [] (boundInputs before_binding) before_mstate
  State.put $ (after_binding, Just after_state)
  liftIO $ updateGrab front before_grabset (boundInputs after_binding after_state)
  hook <- optBindingHook <$> askOption
  liftIO $ hook $ boundDescriptions after_binding after_state

updateFrontState :: (Eq i) => FrontEnd s i -> s -> WBContext s i ()
updateFrontState front after_state = do
  (cur_binding, _) <- State.get
  updateWBState front cur_binding after_state

updateBinding :: (Eq i) => FrontEnd s i -> Binding s i -> WBContext s i ()
updateBinding front after_binding = do
  (_, mstate) <- State.get
  case mstate of
    Nothing -> return ()
    Just state -> updateWBState front after_binding state

wildBindInContext :: (Ord i) => FrontEnd s i -> WBContext s i ()
wildBindInContext front = do
  event <- liftIO $ frontNextEvent front
  case event of
    FEChange state ->
      updateFrontState front state
    FEInput input -> do
      (cur_binding, mcur_state) <- State.get
      case mcur_state >>= \state -> boundAction cur_binding state input of
        Nothing -> return ()
        Just action -> do
          next_binding <- liftIO $ actDo action
          updateBinding front next_binding
  wildBindInContext front
