-- |
-- Module: WildBind
-- Description: WildBind main module
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
{-# LANGUAGE MultiParamTypeClasses #-}
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
  -- * Entry point
  wildBind
) where

import Data.Monoid (Monoid(..))

import Data.List ((\\))
import Data.Text (Text)
import qualified Data.Map as M

-- | The state of the front-end. It can change with no regard with WildBind.
class FrontState s

-- | The type of input symbols, like key buttons.
class Ord i => FrontInput i

-- | Human-readable description of an action.
type ActionDescription = Text

-- | Something that knows about and controls the input device.
class (FrontInput i) => FrontInputDevice f i where
  defaultActionDescription :: f -> i -> ActionDescription
  setGrab :: f -> i -> IO () -- ^ Grab (or capture) the specified input symbol. 
  unsetGrab :: f -> i -> IO () -- ^ Release the grab for the input symbol.

-- | Event from the front-end.
data FrontEvent s i = FEInput s i
                    | FEChange s

-- | Something that brings stream of events from the front-end.
class (FrontState s, FrontInput i) => FrontEventSource f s i where
  nextEvent :: f -> IO (FrontEvent s i)  -- should it be a streaming I/F, like conduit?

-- | Action done by WildBind
data Action a = Action {
  actDescription :: ActionDescription, -- ^ Human-readable description of the action.
  actDo :: IO a -- ^ The actual job.
}

-- | Something that describes current bindings for the user.
class (FrontInput i) => FrontDescriber f i where
  describeAction :: f -> i -> ActionDescription -> IO ()

-- | WildBind back-end binding between inputs and actions.
newtype Binding s i = Binding {
  bindingFor :: s -> M.Map i (Action (Binding s i))
                -- ^ The result of the 'Action' is the new state of
                -- the 'Binding'.
}

instance Monoid (Binding s i) where
  mempty = undefined
  mappend = undefined

type GrabSet i = [i]

getGrabSet :: (FrontState s) => Binding s i -> s -> GrabSet i
getGrabSet binding state = M.keys $ bindingFor binding state

updateGrab :: (FrontInputDevice f i) => f -> GrabSet i -> GrabSet i -> IO ()
updateGrab front before after = do
  mapM_ (unsetGrab front) (before \\ after)
  mapM_ (setGrab front) (after \\ before)

-- | Combines the front-end and the 'Binding' and returns the executable.
wildBind :: (FrontInputDevice f i, FrontEventSource f s i) => f -> Binding s i -> IO ()
wildBind front binding = wildBindWithLastState front binding Nothing

-- | TODO: we should rewrite this function with StateT IO... This is safer. The state is Maybe (Binding s i, s)
wildBindWithLastState :: (FrontInputDevice f i, FrontEventSource f s i) => f -> Binding s i -> Maybe s -> IO ()
wildBindWithLastState front binding mlast_state = do
  event <- nextEvent front
  case event of
    FEChange state ->
      updateGrab front (maybe [] (getGrabSet binding) mlast_state) (getGrabSet binding state)
    FEInput state input -> do
      updateGrab front (maybe [] (getGrabSet binding) mlast_state) (getGrabSet binding state)
      case M.lookup input $ bindingFor binding state of
        Nothing -> wildBind front binding
        Just action -> do
          next_binding <- actDo action
          updateGrab front (getGrabSet binding state) (getGrabSet next_binding state)
