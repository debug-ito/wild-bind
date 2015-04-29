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
  Binding(..),
  -- * Entry point
  wildBind
) where

import Data.Maybe (isJust)
import Data.Text (Text)

-- | The state of the front-end. It can change with no regard with WildBind.
class FrontState s

-- | The type of input symbols, like key buttons.
class FrontInput i

-- | Human-readable description of an action.
type ActionDescription = Text

-- | Something that knows about and controls the input device.
class (FrontInput i) => FrontInputDevice f i where
  defaultActionDescription :: f -> i -> ActionDescription
  setGrab :: f -> (i -> Bool) -> IO ()

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
  describeActions :: f -> (i -> ActionDescription) -> IO ()

-- | WildBind back-end binding between inputs and actions.
newtype Binding s i = Binding {
  bindingFor :: s -> i -> Maybe (Action (Binding s i))
                -- ^ If 'Nothing', the input is not bound. If 'Just',
                -- the result of the 'Action' is the new state of the
                -- 'Binding'.
}

-- | Combines the front-end and the 'Binding' and returns the executable.
wildBind :: (FrontInputDevice f i, FrontEventSource f s i) => f -> Binding s i -> IO ()
wildBind front binding = do
  event <- nextEvent front
  case event of
    FEChange state -> setGrab' state
    FEInput state input -> do
      setGrab' state
      case bindingFor binding state input of
        Nothing -> loop
        Just action -> wildBind front =<< actDo action
  where
    loop = wildBind front binding
    setGrab' state = setGrab front (isJust . bindingFor binding state)
