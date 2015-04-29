-- |
-- Module: WildBind
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
{-# LANGUAGE MultiParamTypeClasses #-}
module WildBind (
  
) where

import Data.Maybe (isJust)
import Data.Text (Text)

class FrontState s
class FrontInput i

type ActionDescription = Text

class (FrontInput i) => FrontInputDevice f i where
  defaultActionDescription :: f -> i -> ActionDescription
  setGrab :: f -> (i -> Bool) -> IO ()

data FrontEvent s i = FEInput s i
                    | FEChange s

class (FrontState s, FrontInput i) => FrontEventSource f s i where
  nextEvent :: f -> IO (FrontEvent s i)  -- should it be a streaming I/F, like conduit?

data Action a = Action {
  actDescription :: ActionDescription,
  actDo :: IO a
}

class (FrontInput i) => FrontDescriber f i where
  describeActions :: f -> (i -> ActionDescription) -> IO ()

newtype Engine s i = Engine {
  bindingFor :: s -> i -> Maybe (Action (Engine s i))
}

wildBind :: (FrontInputDevice f i, FrontEventSource f s i) => f -> Engine s i -> IO ()
wildBind front engine = do
  event <- nextEvent front
  case event of
    FEChange state -> setGrab' state
    FEInput state input -> do
      setGrab' state
      case bindingFor engine state input of
        Nothing -> loop
        Just action -> wildBind front =<< actDo action
  where
    loop = wildBind front engine
    setGrab' state = setGrab front (isJust . bindingFor engine state)
