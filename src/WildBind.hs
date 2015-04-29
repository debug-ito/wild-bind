-- |
-- Module: WildBind
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module WildBind (
  
) where

import Data.Maybe (isJust)
import Data.Text (Text)

class FrontState s
class FrontInput i

type ActionDescription = Text

data FrontEvent s i = FEInput s i
                    | FEChange s

class (FrontState s, FrontInput i) => Front f s i | f -> s, f -> i where
  defaultActionDescription :: f -> i -> ActionDescription
  setGrab :: f -> (i -> Bool) -> IO ()
  nextEvent :: f -> IO (FrontEvent s i)  -- should it be a streaming I/F?

data Action a = Action {
  actDescription :: ActionDescription,
  actDo :: IO a
}

newtype Engine s i = Engine {
  bindingFor :: s -> i -> Maybe (Action (Engine s i))
}

wildBind :: (Front f s i) => f -> Engine s i -> IO ()
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
