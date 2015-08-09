-- |
-- Module: WildBind.Internal.FrontEnd
-- Description: Data types and type classes about front-ends.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. End-users should not use this module
-- directly. Use "WildBind" instead.
module WildBind.Internal.FrontEnd (
  FrontState,
  FrontInput,
  FrontInputDevice(..),
  FrontEvent(..),
  FrontEventSource(..),
  FrontDescriber(..)
) where

import WildBind.Internal.Common (ActionDescription)

-- | The state of the front-end. It can change with no regard with WildBind.
class FrontState s

-- | The type of input symbols, like key buttons.
class Ord i => FrontInput i

-- | Something that knows about and controls the input device. @f@ is
-- the device, @i@ is the 'FrontInput' data it generates.
class (FrontInput i) => FrontInputDevice f i where
  defaultActionDescription :: f -> i -> ActionDescription
  -- | Grab (or capture) the specified input symbol on the device. 
  setGrab :: f -> i -> IO ()
  -- | Release the grab for the input symbol.
  unsetGrab :: f -> i -> IO ()

-- | Event from the front-end. @s@ is the state of the front-end. @i@ is the input.
data FrontEvent s i = FEInput s i -- ^ An event that a new input is made.
                    | FEChange s  -- ^ An event that the front-end state is changed.
                    deriving (Show)

-- | Something that brings stream of events from the front-end.
class (FrontState s, FrontInput i) => FrontEventSource f s i where
  -- | Retrieve the next event. It blocks if no event is queued.
  nextEvent :: f -> IO (FrontEvent s i)

-- | Something that describes current bindings for the user.
class (FrontInput i) => FrontDescriber f i where
  -- | Describe (show) the given action to the user.
  describeAction :: f -> i -> ActionDescription -> IO ()

