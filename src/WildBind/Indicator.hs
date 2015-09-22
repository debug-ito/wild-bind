-- |
-- Module: WildBind.Indicator
-- Description: Graphical indicator for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module WildBind.Indicator (
  withNumPadIndicator,
  Indicator,
  updateDescription,
  getPresence,
  setPresence,
  togglePresence,
  NumPadPosition(..)
) where

import WildBind (ActionDescription)
import WildBind.Input.NumPad (NumPadUnlockedInput(..), NumPadLockedInput(..))

-- | Indicator interface. @s@ is the front-end state, @i@ is the input
-- type.
data Indicator s i = Indicator {
  updateDescription :: [(i, ActionDescription)] -> IO (),
  -- ^ Update and show the description for the current binding.
  getPresence :: IO Bool,
  -- ^ Get the current presence of the indicator. Returns 'True' if
  -- it's present.
  setPresence :: Bool -> IO ()
  -- ^ Set the presence of the indicator.
  }

-- | Toggle the presence of the indicator.
togglePresence :: Indicator s i -> IO ()
togglePresence ind = (setPresence ind . not) =<< getPresence ind


-- | Something that can be mapped to number pad's key positions.
class NumPadPosition a where
  toNumPad :: a -> NumPadLockedInput

instance NumPadPosition NumPadLockedInput where
  toNumPad = id

instance NumPadPosition NumPadUnlockedInput where
  toNumPad input = case input of
    NumInsert -> NumL0
    NumEnd -> NumL1
    NumDown -> NumL2
    NumPageDown -> NumL3
    NumLeft -> NumL4
    NumCenter -> NumL5
    NumRight -> NumL6
    NumHome -> NumL7
    NumUp -> NumL8
    NumPageUp -> NumL9
    NumDivide -> NumLDivide
    NumMulti -> NumLMulti
    NumMinus -> NumLMinus
    NumPlus -> NumLPlus
    NumEnter -> NumLEnter
    NumDelete -> NumLPeriod


-- | Initialize the indicator and run the given action.
withNumPadIndicator :: NumPadPosition i => (Indicator s i -> IO a) -> IO a
withNumPadIndicator action = undefined

