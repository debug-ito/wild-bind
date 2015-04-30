-- |
-- Module: WildBind.NumPad
-- Description: Types about number pads
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--

module WildBind.NumPad (NumPadUnlockedInput(..)) where

import WildBind (FrontInput)

-- | Number pad key input with NumLock disabled.
data NumPadUnlockedInput
  = NumHome
  | NumUp
  | NumPageUp
  | NumLeft
  | NumCenter
  | NumRight
  | NumEnd
  | NumDown
  | NumPageDown
  | NumDivide
  | NumMulti
  | NumMinus
  | NumPlus
  | NumEnter
  | NumInsert
  | NumDelete
  deriving (Eq,Ord,Show,Bounded,Enum)

instance FrontInput NumPadUnlockedInput
