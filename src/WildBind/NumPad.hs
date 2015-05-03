-- |
-- Module: WildBind.NumPad
-- Description: Types about number pads
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
{-# LANGUAGE OverloadedStrings #-}
module WildBind.NumPad (
  NumPadUnlockedInput(..),
  descriptionForUnlocked
) where

import WildBind (FrontInput, ActionDescription)

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

-- | default description for unlocked numpad keys
descriptionForUnlocked :: NumPadUnlockedInput -> ActionDescription
descriptionForUnlocked input = case input of
  NumHome -> "Home"
  NumUp -> "↑"
  NumPageUp -> "PageUp"
  NumLeft -> "←"
  NumCenter -> ""
  NumRight -> "→"
  NumEnd -> "End"
  NumDown -> "↓"
  NumPageDown -> "PageDown"
  NumDivide -> "/"
  NumMulti -> "*"
  NumMinus -> "-"
  NumPlus -> "+"
  NumEnter -> "Enter"
  NumInsert -> "Insert"
  NumDelete -> "Delete"
