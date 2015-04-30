-- |
-- Module: WildBind.X11.Key
-- Description: types and functions related to key symbols and their conversion
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module WildBind.X11.Key (KeySymLike(..)) where

import qualified Graphics.X11.Xlib as Xlib
import qualified Data.Map as M

import qualified WildBind.NumPad as NumPad

-- | Convertible to/from Xlib's 'KeySym'
--
-- prop> fromKeySym . toKeySym == Just
class KeySymLike k where
  fromKeySym :: Xlib.KeySym -> Maybe k
  toKeySym :: k -> Xlib.KeySym

instance KeySymLike NumPad.NumPadUnlockedInput where
  fromKeySym ks = M.lookup ks $ M.fromList $ map (\n -> (toKeySym n, n)) $ enumFromTo minBound maxBound 
  toKeySym n = case n of
    NumPad.NumUp -> Xlib.xK_KP_Up
    NumPad.NumDown -> Xlib.xK_KP_Down
    NumPad.NumLeft -> Xlib.xK_KP_Left
    NumPad.NumRight -> Xlib.xK_KP_Right
    NumPad.NumHome -> Xlib.xK_KP_Home
    NumPad.NumPageUp -> Xlib.xK_KP_Page_Up
    NumPad.NumPageDown -> Xlib.xK_KP_Page_Down
    NumPad.NumEnd -> Xlib.xK_KP_End
    NumPad.NumCenter -> Xlib.xK_KP_Begin
    NumPad.NumInsert -> Xlib.xK_KP_Insert
    NumPad.NumDelete -> Xlib.xK_KP_Delete
    NumPad.NumEnter -> Xlib.xK_KP_Enter
    NumPad.NumDivide -> Xlib.xK_KP_Divide
    NumPad.NumMulti -> Xlib.xK_KP_Multiply
    NumPad.NumMinus -> Xlib.xK_KP_Subtract
    NumPad.NumPlus -> Xlib.xK_KP_Add

