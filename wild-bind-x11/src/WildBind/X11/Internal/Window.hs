-- |
-- Module: WildBind.X11.Internal.Window
-- Description: types and functions related to windows
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
{-# LANGUAGE OverloadedStrings #-}
module WildBind.X11.Internal.Window (
  Window(winName,winDesc),
  ActiveWindow,
  emptyWindow
) where

import Data.Text (Text)

import WildBind (FrontState)

-- | Information about window
data Window = Window {
  winName :: Text, -- ^ name of the window
  winDesc :: Text  -- ^ description of the window
} deriving (Eq,Ord,Show)
instance FrontState Window

type ActiveWindow = Window

emptyWindow :: Window
emptyWindow = Window "" ""

