-- |
-- Module: WildBind.X11
-- Description: X11-specific implementation for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- Maybe this should be released as a separate package.
{-# LANGUAGE MultiParamTypeClasses #-}
module WildBind.X11 () where

import Control.Applicative ((<$>))

import Graphics.X11.Xlib (Display, openDisplay)
import Data.Text (Text)

import WildBind (FrontState, FrontInputDevice(..), FrontEventSource(..))
import WildBind.NumPad (NumPadUnlockedInput(..))

data ActiveWindow = ActiveWindow {
  awName :: Text,
  awDesc :: Text
} deriving (Eq,Ord,Show)
instance FrontState ActiveWindow

data X11Handle = X11Handle {
  x11Display :: Display
}

initX11Handle :: IO X11Handle
initX11Handle = X11Handle <$> openDisplay ""

instance FrontInputDevice X11Handle NumPadUnlockedInput where
  defaultActionDescription = undefined
  setGrab = undefined

instance FrontEventSource X11Handle ActiveWindow NumPadUnlockedInput where
  nextEvent = undefined
