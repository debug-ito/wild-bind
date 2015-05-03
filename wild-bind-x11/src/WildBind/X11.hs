-- |
-- Module: WildBind.X11
-- Description: X11-specific implementation for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module WildBind.X11 (
  X11Front,
  ActiveWindow,
  initX11Front
) where

import Control.Applicative ((<$>))

import Graphics.X11.Xlib (Display, Window, XEventPtr)
import qualified Graphics.X11.Xlib as Xlib
import Data.Text (Text)

import WildBind (FrontState, FrontInputDevice(..), FrontEventSource(..), FrontEvent(FEInput))
import WildBind.NumPad (NumPadUnlockedInput(..), descriptionForUnlocked)

import WildBind.X11.Internal.Key (xEventToKeySymLike, xGrabKey, xUngrabKey)

-- | Information of the currently active window.
data ActiveWindow = ActiveWindow {
  awName :: Text, -- ^ name of the window
  awDesc :: Text  -- ^ description of the window
} deriving (Eq,Ord,Show)
instance FrontState ActiveWindow

emptyActiveWindow :: ActiveWindow
emptyActiveWindow = ActiveWindow "" ""

-- | The X11 front-end
data X11Front = X11Front {
  x11Display :: Display
}

x11RootWindow :: X11Front -> Window
x11RootWindow = Xlib.defaultRootWindow . x11Display

-- | Initialize and obtain 'X11Front' object.
initX11Front :: IO X11Front
initX11Front = X11Front <$> Xlib.openDisplay ""

convertEvent :: XEventPtr -> IO (Maybe NumPadUnlockedInput)
convertEvent xev = convert' =<< Xlib.get_EventType xev
  where
    convert' xtype
      | xtype == Xlib.keyRelease = xEventToKeySymLike xev
      | otherwise = return Nothing

instance FrontInputDevice X11Front NumPadUnlockedInput where
  setGrab f key = xGrabKey (x11Display f) (x11RootWindow f) key
  unsetGrab f key = xUngrabKey (x11Display f) (x11RootWindow f) key
  defaultActionDescription _ = descriptionForUnlocked


instance FrontEventSource X11Front ActiveWindow NumPadUnlockedInput where
  nextEvent handle = Xlib.allocaXEvent $ \xev -> do
    Xlib.nextEvent (x11Display handle) xev
    maybe (nextEvent handle) (return . FEInput emptyActiveWindow) =<< convertEvent xev
    
