-- |
-- Module: WildBind.X11
-- Description: X11-specific implementation for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, FlexibleInstances #-}
module WildBind.X11 (
  X11Front,
  ActiveWindow,
  initX11Front
) where

import Control.Applicative ((<$>))

import Graphics.X11.Xlib (Display, Window, XEventPtr)
import qualified Graphics.X11.Xlib as Xlib
import Data.Text (Text)

import WildBind (FrontState, FrontInput, FrontInputDevice(..), FrontEventSource(..), FrontEvent(FEInput))
import WildBind.NumPad (NumPadUnlockedInput, NumPadLockedInput, descriptionForUnlocked, descriptionForLocked)

import WildBind.X11.Internal.Key (KeySymLike, ModifierLike, xEventToKeySymLike, xGrabKey, xUngrabKey)

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

convertEvent :: (KeySymLike k) => XEventPtr -> IO (Maybe k)
convertEvent xev = convert' =<< Xlib.get_EventType xev
  where
    convert' xtype
      | xtype == Xlib.keyRelease = xEventToKeySymLike xev
      | otherwise = return Nothing

grabDef :: (KeySymLike k, ModifierLike k) => (Xlib.Display -> Xlib.Window -> k -> IO ()) -> X11Front -> k -> IO ()
grabDef func front key = func (x11Display front) (x11RootWindow front) key

instance FrontInputDevice X11Front NumPadUnlockedInput where
  setGrab = grabDef xGrabKey
  unsetGrab = grabDef xUngrabKey
  defaultActionDescription _ = descriptionForUnlocked

instance FrontInputDevice X11Front NumPadLockedInput where
  setGrab = grabDef xGrabKey
  unsetGrab = grabDef xUngrabKey
  defaultActionDescription _ = descriptionForLocked


instance (FrontInput k, KeySymLike k) => FrontEventSource X11Front ActiveWindow k where
  nextEvent handle = Xlib.allocaXEvent $ \xev -> do
    Xlib.nextEvent (x11Display handle) xev
    maybe (nextEvent handle) (return . FEInput emptyActiveWindow) =<< convertEvent xev
    
