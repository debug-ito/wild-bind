-- |
-- Module: WildBind.X11
-- Description: X11-specific implementation for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, FlexibleInstances #-}
module WildBind.X11 (
  X11Front,
  ActiveWindow,
  initX11Front,
  releaseX11Front
) where

import Control.Applicative ((<$>),empty)

import qualified Graphics.X11.Xlib as Xlib
import Control.Monad.Trans.Maybe (MaybeT,runMaybeT)
import Control.Monad.IO.Class (liftIO)

import WildBind (FrontState, FrontInput, FrontInputDevice(..), FrontEventSource(..), FrontEvent(FEInput,FEChange))
import WildBind.NumPad (NumPadUnlockedInput, NumPadLockedInput, descriptionForUnlocked, descriptionForLocked)

import WildBind.X11.Internal.Key (KeySymLike, ModifierLike, xEventToKeySymLike, xGrabKey, xUngrabKey)
import WildBind.X11.Internal.Window (ActiveWindow,getActiveWindow)

-- | The X11 front-end
data X11Front = X11Front {
  x11Display :: Xlib.Display
}

x11RootWindow :: X11Front -> Xlib.Window
x11RootWindow = Xlib.defaultRootWindow . x11Display

-- | Initialize and obtain 'X11Front' object.
initX11Front :: IO X11Front
initX11Front = do
  disp <- Xlib.openDisplay ""
  Xlib.selectInput disp (Xlib.defaultRootWindow disp) Xlib.substructureNotifyMask
  return $ X11Front disp

-- | Release all resource associated with 'X11Front'.
releaseX11Front :: X11Front -> IO ()
releaseX11Front = Xlib.closeDisplay . x11Display

convertEvent :: (KeySymLike k) => Xlib.Display -> Xlib.XEventPtr -> MaybeT IO (FrontEvent ActiveWindow k)
convertEvent disp xev = do
  xtype <- liftIO $ Xlib.get_EventType xev
  awin <- liftIO $ getActiveWindow disp
  convert' awin xtype
  where
    convert' awin xtype
      | xtype == Xlib.keyRelease = FEInput awin <$> xEventToKeySymLike xev
      | xtype == Xlib.configureNotify || xtype == Xlib.destroyNotify = return $ FEChange awin
      | otherwise = empty

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
    maybe (nextEvent handle) return =<< (runMaybeT $ convertEvent (x11Display handle) xev)
    
