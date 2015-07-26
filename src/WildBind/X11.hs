-- |
-- Module: WildBind.X11
-- Description: X11-specific implementation for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module WildBind.X11 (
  X11Front,
  ActiveWindow,
  withX11Front
) where

import Control.Exception (bracket)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Bits ((.|.))

import qualified Graphics.X11.Xlib as Xlib
import Control.Monad.Trans.Maybe (MaybeT,runMaybeT)
import Control.Monad.IO.Class (liftIO)

import WildBind (FrontInput, FrontInputDevice(..), FrontEventSource(..), FrontEvent(FEInput,FEChange))
import WildBind.NumPad (NumPadUnlockedInput, NumPadLockedInput, descriptionForUnlocked, descriptionForLocked)

import WildBind.X11.Internal.Key (KeySymLike, ModifierLike, xEventToKeySymLike, xGrabKey, xUngrabKey)
import WildBind.X11.Internal.Window (ActiveWindow,getActiveWindow)
import qualified WildBind.X11.Internal.NotificationDebouncer as Ndeb

-- | The X11 front-end
data X11Front = X11Front {
  x11Display :: Xlib.Display,
  x11Debouncer :: Ndeb.Debouncer
}

x11RootWindow :: X11Front -> Xlib.Window
x11RootWindow = Xlib.defaultRootWindow . x11Display

openMyDisplay :: IO Xlib.Display
openMyDisplay = Xlib.openDisplay ""

-- | Initialize and obtain 'X11Front' object, and run the given
-- action.
withX11Front :: (X11Front -> IO a) -> IO a
withX11Front action =
  bracket openMyDisplay Xlib.closeDisplay $ \disp ->
    bracket openMyDisplay Xlib.closeDisplay $ \notif_disp ->
      Ndeb.withDebouncer notif_disp $ \debouncer -> do
        Xlib.selectInput disp (Xlib.defaultRootWindow disp)
          (Xlib.substructureNotifyMask .|. Ndeb.xEventMask)
        action $ X11Front disp debouncer

convertEvent :: (KeySymLike k) => X11Front -> Xlib.XEventPtr -> MaybeT IO (FrontEvent ActiveWindow k)
convertEvent front xev = do
  xtype <- liftIO $ Xlib.get_EventType xev
  let disp = x11Display front
      deb = x11Debouncer front
      is_key_event = xtype == Xlib.keyRelease
      is_awin_event = xtype == Xlib.configureNotify || xtype == Xlib.destroyNotify
      getAWin = liftIO $ getActiveWindow disp
  is_deb_event <- liftIO $ Ndeb.isDebouncedEvent deb xev
  if is_key_event
    then FEInput <$> getAWin <*> xEventToKeySymLike xev
  else if is_deb_event
    then FEChange <$> getAWin
  else if is_awin_event
    then liftIO (Ndeb.notify deb) >> empty
  else empty


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
    maybe (nextEvent handle) return =<< (runMaybeT $ convertEvent handle xev)
    
