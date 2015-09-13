-- |
-- Module: WildBind.X11
-- Description: X11-specific implementation for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module WildBind.X11 (
  -- * X11 front-end handle
  X11Front,
  withX11Front,
  -- * Windows in X11
  Window,
  ActiveWindow,
  -- ** Accessor functions for Window
  winInstance,
  winClass,
  winName
) where

import Control.Exception (bracket)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Bits ((.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import qualified Graphics.X11.Xlib as Xlib
import Control.Monad.Trans.Maybe (MaybeT,runMaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT(ContT), runContT)

import WildBind (FrontInputDevice(..), FrontEventSource(..), FrontEvent(FEInput,FEChange))
import WildBind.NumPad (NumPadUnlockedInput, NumPadLockedInput, descriptionForUnlocked, descriptionForLocked)

import WildBind.X11.Internal.Key (KeySymLike, ModifierLike, xEventToKeySymLike, xGrabKey, xUngrabKey)
import WildBind.X11.Internal.Window (ActiveWindow,getActiveWindow, Window, winInstance, winClass, winName)
import qualified WildBind.X11.Internal.NotificationDebouncer as Ndeb

-- | The X11 front-end
data X11Front = X11Front {
  x11Display :: Xlib.Display,
  x11Debouncer :: Ndeb.Debouncer,
  x11PrevActiveWindow :: IORef ActiveWindow
}

x11RootWindow :: X11Front -> Xlib.Window
x11RootWindow = Xlib.defaultRootWindow . x11Display

openMyDisplay :: IO Xlib.Display
openMyDisplay = Xlib.openDisplay ""

-- | Initialize and obtain 'X11Front' object, and run the given
-- action.
withX11Front :: (X11Front -> IO a) -> IO a
withX11Front = runContT $ do
  disp <- ContT $ bracket openMyDisplay Xlib.closeDisplay
  notif_disp <- ContT $ bracket openMyDisplay Xlib.closeDisplay
  debouncer <- ContT $ Ndeb.withDebouncer notif_disp
  liftIO $ Xlib.selectInput disp (Xlib.defaultRootWindow disp)
    (Xlib.substructureNotifyMask .|. Ndeb.xEventMask)
  awin_ref <- liftIO $ newIORef =<< getActiveWindow disp
  return $ X11Front disp debouncer awin_ref

convertEvent :: (KeySymLike k) => Xlib.Display -> Ndeb.Debouncer -> Xlib.XEventPtr -> MaybeT IO (FrontEvent ActiveWindow k)
convertEvent disp deb xev = do
  xtype <- liftIO $ Xlib.get_EventType xev
  let is_key_event = xtype == Xlib.keyRelease
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

filterUnchangedEvent :: X11Front -> FrontEvent ActiveWindow k -> MaybeT IO ()
filterUnchangedEvent front (FEChange new_state) = do
  old_state <- liftIO $ readIORef $ x11PrevActiveWindow front
  if new_state == old_state then empty else return ()
filterUnchangedEvent _ _ = return ()

updateState :: X11Front -> FrontEvent ActiveWindow k -> IO ()
updateState front fev = do
  let new_state = case fev of
        (FEInput s _) -> s
        (FEChange s) -> s
    in writeIORef (x11PrevActiveWindow front) new_state

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


instance (KeySymLike k) => FrontEventSource X11Front ActiveWindow k where
  nextEvent handle = Xlib.allocaXEvent $ \xev -> do
    Xlib.nextEvent (x11Display handle) xev
    maybe (nextEvent handle) return =<< (runMaybeT $ processEvent xev)
    where
      processEvent xev = do
        fevent <- convertEvent (x11Display handle) (x11Debouncer handle) xev
        filterUnchangedEvent handle fevent
        liftIO $ updateState handle fevent
        return fevent
