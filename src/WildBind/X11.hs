-- |
-- Module: WildBind.X11
-- Description: X11-specific implementation for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module WildBind.X11 (
  -- * X11 front-end
  withFrontEnd,
  -- * Windows in X11
  Window,
  ActiveWindow,
  -- ** Accessor functions for Window
  winInstance,
  winClass,
  winName
) where

import Control.Exception (bracket)
import Control.Applicative ((<$>), empty)
import Data.Bits ((.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import qualified Graphics.X11.Xlib as Xlib
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT(ContT), runContT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.List (ListT(ListT), runListT)
import Control.Monad.Trans.Writer (WriterT, execWriterT, tell)
import Control.Concurrent.STM (atomically, TChan, newTChanIO, tryReadTChan, writeTChan)

import WildBind (
  FrontEnd(FrontEnd, frontDefaultDescription, frontSetGrab, frontUnsetGrab, frontNextEvent),
  FrontEvent(FEInput,FEChange)
  )
import qualified WildBind.Description as WBD

import WildBind.X11.Internal.Key (KeySymLike, ModifierLike, xEventToKeySymLike, xGrabKey, xUngrabKey)
import WildBind.X11.Internal.Window (ActiveWindow,getActiveWindow, Window, winInstance, winClass, winName, emptyWindow)
import qualified WildBind.X11.Internal.NotificationDebouncer as Ndeb

-- | The X11 front-end
data X11Front k = X11Front {
  x11Display :: Xlib.Display,
  x11Debouncer :: Ndeb.Debouncer,
  x11PrevActiveWindow :: IORef ActiveWindow,
  x11PendingEvents :: TChan (FrontEvent ActiveWindow k)
}

x11RootWindow :: X11Front k -> Xlib.Window
x11RootWindow = Xlib.defaultRootWindow . x11Display

x11PopPendingEvent :: X11Front k -> IO (Maybe (FrontEvent ActiveWindow k))
x11PopPendingEvent f = atomically $ tryReadTChan $ x11PendingEvents f

x11UnshiftPendingEvents :: X11Front k -> [FrontEvent ActiveWindow k] -> IO ()
x11UnshiftPendingEvents f = atomically . mapM_ (writeTChan $ x11PendingEvents f)

openMyDisplay :: IO Xlib.Display
openMyDisplay = Xlib.openDisplay ""

-- | Initialize and obtain 'FrontEnd' for X11, and run the given
-- action.
-- 
-- Code using this function must be compiled with @-threaded@ option
-- enabled in @ghc@. Otherwise, the behavior of the resulting action
-- is undefined.
--
-- For now, 'NumPadUnlockedInput' and 'NumPadLockedInput' are
-- supported as type @i@.
withFrontEnd :: (KeySymLike i, ModifierLike i, WBD.Describable i) => (FrontEnd ActiveWindow i -> IO a) -> IO a
withFrontEnd = runContT $ do
  disp <- ContT $ bracket openMyDisplay Xlib.closeDisplay
  notif_disp <- ContT $ bracket openMyDisplay Xlib.closeDisplay
  debouncer <- ContT $ Ndeb.withDebouncer notif_disp
  liftIO $ Xlib.selectInput disp (Xlib.defaultRootWindow disp)
    (Xlib.substructureNotifyMask .|. Ndeb.xEventMask)
  awin_ref <- liftIO $ newIORef emptyWindow
  pending_events <- liftIO $ newTChanIO
  liftIO $ Ndeb.notify debouncer
  return $ makeFrontEnd $ X11Front disp debouncer awin_ref pending_events

tellElem :: Monad m => a -> WriterT [a] m ()
tellElem a = tell [a]

convertEvent :: (KeySymLike k) => Xlib.Display -> Ndeb.Debouncer -> Xlib.XEventPtr -> ListT IO (FrontEvent ActiveWindow k)
convertEvent disp deb xev = ListT $ execWriterT $ convertEventWriter where
  convertEventWriter :: KeySymLike k => WriterT [FrontEvent ActiveWindow k] IO ()
  convertEventWriter = do
    xtype <- liftIO $ Xlib.get_EventType xev
    let is_key_event = xtype == Xlib.keyRelease
        is_awin_event = xtype == Xlib.configureNotify || xtype == Xlib.destroyNotify
        tellChangeEvent = (tellElem . FEChange) =<< (liftIO $ getActiveWindow disp)
    is_deb_event <- liftIO $ Ndeb.isDebouncedEvent deb xev
    if is_key_event
      then do
        tellChangeEvent
        (maybe (return ()) tellElem) =<< (liftIO $ runMaybeT (FEInput <$> xEventToKeySymLike xev))
    else if is_deb_event
      then tellChangeEvent
    else if is_awin_event
      then liftIO (Ndeb.notify deb) >> return ()
    else return ()

filterUnchangedEvent :: X11Front k -> FrontEvent ActiveWindow k -> ListT IO ()
filterUnchangedEvent front (FEChange new_state) = do
  old_state <- liftIO $ readIORef $ x11PrevActiveWindow front
  if new_state == old_state then empty else return ()
filterUnchangedEvent _ _ = return ()

updateState :: X11Front k -> FrontEvent ActiveWindow k -> IO ()
updateState front fev = case fev of
  (FEInput _) -> return ()
  (FEChange s) -> writeIORef (x11PrevActiveWindow front) s

grabDef :: (KeySymLike k, ModifierLike k) => (Xlib.Display -> Xlib.Window -> k -> IO ()) -> X11Front k -> k -> IO ()
grabDef func front key = func (x11Display front) (x11RootWindow front) key

nextEvent :: (KeySymLike k) => X11Front k -> IO (FrontEvent ActiveWindow k)
nextEvent handle = do
  mpending <- x11PopPendingEvent handle
  case mpending of
    Just eve -> return eve
    Nothing -> nextEventFromX11
  where
    nextEventFromX11 = Xlib.allocaXEvent $ \xev -> do
      Xlib.nextEvent (x11Display handle) xev
      got_events <- processEvents xev
      case got_events of
        [] -> loop
        (eve : rest) -> do
          x11UnshiftPendingEvents handle rest
          return eve
    processEvents xev = runListT $ do
      fevent <- convertEvent (x11Display handle) (x11Debouncer handle) xev
      filterUnchangedEvent handle fevent
      liftIO $ updateState handle fevent
      return fevent
    loop = nextEvent handle

makeFrontEnd :: (KeySymLike k, ModifierLike k, WBD.Describable k) => X11Front k -> FrontEnd ActiveWindow k
makeFrontEnd f = FrontEnd {
  frontDefaultDescription = WBD.describe,
  frontSetGrab = grabDef xGrabKey f,
  frontUnsetGrab = grabDef xUngrabKey f,
  frontNextEvent = nextEvent f
  }
