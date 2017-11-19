-- |
-- Module: WildBind.X11
-- Description: X11-specific implementation for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
-- This module exports a 'FrontEnd' for X11 environments.
module WildBind.X11
       ( -- * X11 front-end
         withFrontEnd,
         -- * Windows in X11
         Window,
         ActiveWindow,
         -- ** Accessor functions for Window
         winInstance,
         winClass,
         winName
       ) where

import Control.Applicative ((<$>), empty)
import Control.Concurrent (rtsSupportsBoundThreads)
import Control.Concurrent.STM (atomically, TChan, newTChanIO, tryReadTChan, writeTChan)
import Control.Exception (bracket, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT(ContT), runContT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.List (ListT(ListT), runListT)
import Control.Monad.Trans.Writer (WriterT, execWriterT, tell)
import Data.Bits ((.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Graphics.X11.Xlib as Xlib

import WildBind
  ( FrontEnd(FrontEnd, frontDefaultDescription, frontSetGrab, frontUnsetGrab, frontNextEvent),
    FrontEvent(FEInput,FEChange)
  )
import qualified WildBind.Description as WBD

import WildBind.X11.Internal.Key
  ( xEventToXKeyInput,
    xGrabKey, xUngrabKey,
    XInputKey, KeyMaskMap, getKeyMaskMap
  )
import WildBind.X11.Internal.Window (ActiveWindow,getActiveWindow, Window, winInstance, winClass, winName, emptyWindow)
import qualified WildBind.X11.Internal.NotificationDebouncer as Ndeb

-- | The X11 front-end
data X11Front k =
  X11Front { x11Display :: Xlib.Display,
             x11Debouncer :: Ndeb.Debouncer,
             x11PrevActiveWindow :: IORef (Maybe ActiveWindow),
             x11PendingEvents :: TChan (FrontEvent ActiveWindow k),
             x11KeyMaskMap :: KeyMaskMap
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
-- The X11 'FrontEnd' watches and provides 'ActiveWindow' as the
-- front-end state. 'ActiveWindow' keeps information about the window
-- currently active. As for the input type @i@,
-- 'WildBind.Input.NumPad.NumPadUnlocked' and
-- 'WildBind.Input.NumPad.NumPadLocked' are currently supported.
-- 
-- Code using this function must be compiled __with @-threaded@ option enabled__
-- in @ghc@. Otherwise, it aborts.
--
-- Note that bound actions are executed when the key is released. That
-- way, you can deliver events to the window that originally has the
-- keyboard focus.
--
withFrontEnd :: (XInputKey i, WBD.Describable i) => (FrontEnd ActiveWindow i -> IO a) -> IO a
withFrontEnd = if rtsSupportsBoundThreads then impl else error_impl where
  impl = runContT $ do
    disp <- ContT $ bracket openMyDisplay Xlib.closeDisplay
    keymask_map <- liftIO $ getKeyMaskMap disp
    notif_disp <- ContT $ bracket openMyDisplay Xlib.closeDisplay
    debouncer <- ContT $ Ndeb.withDebouncer notif_disp
    liftIO $ Xlib.selectInput disp (Xlib.defaultRootWindow disp)
      (Xlib.substructureNotifyMask .|. Ndeb.xEventMask)
    awin_ref <- liftIO $ newIORef Nothing
    pending_events <- liftIO $ newTChanIO
    liftIO $ Ndeb.notify debouncer
    return $ makeFrontEnd $ X11Front disp debouncer awin_ref pending_events keymask_map
  error_impl _ = throwIO $ userError "You need to build with -threaded option when you use WildBind.X11.withFrontEnd function."

tellElem :: Monad m => a -> WriterT [a] m ()
tellElem a = tell [a]

convertEvent :: (XKeyInput k) => Xlib.Display -> Ndeb.Debouncer -> Xlib.XEventPtr -> ListT IO (FrontEvent ActiveWindow k)
convertEvent disp deb xev = ListT $ execWriterT $ convertEventWriter where
  convertEventWriter :: XKeyInput k => WriterT [FrontEvent ActiveWindow k] IO ()
  convertEventWriter = do
    xtype <- liftIO $ Xlib.get_EventType xev
    let is_key_event = xtype == Xlib.keyRelease
        is_awin_event = xtype == Xlib.configureNotify || xtype == Xlib.destroyNotify
        tellChangeEvent = (tellElem . FEChange) =<< (liftIO $ getActiveWindow disp)
    is_deb_event <- liftIO $ Ndeb.isDebouncedEvent deb xev
    if is_key_event
      then do
        tellChangeEvent
        (maybe (return ()) tellElem) =<< (liftIO $ runMaybeT (FEInput <$> xEventToXKeyInput xev))
    else if is_deb_event
      then tellChangeEvent
    else if is_awin_event
      then liftIO (Ndeb.notify deb) >> return ()
    else return ()

filterUnchangedEvent :: X11Front k -> FrontEvent ActiveWindow k -> ListT IO ()
filterUnchangedEvent front (FEChange new_state) = do
  m_old_state <- liftIO $ readIORef $ x11PrevActiveWindow front
  case m_old_state of
   Nothing -> return ()
   Just old_state -> if new_state == old_state then empty else return ()
filterUnchangedEvent _ _ = return ()

updateState :: X11Front k -> FrontEvent ActiveWindow k -> IO ()
updateState front fev = case fev of
  (FEInput _) -> return ()
  (FEChange s) -> writeIORef (x11PrevActiveWindow front) (Just s)

grabDef :: (XInputKey k) => (KeyMaskMap -> Xlib.Display -> Xlib.Window -> k -> IO ()) -> X11Front k -> k -> IO ()
grabDef func front key = func (x11KeyMaskMap front) (x11Display front) (x11RootWindow front) key

nextEvent :: (XInputKey k) => X11Front k -> IO (FrontEvent ActiveWindow k)
nextEvent handle = loop where
  loop = do
    mpending <- x11PopPendingEvent handle
    case mpending of
      Just eve -> return eve
      Nothing -> nextEventFromX11
  nextEventFromX11 = Xlib.allocaXEvent $ \xev -> do
    Xlib.nextEvent (x11Display handle) xev
    got_events <- processEvents xev
    case got_events of
      [] -> loop
      (eve : rest) -> do
        x11UnshiftPendingEvents handle rest
        return eve
  processEvents xev = runListT $ do
    fevent <- convertEvent (x11KeyMaskMap handle) (x11Display handle) (x11Debouncer handle) xev
    filterUnchangedEvent handle fevent
    liftIO $ updateState handle fevent
    return fevent

makeFrontEnd :: (XInputKey k, WBD.Describable k) => X11Front k -> FrontEnd ActiveWindow k
makeFrontEnd f = FrontEnd { frontDefaultDescription = WBD.describe,
                            frontSetGrab = grabDef xGrabKey f,
                            frontUnsetGrab = grabDef xUngrabKey f,
                            frontNextEvent = nextEvent f
                          }
