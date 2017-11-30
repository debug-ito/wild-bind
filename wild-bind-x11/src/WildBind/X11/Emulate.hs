{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module: WildBind.X11.Emulate
-- Description: X11 event emulation functions
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module WildBind.X11.Emulate
       ( sendKeyEventTo,
         sendKeyEvent,
         pushTo,
         push,
         remap
       ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader.Class (MonadReader(ask))
import WildBind.Binding (Binding', bindsF, on, run)

import WildBind.X11.Internal.FrontEnd
  ( X11Front(..)
  )
import WildBind.X11.Internal.Key
  ( ToXKeyEvent(..), KeyEventType(..), XKeyEvent,
    xSendKeyEvent, press, release
  )
import WildBind.X11.Internal.Window
  ( Window, ActiveWindow, winID
  )

-- | Send a X11 key event to a 'Window'.
sendKeyEventTo :: (ToXKeyEvent k, MonadIO m)
               => X11Front i
               -> Window -- ^ target window
               -> k -- ^ Key event to send
               -> m ()
sendKeyEventTo front win key = liftIO $ xSendKeyEvent kmmap disp win_id key_event
  where
    kmmap = x11KeyMaskMap front
    disp = x11Display front
    win_id = winID win
    key_event = toXKeyEvent key

-- | Same as 'sendKeyEventTo', but the target window is obtained from
-- 'MonadReader'.
sendKeyEvent :: (ToXKeyEvent k, MonadIO m, MonadReader Window m) => X11Front i -> k -> m ()
sendKeyEvent front key = do
  win <- ask
  sendKeyEventTo front win key

-- | Send a \"key push\" event to a 'Window', that is, send 'KeyPress'
-- and 'KeyRelease' events.
pushTo :: (ToXKeyEvent k, MonadIO m) => X11Front i -> Window -> k -> m ()
pushTo front win key = do
  send $ press key
  send $ release key
  where
    send = sendKeyEventTo front win

-- | Same as 'pushTo', but the target window is obtained from
-- 'MonadReader'.
push :: (ToXKeyEvent k, MonadIO m, MonadReader Window m) => X11Front i -> k -> m ()
push front key = do
  win <- ask
  pushTo front win key

-- | Create a binding that remaps key event \"@from@\" to
-- \"@to@\".
--
-- This binding captures 'KeyPress' and 'KeyRelease' events of
-- \"@from@\", and sends respective events of \"@to@\" to the active
-- window.
remap :: (ToXKeyEvent from, ToXKeyEvent to) => X11Front i -> from -> to -> Binding' bs ActiveWindow XKeyEvent
remap front from to = bindsF $ do
  on (press from) `run` push' (press to)
  on (release from) `run` push' (release to)
  where
    push' = push front
