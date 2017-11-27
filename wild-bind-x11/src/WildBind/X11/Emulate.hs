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

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import WildBind.Binding (Binding')

import WildBind.X11.Internal.FrontEnd
  ( X11Front(..)
  )
import WildBind.X11.Internal.Key
  ( ToXKeyEvent, KeyEventType(..), XKeyEvent
  )
import WildBind.X11.Internal.Window
  ( Window, ActiveWindow
  )

-- | Send a X11 key event to a 'Window'.
sendKeyEventTo :: (ToXKeyEvent k, MonadIO m)
               => X11Front i
               -> Window -- ^ target window
               -> k -- ^ Key event to send
               -> m ()
sendKeyEventTo = undefined

-- | Same as 'sendKeyEventTo', but the target window is obtained from
-- 'MonadReader'.
sendKeyEvent :: (ToXKeyEvent k, MonadIO m, MonadReader Window m) => X11Front i -> k -> m ()
sendKeyEvent = undefined

-- | Send a \"key push\" event to a 'Window', that is, send 'KeyPress'
-- and 'KeyRelease' events.
pushTo :: (ToXKeyEvent k, MonadIO m) => X11Front i -> Window -> k -> m ()
pushTo = undefined

-- | Same as 'pushTo', but the target window is obtained from
-- 'MonadReader'.
push :: (ToXKeyEvent k, MonadIO m, MonadReader Window m) => X11Front i -> k -> m ()
push = undefined

-- | Create a binding that remaps key event \"@from@\" to
-- \"@to@\".
--
-- This binding captures 'KeyPress' and 'KeyRelease' events of
-- \"@from@\", and sends respective events of \"@to@\" to the active
-- window.
remap :: (ToXKeyEvent from, ToXKeyEvent to) => X11Front i -> from -> to -> Binding' bs ActiveWindow XKeyEvent
remap = undefined
