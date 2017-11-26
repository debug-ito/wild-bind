-- |
-- Module: WildBind.X11.Emulate
-- Description: X11 event emulation functions
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module WildBind.X11.Emulate
       ( sendKeyEventTo
       ) where

import Control.Monad.IO.Class (MonadIO)

import WildBind.X11.Internal.FrontEnd
  ( X11Front(..)
  )
import WildBind.X11.Internal.Key
  ( XKeyEvent
  )
import WildBind.X11.Internal.Window
  ( Window
  )

-- | Send a 'XKeyEvent' to the 'Window'.
sendKeyEventTo :: MonadIO m => X11Front k -> Window -> XKeyEvent -> m ()
sendKeyEventTo = undefined
