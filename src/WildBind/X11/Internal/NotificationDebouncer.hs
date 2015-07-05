-- |
-- Module: WildBind.X11.Internal.NotificationDebouncer
-- Description: debouce X11 notification events
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. End-users should not rely on it.
--
-- WildBind.X11 module receives some notification events to update the
-- current state of the desktop (usually it is the active
-- window). However, there are some problems in updating the state
-- every time it receives a notification event.
--
-- * Notification events can come too fast. It can make siginificant
--   overhead to the system.
--
-- * The active window obtained at the very moment a notification
--   arrives is often unstable. It can become invalid soon. In
--   addition, Xlib is notorious for being bad at handling that kind
--   of exceptions (it just crashes the entire process and it's
--   practically impossible to catch the exceptions).
--
-- Personally, I have experienced even weirder behaviors when I did
-- some X11 operations at arrivals of notification events.
--
-- * Sometimes I could not obtain the current active window. Instead,
--   I ended up with getting the previous active window.
-- 
-- * Sometimes GetWindowProperty blocked forever.
-- 
-- So, as a workaround, we debouce the raw notification events and
-- generate a ClientMessage X11 event. When we get the ClientMessage,
-- we update the state.

-- Toshio's personal note: 2015/05/06, 2010/12/05 - 19

module WildBind.X11.Internal.NotificationDebouncer (
  Debouncer,
  new,
  close,
  notify,
  xEventMask,
  isDebouncedEvent
) where

import Control.Applicative ((<$>), (<*>))
import qualified Graphics.X11.Xlib as Xlib
import qualified Graphics.X11.Xlib.Extras as XlibE
import qualified Control.FoldDebounce as Fdeb

data Debouncer = Debouncer {
  ndTrigger :: Fdeb.Trigger () (),
  ndMessageType :: Xlib.Atom
}

new :: Xlib.Display -- ^ a handle to send ClientMessage event.
                    -- It should be a different handle from the one held by 'X11Front'
    -> IO Debouncer
new disp = do
  mtype <- Xlib.internAtom disp "_WILDBIND_NOTIFY_CHANGE" False
  Debouncer <$> newTrigger disp mtype <*> return mtype

close :: Debouncer -> IO ()
close = Fdeb.close . ndTrigger

-- | Notify the 'Debouncer' that a notification event arrives.
notify :: Debouncer -> IO ()
notify deb = Fdeb.send (ndTrigger deb) ()

newTrigger :: Xlib.Display -> Xlib.Atom -> IO (Fdeb.Trigger () ())
newTrigger disp mtype = Fdeb.new (Fdeb.forVoid $ sendClientMessage disp mtype)
                           Fdeb.def { Fdeb.delay = 50000, Fdeb.alwaysResetTimer = True }

-- | The Xlib EventMask for sending the ClientMessage. You have to
-- select this mask by 'selectInput' function to receive the
-- ClientMessage.
xEventMask :: Xlib.EventMask
xEventMask = Xlib.substructureNotifyMask

sendClientMessage :: Xlib.Display -> Xlib.Atom -> IO ()
sendClientMessage disp mtype = Xlib.allocaXEvent $ \xev -> do
  let root_win = Xlib.defaultRootWindow disp
  XlibE.setEventType xev Xlib.clientMessage
  XlibE.setClientMessageEvent xev root_win mtype 8 0 0
  Xlib.sendEvent disp root_win False xEventMask xev

-- | Check if the given event is the debounced event.
isDebouncedEvent :: Debouncer -> Xlib.XEventPtr -> IO Bool
isDebouncedEvent deb xev = do
  ev <- XlibE.getEvent xev
  let exp_type = ndMessageType deb
  case ev of
    XlibE.ClientMessageEvent _ _ _ _ _ got_type _ -> return (got_type == exp_type)
    _ -> return False
