-- |
-- Module: WildBind.X11.Internal.Window
-- Description: types and functions related to X11 windows
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
{-# LANGUAGE OverloadedStrings #-}
module WildBind.X11.Internal.Window (
  -- * The 'Window' data type
  Window,
  ActiveWindow,
  -- * Accessor functions for 'Window'
  winAppName,
  winAppClass,
  winTitle,
  -- * Functions
  getActiveWindow
) where

import Data.Maybe (listToMaybe)
import Control.Applicative ((<$>))

import Data.Text (Text)
import qualified Graphics.X11.Xlib as Xlib
import qualified Graphics.X11.Xlib.Extras as XlibE

import WildBind (FrontState)

-- | Information about window. You can inspect properties 'winAppName'
-- and 'winAppClass' by @wmctrl@ command.
--
-- > $ wmctrl -lx
-- > 0x01400004 -1 xfce4-panel.Xfce4-panel  mydesktop xfce4-panel
-- > 0x01800003 -1 xfdesktop.Xfdesktop   mydesktop desktop
-- > 0x03800004  0 xfce4-terminal.Xfce4-terminal  mydesktop Terminal - toshio@mydesktop - byobu
-- > 0x03a000a7  0 emacs.Emacs23         mydesktop emacs@mydesktop
-- > 0x03e010fc  0 Navigator.Firefox     mydesktop debug-ito (Toshio Ito) - Mozilla Firefox
-- > 0x02600003  0 totem.Totem           mydesktop Movie Player
--
-- In the above example, the third column shows @winAppName.winAppClass@.
data Window = Window {
  winAppName :: Text,  -- ^ name of the application
  winAppClass :: Text, -- ^ class name of the application
  winTitle :: Text  -- ^ what's shown in the title bar
} deriving (Eq,Ord,Show)
instance FrontState Window

-- | Use this type especially when the 'Window' is active.
type ActiveWindow = Window

-- | An empty Window instance used for fallback
emptyWindow :: Window
emptyWindow = Window "" "" ""

-- | Get currently active 'Window'.
getActiveWindow :: Xlib.Display -> IO ActiveWindow
getActiveWindow = undefined


-- | Check whether specified feature is supported by the window
-- manager(?) Port of libxdo's _xdo_ewmh_is_supported() function.
ewmhIsSupported :: Xlib.Display -> String -> IO Bool
ewmhIsSupported disp feature_str = do
  req <- Xlib.internAtom disp "_NET_SUPPORTED" False
  feature <- Xlib.internAtom disp feature_str False
  result <- XlibE.getWindowProperty32 disp req (Xlib.defaultRootWindow disp)
  case result of
    Nothing -> return False
    Just atoms -> return $ any ((feature ==) . fromIntegral) atoms

-- | Get X11 Window handle for the active window. Port of libxdo's
-- xdo_window_get_active() function.
xGetActiveWindow :: Xlib.Display -> IO (Maybe Xlib.Window)
xGetActiveWindow disp = do
  let req_str = "_NET_ACTIVE_WINDOW"
  supported <- ewmhIsSupported disp req_str
  if not supported
    then return Nothing
    else do
    req <- Xlib.internAtom disp req_str False
    result <- XlibE.getWindowProperty32 disp req (Xlib.defaultRootWindow disp)
    return (fromIntegral <$> maybe Nothing listToMaybe result)

-- Doesn't it actually get window "title", does it? WM_NAME (or
-- _NET_WM_NAME) is supposed to be shown in the title bar.
--
-- -- | Get the window name for the X11 window. The window name refers to
-- -- @_NET_WM_NAME@ or @WM_NAME@. Port of libxdo's xdo_get_window_name()
-- -- function.
-- xGetWindowName :: Xlib.Display -> Xlib.Window -> IO Text
-- xGetWindowName disp win = do
--   req <- Xlib.internAtom disp "_NET_WM_NAME" False
--   prop <- XlibE.getTextProperty disp win req


