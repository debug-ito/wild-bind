module WildBind.X11.EmulateSpec (main,spec) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Bits ((.|.))
import qualified Graphics.X11.Xlib as Xlib
import Test.Hspec

import WildBind (frontNextEvent, FrontEvent(..))
import WildBind.X11
  ( withX11Front, makeFrontEnd,
    XMod(..), (.+), release, press,
    defaultRootWindow,
    XKeyEvent(..), KeyEventType(..)
  )
import WildBind.X11.Emulate (sendKeyEventTo)
import WildBind.X11.Internal.Key (xKeyEventToXKeyInput, getKeyMaskMap, KeyMaskMap)
import WildBind.X11.Internal.Window (fromWinID)

import WildBind.X11.TestUtil (checkIfX11Available)

main :: IO ()
main = hspec spec

spec :: Spec
spec = checkIfX11Available $ describe "sendKeyEventTo" $ do
  it "should send key event" $ withX11Front $ \x11 -> do
    let inputs = [ Alt .+ Super .+ Xlib.xK_r,
                   release Xlib.xK_w,
                   press Xlib.xK_Right,
                   release $ Ctrl .+ Shift .+ Xlib.xK_F14
                 ]
    bracket (Xlib.openDisplay "") Xlib.closeDisplay $ \disp -> do
      kmmap <- getKeyMaskMap disp
      win <- makeWindow disp
      putStrLn ("Window created: " ++ show win)
      forM_ inputs $ \input -> do
        Xlib.sync disp False
        putStrLn ("Do send input: " ++ show input)
        sendKeyEventTo x11 (fromWinID win) input
        putStrLn ("Receiving..")
        (nextKey kmmap disp) `shouldReturn` input

-- We have to create a dedicated window to receive events sent by
-- 'sendKeyEventTo', because XSendEvent ignores key grabs (so X11Front
-- cannot get the sent event.)
--
-- By the way, key events by XTEST extension emulate the real key
-- events from a real keyboard, so they are caught by key grabs.
--
-- c.f. http://t-sato.in.coocan.jp/xvkbd/events.html

makeWindow :: Xlib.Display -> IO Xlib.Window
makeWindow disp = do
  win <- Xlib.createSimpleWindow disp root x y w h border_width border_pixel bg_pixel
  -- Xlib.storeName disp win "test window"
  -- Xlib.mapWindow disp win
  Xlib.selectInput disp win select_mask
  Xlib.flush disp
  return win
  where
    root = Xlib.defaultRootWindow disp
    x = 0
    y = 0
    w = 50
    h = 50
    border_width = 1
    border_pixel = Xlib.blackPixel disp 0
    bg_pixel = Xlib.whitePixel disp 0
    select_mask = Xlib.keyPressMask .|. Xlib.keyReleaseMask

nextKey :: KeyMaskMap -> Xlib.Display -> IO XKeyEvent
nextKey kmmap disp = Xlib.allocaXEvent $ \xev -> do
  Xlib.nextEvent disp xev
  xtype <- Xlib.get_EventType xev
  putStrLn ("Got event type: " ++ show xtype)
  case toKeyType xtype of
   Nothing -> error ("Unknown event type: " ++ show xtype)
   Just key_type -> do
     putStrLn ("KeyEventType = " ++ show key_type)
     ret <- fmap unwrapMaybe $ runMaybeT $ xKeyEventToXKeyInput kmmap key_type $ Xlib.asKeyEvent xev
     putStrLn ("Converted: " ++ show ret)
     return ret
  where
    toKeyType xtype | xtype == Xlib.keyPress = Just KeyPress
                    | xtype == Xlib.keyRelease = Just KeyRelease
                    | otherwise = Nothing
    error_convert = error "Cannot convert the XEvent to XKeyEvent."
    unwrapMaybe = maybe error_convert id


    
