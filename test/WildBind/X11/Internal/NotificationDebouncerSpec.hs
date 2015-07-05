module WildBind.X11.Internal.NotificationDebouncerSpec (spec) where

import Test.Hspec

import qualified Graphics.X11.Xlib as Xlib
import qualified WildBind.X11.Internal.NotificationDebouncer as Deb


spec :: Spec
spec = do
  describe "notify" $ do
    it "should create an X event that passes isDeboucedEvent" $ do
      disp <- Xlib.openDisplay ""
      Xlib.selectInput disp (Xlib.defaultRootWindow disp) Deb.xEventMask
      deb <- Deb.new =<< Xlib.openDisplay ""
      Deb.notify deb
      (Xlib.allocaXEvent $ waitForDebouncedEvent deb disp) `shouldReturn` True
      Deb.close deb
        
        

waitForDebouncedEvent :: Deb.Debouncer -> Xlib.Display -> Xlib.XEventPtr -> IO Bool
waitForDebouncedEvent deb disp xev = doit 0 where
  doit :: Int -> IO Bool
  doit count = do
    Xlib.nextEvent disp xev
    ret <- Deb.isDebouncedEvent deb xev -- is it ok to use 'disp' (different Display from the one Debouncer uses)
    if ret || (count > 20)
      then return ret
      else do
        putStrLn ("Unexpected event: " ++ show xev)
        doit (count + 1)
        

      
