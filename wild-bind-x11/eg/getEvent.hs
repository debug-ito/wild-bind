module Main (main) where

import WildBind (FrontEnd(frontSetGrab, frontNextEvent), FrontEvent)
import WildBind.Input.NumPad (NumPadUnlocked(NumCenter))
import WildBind.X11 (withFrontEnd, ActiveWindow)

main :: IO ()
main = withFrontEnd $ \x11 -> do
  frontSetGrab x11 NumCenter
  loop x11
    where
      loop x11 = do
        printEvent =<< frontNextEvent x11
        loop x11

printEvent :: FrontEvent ActiveWindow NumPadUnlocked -> IO ()
printEvent = print
