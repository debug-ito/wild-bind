module Main (main) where

import WildBind (FrontEnd(frontSetGrab, frontNextEvent), FrontEvent)
import WildBind.Input.NumPad (NumPadUnlockedInput(NumCenter))
import WildBind.X11 (withX11Front, ActiveWindow)

main :: IO ()
main = withX11Front $ \x11 -> do
  frontSetGrab x11 NumCenter
  loop x11
    where
      loop x11 = do
        printEvent =<< frontNextEvent x11
        loop x11

printEvent :: FrontEvent ActiveWindow NumPadUnlockedInput -> IO ()
printEvent = print
