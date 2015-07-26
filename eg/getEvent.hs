module Main (main) where

import WildBind (FrontEventSource(nextEvent), FrontEvent, setGrab)
import WildBind.NumPad (NumPadUnlockedInput(NumCenter))
import WildBind.X11 (withX11Front, ActiveWindow)

main :: IO ()
main = withX11Front $ \x11 -> do
  setGrab x11 NumCenter
  loop x11
    where
      loop x11 = do
        printEvent =<< nextEvent x11
        loop x11

printEvent :: FrontEvent ActiveWindow NumPadUnlockedInput -> IO ()
printEvent = print
