module WildBind.X11Spec (main, spec) where

import System.IO (hPutStrLn,stderr)
import Test.Hspec

import WildBind (setGrab,unsetGrab,nextEvent,FrontEvent)
import qualified WildBind.NumPad as NumPad
import WildBind.X11 (initX11Front,ActiveWindow,X11Front)

main :: IO ()
main = hspec spec

p :: String -> IO ()
p = hPutStrLn stderr . ("--- " ++)

nextEvent' :: X11Front -> IO (FrontEvent ActiveWindow NumPad.NumPadUnlockedInput)
nextEvent' = nextEvent

spec :: Spec
spec = do
  describe "X11Front" $ do
    it "should grab/ungrab keys" $ do
      f <- initX11Front
      setGrab f NumPad.NumMulti
      p "Press some numpad keys> "
      ev <- nextEvent' f
      p $ show ev
