module WildBind.X11.EmulateSpec (main,spec) where

import qualified Graphics.X11.Xlib as Xlib
import Test.Hspec

import WildBind.X11
  ( withX11Front, makeFrontEnd,
    XMod(..), (.+), release
  )

import WildBind.X11.TestUtil (checkIfX11Available, withGrabs)

main :: IO ()
main = hspec spec

spec :: Spec
spec = checkIfX11Available $ describe "sendKeyEvent" $ do
  it "should send key event" $ withX11Front $ \x11 -> do
    let front = makeFrontEnd x11
        inputs = [ Alt .+ Super .+ Xlib.xK_r,
                   release Xlib.xK_w
                 ]
    withGrabs front inputs $ do
      True `shouldBe` False -- TODO
    
