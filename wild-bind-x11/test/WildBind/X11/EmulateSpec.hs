module WildBind.X11.EmulateSpec (main,spec) where

import Control.Monad (forM_)
import qualified Graphics.X11.Xlib as Xlib
import Test.Hspec

import WildBind (frontNextEvent, FrontEvent(..))
import WildBind.X11
  ( withX11Front, makeFrontEnd,
    XMod(..), (.+), release,
    defaultRootWindow
  )
import WildBind.X11.Emulate (sendKeyEventTo)

import WildBind.X11.TestUtil (checkIfX11Available, withGrabs)

main :: IO ()
main = hspec spec

spec :: Spec
spec = checkIfX11Available $ describe "sendKeyEventTo" $ do
  it "should send key event" $ withX11Front $ \x11 -> do
    let front = makeFrontEnd x11
        inputs = [ Alt .+ Super .+ Xlib.xK_r,
                   release Xlib.xK_w
                 ]
    _ <- frontNextEvent front -- discard the first FEChange
    withGrabs front inputs $ do
      forM_ inputs $ \input -> do
        sendKeyEventTo x11 (defaultRootWindow x11) input
        frontNextEvent front `shouldReturn` FEInput input
    
