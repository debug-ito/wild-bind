module WildBind.X11Spec (main, spec) where

import System.IO (hPutStrLn,stderr)
import System.Environment (lookupEnv)
import Control.Applicative ((<$>))
import Test.Hspec

import WildBind (setGrab,unsetGrab,nextEvent,FrontEvent(FEChange,FEInput))
import qualified WildBind.NumPad as NumPad
import WildBind.X11 (initX11Front,ActiveWindow,X11Front)

main :: IO ()
main = hspec spec

checkEnv :: String -> IO Bool
checkEnv env_name = (== "1") <$> maybe "" id <$> lookupEnv env_name

whenEnv :: String -> Expectation -> Expectation
whenEnv env_name act = do
  ret <- checkEnv env_name
  if ret
  then act
  else pendingWith ("You need to set " ++ env_name ++ "=1 to run the test")


whenNumPad :: Expectation -> Expectation
whenNumPad = whenEnv "WILDBIND_TEST_NUMPAD"

p :: String -> IO ()
p = hPutStrLn stderr . ("--- " ++)

nextEvent' :: X11Front -> IO (FrontEvent ActiveWindow NumPad.NumPadUnlockedInput)
nextEvent' = nextEvent

grabExp :: X11Front -> NumPad.NumPadUnlockedInput -> Expectation
grabExp front grab_input = do
  mapM_ (unsetGrab front) (enumFromTo minBound maxBound :: [NumPad.NumPadUnlockedInput])
  setGrab front grab_input
  p ("Press some numpad keys (grab="++ show grab_input ++")..")
  ev <- nextEvent' front
  p ("Got event: " ++ show ev)
  case ev of
    FEChange _ -> expectationFailure "FEChange is caught. not expected"
    FEInput _ got -> do
      got `shouldBe` grab_input

spec :: Spec
spec = do
  describe "X11Front" $ do
    it "should grab/ungrab keys" $ whenNumPad $ do
      f <- initX11Front
      grabExp f NumPad.NumMulti
      grabExp f NumPad.NumLeft
      grabExp f NumPad.NumInsert

