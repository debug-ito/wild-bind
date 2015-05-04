module WildBind.X11Spec (main, spec) where

import System.IO (hPutStrLn,stderr)
import System.Environment (lookupEnv)
import Control.Applicative ((<$>))
import Test.Hspec

import WildBind (setGrab,unsetGrab,nextEvent,FrontEvent)
import qualified WildBind.NumPad as NumPad
import WildBind.X11 (initX11Front,ActiveWindow,X11Front)

main :: IO ()
main = hspec spec

checkEnv :: String -> IO Bool
checkEnv env_name = (== "1") <$> maybe "" id <$> lookupEnv env_name

whenEnv :: String -> Expectation -> Expectation
whenEnv env_name exp = do
  ret <- checkEnv env_name
  if ret
  then exp
  else pendingWith ("You need to set " ++ env_name ++ "=1 to run the test")


whenNumPad :: Expectation -> Expectation
whenNumPad = whenEnv "WILDBIND_TEST_NUMPAD"

p :: String -> IO ()
p = hPutStrLn stderr . ("--- " ++)

nextEvent' :: X11Front -> IO (FrontEvent ActiveWindow NumPad.NumPadUnlockedInput)
nextEvent' = nextEvent

spec :: Spec
spec = do
  describe "X11Front" $ do
    it "should grab/ungrab keys" $ whenNumPad $ do
      f <- initX11Front
      setGrab f NumPad.NumMulti
      p "Press some numpad keys> "
      ev <- nextEvent' f
      p $ show ev

