{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module WildBind.X11Spec (main, spec) where

import System.IO (hPutStrLn,stderr)
import System.Environment (lookupEnv)
import Control.Exception (finally)
import Control.Applicative ((<$>))
import Test.Hspec

import WildBind (setGrab,unsetGrab,nextEvent,FrontEvent(FEChange,FEInput),FrontInputDevice,FrontEventSource)
import qualified WildBind.NumPad as NumPad
import WildBind.X11 (initX11Front,ActiveWindow)

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

grabExp :: forall f i
           . (Bounded i, Enum i, Show i, FrontInputDevice f i, FrontEventSource f ActiveWindow i)
           => f -> i -> Expectation
grabExp front grab_input = grabExpMain `finally` releaseAll where
  grabExpMain = do
    setGrab front grab_input
    p ("Press some numpad keys (grab="++ show grab_input ++")..")
    ev <- nextEvent front :: IO (FrontEvent ActiveWindow i)
    p ("Got event: " ++ show ev)
    case ev of
      FEChange _ -> expectationFailure "FEChange is caught. not expected"
      FEInput _ got -> do
        got `shouldBe` grab_input
  releaseAll = mapM_ (unsetGrab front) (enumFromTo minBound maxBound :: [i])

spec :: Spec
spec = do
  describe "X11Front - NumPadUnlockedInput" $ do
    it "should grab/ungrab keys" $ whenNumPad $ do
      f <- initX11Front
      mapM_ (grabExp f) (enumFromTo minBound maxBound :: [NumPad.NumPadUnlockedInput] )
  describe "X11Front - NumPadLockedInput" $ do
    it "should grab/ungrab keys" $ whenNumPad $ do
      f <- initX11Front
      mapM_ (grabExp f) (enumFromTo minBound maxBound :: [NumPad.NumPadLockedInput] )


