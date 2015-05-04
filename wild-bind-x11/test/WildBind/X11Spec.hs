{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module WildBind.X11Spec (main, spec) where

import System.IO (hPutStrLn,stderr)
import System.Environment (lookupEnv)
import Control.Exception (finally,bracket)
import Control.Applicative ((<$>))
import Test.Hspec

import WildBind (setGrab,unsetGrab,nextEvent,FrontEvent(FEChange,FEInput),FrontInputDevice,FrontEventSource)
import qualified WildBind.NumPad as NumPad
import WildBind.X11 (initX11Front,releaseX11Front,ActiveWindow,X11Front)

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

withX11Front :: (X11Front -> Expectation) -> Expectation
withX11Front act = bracket initX11Front releaseX11Front act

spec :: Spec
spec = do
  describe "X11Front - NumPadUnlockedInput" $ do
    it "should grab/ungrab keys" $ whenNumPad $ withX11Front $ \f ->
      mapM_ (grabExp f) (enumFromTo minBound maxBound :: [NumPad.NumPadUnlockedInput] )
  describe "X11Front - NumPadLockedInput" $ do
    it "should grab/ungrab keys" $ whenNumPad $ withX11Front $ \f ->
      mapM_ (grabExp f) (enumFromTo minBound maxBound :: [NumPad.NumPadLockedInput] )
  describe "X11Front" $ do
    it "should NOT throw exception when it tries to double-grab in the same process" $ withX11Front $ \f1 ->
      withX11Front $ \f2 -> do
        setGrab f1 NumPad.NumLeft `shouldReturn` ()
        setGrab f2 NumPad.NumLeft `shouldReturn` ()
      


