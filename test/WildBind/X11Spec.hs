{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module WildBind.X11Spec (main, spec) where

import System.IO (hPutStrLn,stderr)
import System.Environment (lookupEnv)
import Control.Exception (finally)
import Control.Applicative ((<$>))
import Test.Hspec

import Data.Time.Clock (getCurrentTime, diffUTCTime)

import WildBind (setGrab,unsetGrab,nextEvent,FrontEvent(FEChange,FEInput),FrontInputDevice,FrontEventSource)
import qualified WildBind.NumPad as NumPad
import WildBind.X11 (withX11Front,ActiveWindow)

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
           . (Bounded i, Enum i, Show i, Eq i, FrontInputDevice f i, FrontEventSource f ActiveWindow i)
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

-- The second arg is just for specifying the type.
grabCase :: forall f i . (Bounded i, Enum i, Show i, Eq i, FrontInputDevice f i, FrontEventSource f ActiveWindow i) => f -> i -> Expectation
grabCase front _ = do
  _ <- nextEvent front :: IO (FrontEvent ActiveWindow i) -- discard the first FEChange event.
  mapM_ (grabExp front) (enumFromTo minBound maxBound :: [i])

stopWatchMsec :: IO a -> IO (a, Int)
stopWatchMsec act = do
  start <- getCurrentTime
  ret <- act
  end <- getCurrentTime
  return (ret, floor ((diffUTCTime end start) * 1000))

spec :: Spec
spec = do
  describe "X11Front" $ do
    it "should first emit FEChange event when initialized" $ withX11Front $ \f -> do
      p "try to get the first event..."
      (ev, time) <- stopWatchMsec $ nextEvent f :: IO (FrontEvent ActiveWindow NumPad.NumPadUnlockedInput, Int)
      time `shouldSatisfy` (< 100)
      case ev of
        FEChange _ -> return ()
        _ -> expectationFailure ("FEChange is expected, but got " ++ show ev)
    it "should NOT throw exception when it tries to double-grab in the same process" $ withX11Front $ \f1 ->
      withX11Front $ \f2 -> do
        setGrab f1 NumPad.NumLeft `shouldReturn` ()
        setGrab f2 NumPad.NumLeft `shouldReturn` ()
  describe "X11Front - NumPadUnlockedInput" $ do
    it "should grab/ungrab keys" $ whenNumPad $ withX11Front $ \f -> do
      grabCase f NumPad.NumCenter
  describe "X11Front - NumPadLockedInput" $ do
    it "should grab/ungrab keys" $ whenNumPad $ withX11Front $ \f -> do
      grabCase f NumPad.NumL5
      


