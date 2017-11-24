{-# LANGUAGE FlexibleContexts, CPP #-}
module WildBind.X11Spec (main, spec) where

import Control.Applicative ((<$>))
import Control.Exception (finally)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hPutStrLn,stderr)
import Test.Hspec

import WildBind
  ( FrontEnd(frontSetGrab, frontUnsetGrab, frontNextEvent),
    FrontEvent(FEChange,FEInput), Describable
  )
import qualified WildBind.Input.NumPad as NumPad
import WildBind.X11 (withFrontEnd, ActiveWindow, XKeyInput)

import WildBind.X11.TestUtil (checkIfX11Available)

main :: IO ()
main = hspec spec

maybeRun :: Expectation -> Expectation
#ifdef TEST_INTERACTIVE
maybeRun = id
#else
maybeRun _ = pendingWith ("You need to set test-interactive flag to run the test.")
#endif


p :: String -> IO ()
p = hPutStrLn stderr . ("--- " ++)

withFrontEndForTest :: (XKeyInput i, Describable i) => (FrontEnd ActiveWindow i -> IO a) -> IO a
withFrontEndForTest action = withFrontEnd $ \front -> do
  _ <- frontNextEvent front -- discard the first FEChange event.
  action front

grabExp :: (Bounded i, Enum i, Show i, Eq i)
        => FrontEnd ActiveWindow i -> i -> Expectation
grabExp front grab_input = grabExpMain `finally` releaseAll where
  grabExpMain = do
    frontSetGrab front grab_input
    p ("Press some numpad keys (grab="++ show grab_input ++")..")
    ev <- frontNextEvent front
    p ("Got event: " ++ show ev)
    case ev of
      FEChange _ -> expectationFailure "FEChange is caught. not expected"
      FEInput got -> do
        got `shouldBe` grab_input
  releaseAll = mapM_ (frontUnsetGrab front) (enumFromTo minBound maxBound)

grabCase :: (Bounded i, Enum i, Show i, Eq i) => FrontEnd ActiveWindow i -> Expectation
grabCase front = mapM_ (grabExp front) (enumFromTo minBound maxBound)

stopWatchMsec :: IO a -> IO (a, Int)
stopWatchMsec act = do
  start <- getCurrentTime
  ret <- act
  end <- getCurrentTime
  return (ret, floor ((diffUTCTime end start) * 1000))

spec :: Spec
spec = checkIfX11Available $ do
  describe "X11Front" $ do
    it "should first emit FEChange event when initialized" $ withFrontEnd $ \f -> do
      p "try to get the first event..."
      (ev, time) <- stopWatchMsec $ frontNextEvent f :: IO (FrontEvent ActiveWindow NumPad.NumPadUnlocked, Int)
      time `shouldSatisfy` (< 500)
      case ev of
        FEChange _ -> return ()
        _ -> expectationFailure ("FEChange is expected, but got " ++ show ev)
    it "should NOT throw exception when it tries to double-grab in the same process" $ withFrontEnd $ \f1 ->
      withFrontEnd $ \f2 -> do
        frontSetGrab f1 NumPad.NumLeft `shouldReturn` ()
        frontSetGrab f2 NumPad.NumLeft `shouldReturn` ()
  describe "X11Front - NumPadUnlocked" $ do
    it "should grab/ungrab keys" $ maybeRun $ withFrontEndForTest $ \f -> do
      let grabCase' :: FrontEnd ActiveWindow NumPad.NumPadUnlocked -> Expectation
          grabCase' = grabCase
      grabCase' f
  describe "X11Front - NumPadLocked" $ do
    it "should grab/ungrab keys" $ maybeRun $ withFrontEndForTest $ \f -> do
      let grabCase' :: FrontEnd ActiveWindow NumPad.NumPadLocked -> Expectation
          grabCase' = grabCase
      grabCase' f
