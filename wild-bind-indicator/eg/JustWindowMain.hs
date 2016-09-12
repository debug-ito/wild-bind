{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Monoid (mempty)

import Control.Exception (throw)
import System.Environment (getArgs)
import WildBind (Binding, binds, on, run, as, wildBind', defOption, optBindingHook, optCatch)
import WildBind.Input.NumPad (NumPadUnlocked(..))
import WildBind.X11 (withFrontEnd, ActiveWindow)
import WildBind.Indicator
  ( Indicator, withNumPadIndicator, wildBindWithIndicator, togglePresence, quit,
    bindingHook
  )

main :: IO ()
main = do
  arg <- fmap headOrEmpty $ getArgs
  withNumPadIndicator $ selectMain arg
  where
    headOrEmpty (h:_) = h
    headOrEmpty _ = ""
    selectMain arg | arg == "error" = mainError
                   | otherwise = mainDefault

mainDefault :: Indicator ActiveWindow NumPadUnlocked -> IO ()
mainDefault ind = do
  putStrLn "mainDefault"
  withFrontEnd (wildBindWithIndicator ind $ binding ind)

mainError :: Indicator ActiveWindow NumPadUnlocked -> IO ()
mainError ind = do
  putStrLn "mainError"
  withFrontEnd $ \front -> wildBind' (my_option front) (binding ind) front
  where
    my_option front = defOption { optBindingHook = bindingHook ind front,
                                  optCatch = rethrower
                                }
    rethrower _ _ e = throw e

binding :: Indicator ActiveWindow NumPadUnlocked -> Binding ActiveWindow NumPadUnlocked
binding ind = binds $ do
  on NumEnter `as` "Toggle description" `run` togglePresence ind
  on NumMinus `as` "Quit" `run` quit ind
  on NumHome `as` "BOOM!" `run` fail "exception thrown"
