{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Monoid (mempty)

import WildBind (Binding, binds, on, run, as)
import WildBind.Input.NumPad (NumPadUnlockedInput(..))
import WildBind.X11 (withFrontEnd, ActiveWindow)
import WildBind.Indicator
  ( Indicator, withNumPadIndicator, wildBindWithIndicator, togglePresence,
    quit
  )

main :: IO ()
main = withNumPadIndicator $ \ind -> withFrontEnd (wildBindWithIndicator ind $ binding ind)

binding :: Indicator ActiveWindow NumPadUnlockedInput -> Binding ActiveWindow NumPadUnlockedInput
binding ind = binds $ do
  on NumEnter `as` "Toggle indicator" `run` togglePresence ind
  on NumMinus `as` "Quit" `run` quit ind
  on NumHome `as` "BOOM!" `run` fail "exception thrown"
