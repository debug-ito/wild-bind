{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Monoid (mempty)

import WildBind (wildBind', def, Binding, binds, on)
import WildBind.Input.NumPad (NumPadUnlockedInput(..))
import WildBind.X11 (withFrontEnd, ActiveWindow)
import WildBind.Indicator
  ( Indicator, withNumPadIndicator, wildBindWithIndicator, togglePresence,
    destroy
  )

main :: IO ()
main = withNumPadIndicator $ \ind -> withFrontEnd (wildBindWithIndicator ind $ binding ind)

binding :: Indicator ActiveWindow NumPadUnlockedInput -> Binding ActiveWindow NumPadUnlockedInput
binding ind = binds [
  on NumEnter "Toggle indicator" $ togglePresence ind,
  on NumMinus "Destroy" $ destroy ind
  ]
