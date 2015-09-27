module Main (main) where

import Data.Monoid (mempty)

import WildBind (wildBind', def, Binding)
import WildBind.Input.NumPad (NumPadUnlockedInput)
import WildBind.X11 (withFrontEnd, ActiveWindow)
import WildBind.Indicator (Indicator, withNumPadIndicator, optionFor, wildBindWithIndicator)

main :: IO ()
main = withNumPadIndicator $ \ind -> withFrontEnd (wildBindWithIndicator ind binding)

binding :: Binding ActiveWindow NumPadUnlockedInput
binding = mempty
