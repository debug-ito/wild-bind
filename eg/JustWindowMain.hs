module Main (main) where

import Data.Monoid (mempty)

import WildBind (wildBind', def)
import WildBind.Input.NumPad (NumPadUnlockedInput)
import WildBind.X11 (withFrontEnd, ActiveWindow)
import WildBind.Indicator (Indicator, withNumPadIndicator, optionFor)

main :: IO ()
main = withNumPadIndicator indicatorMain

indicatorMain :: Indicator ActiveWindow NumPadUnlockedInput -> IO ()
indicatorMain ind = withFrontEnd $ \f -> wildBind' (optionFor ind f def) mempty f
