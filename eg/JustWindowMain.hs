module Main (main) where

import WildBind.Input.NumPad (NumPadUnlockedInput)
import WildBind.Indicator (Indicator, withNumPadIndicator)

main :: IO ()
main = withNumPadIndicator indicatorMain

indicatorMain :: Indicator s NumPadUnlockedInput -> IO ()
indicatorMain = const $ return ()
