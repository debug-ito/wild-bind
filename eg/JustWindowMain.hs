module Main (main) where

import Control.Monad (forM_)

import WildBind (Describable(describe))
import WildBind.Input.NumPad (NumPadUnlockedInput)
import WildBind.Indicator (Indicator, updateDescription, withNumPadIndicator)

main :: IO ()
main = withNumPadIndicator indicatorMain

indicatorMain :: Indicator s NumPadUnlockedInput -> IO ()
indicatorMain ind = do
  forM_ (enumFromTo minBound maxBound) $ \key -> do
    updateDescription ind key (describe key)
