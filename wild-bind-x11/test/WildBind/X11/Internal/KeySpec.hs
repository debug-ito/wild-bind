module WildBind.X11.Internal.KeySpec (spec) where

import Test.Hspec
import Test.QuickCheck (Arbitrary(arbitrary), arbitraryBoundedEnum, property)

import WildBind.NumPad (NumPadUnlockedInput)
import WildBind.X11.Internal.Key (KeySymLike(fromKeySym,toKeySym))

instance Arbitrary NumPadUnlockedInput where
  arbitrary = arbitraryBoundedEnum

spec :: Spec
spec = do
  describe "NumPadUnlockedInput" $ do
    it "is a KeySymLike" $ property $ (prop_keySymLike :: NumPadUnlockedInput -> Bool)

prop_keySymLike :: (KeySymLike k, Eq k) => k -> Bool
prop_keySymLike key = (fromKeySym . toKeySym) key == Just key

