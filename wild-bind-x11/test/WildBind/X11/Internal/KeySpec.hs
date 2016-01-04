module WildBind.X11.Internal.KeySpec (spec) where

import Test.Hspec
import Test.QuickCheck (Arbitrary(arbitrary), arbitraryBoundedEnum, property)

import WildBind.Input.NumPad (NumPadUnlockedInput,NumPadLockedInput)
import WildBind.X11.Internal.Key (KeySymLike(fromKeySym,toKeySym))

newtype NumPadUnlockedInput' = NumPadUnlockedInput' {
  unwrapNumPadUnlockedInput :: NumPadUnlockedInput
} deriving Show

instance Arbitrary NumPadUnlockedInput' where
  arbitrary = fmap NumPadUnlockedInput' arbitraryBoundedEnum

newtype NumPadLockedInput' = NumPadLockedInput' {
  unwrapNumPadLockedInput :: NumPadLockedInput
} deriving Show

instance Arbitrary NumPadLockedInput' where
  arbitrary = fmap NumPadLockedInput' arbitraryBoundedEnum

spec :: Spec
spec = do
  spec_keySymLike "NumPadUnlockedInput" unwrapNumPadUnlockedInput
  spec_keySymLike "NumPadLockedInput" unwrapNumPadLockedInput

spec_keySymLike :: (Arbitrary k', Show k', KeySymLike k, Eq k) => String -> (k' -> k) -> Spec
spec_keySymLike label unwrapper = describe label $ do
  it "is a KeySymLike" $ property $ (prop_keySymLike . unwrapper)

prop_keySymLike :: (KeySymLike k, Eq k) => k -> Bool
prop_keySymLike key = (fromKeySym . toKeySym) key == Just key

