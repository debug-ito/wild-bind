module WildBind.X11.Internal.KeySpec (spec) where

import Test.Hspec
import Test.QuickCheck (Arbitrary(arbitrary), arbitraryBoundedEnum, property)

import WildBind.Input.NumPad (NumPadUnlocked, NumPadLocked)
import WildBind.X11.Internal.Key (KeySymLike(fromKeySym,toKeySym))

newtype NumPadUnlocked' =
  NumPadUnlocked' { unwrapNumPadUnlocked :: NumPadUnlocked }
  deriving Show

instance Arbitrary NumPadUnlocked' where
  arbitrary = fmap NumPadUnlocked' arbitraryBoundedEnum

newtype NumPadLocked' =
  NumPadLocked' { unwrapNumPadLocked :: NumPadLocked }
  deriving Show

instance Arbitrary NumPadLocked' where
  arbitrary = fmap NumPadLocked' arbitraryBoundedEnum

spec :: Spec
spec = do
  spec_keySymLike "NumPadUnlocked" unwrapNumPadUnlocked
  spec_keySymLike "NumPadLocked" unwrapNumPadLocked

spec_keySymLike :: (Arbitrary k', Show k', KeySymLike k, Eq k) => String -> (k' -> k) -> Spec
spec_keySymLike label unwrapper = describe label $ do
  it "is a KeySymLike" $ property $ (prop_keySymLike . unwrapper)

prop_keySymLike :: (KeySymLike k, Eq k) => k -> Bool
prop_keySymLike key = (fromKeySym . toKeySym) key == Just key

