module Main (main) where

import Control.Applicative ((<$>))
import Test.Hspec

main :: IO ()
main = do
  specs <- map specFor <$> makeTestCases <$> extractExamples <$> loadREADME
  hspec $ sequence_ specs

specFor :: TestCase -> Spec
specFor tc = describe label $ do
  it "should compile OK" $ checkCompile tc
  where
    label = "example " ++ (show $ tcIndex tc)

loadREADME :: IO String
loadREADME = undefined


type CodeBlock = String

data TestCase = TestCase { tcIndex :: Int,
                           tcPrefix :: CodeBlock,
                           tcBody :: CodeBlock
                         } deriving (Show,Eq,Ord)

extractExamples :: String -> [CodeBlock]
extractExamples = undefined

prefixFor :: Int -> CodeBlock
prefixFor = undefined

makeTestCases :: [CodeBlock] -> [TestCase]
makeTestCases = map f . zip [0 ..] where
  f (i, cb) = TestCase { tcIndex = i,
                         tcPrefix = prefixFor i,
                         tcBody = cb
                       }


checkCompile :: TestCase -> Expectation
checkCompile = undefined
