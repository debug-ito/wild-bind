module Main (main) where

import Control.Applicative ((<$>))
import System.IO (withFile, IOMode(ReadMode), hGetContents)
import Test.Hspec

main :: IO ()
main = withREADME $ \readme_doc -> hspec $ sequence_ $ map specFor $ makeTestCases $ extractExamples readme_doc

specFor :: TestCase -> Spec
specFor tc = describe label $ do
  it "should compile OK" $ checkCompile tc
  where
    label = "example " ++ (show $ tcIndex tc)

withREADME :: (String -> IO a) -> IO a
withREADME cont = withFile "../README.md" ReadMode $ \h -> cont =<< hGetContents h

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
