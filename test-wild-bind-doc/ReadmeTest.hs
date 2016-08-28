module Main (main) where

import Control.Applicative ((<$>))
import Control.Exception (finally)
import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.List (isPrefixOf, reverse)
import System.Directory (doesFileExist, removeFile)
import System.IO (withFile, IOMode(ReadMode), hGetContents, FilePath, writeFile)
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

data CodeAcc = CodeAcc { caCurrent :: Maybe CodeBlock,
                         caResult :: [CodeBlock]
                       } deriving (Show,Eq,Ord)

extractExamples :: String -> [CodeBlock]
extractExamples = obtainResult . foldl' f start . lines where
  start = CodeAcc Nothing []
  f acc line = case caCurrent acc of
    Nothing | line == "```haskell" -> acc { caCurrent = Just "" }
            | otherwise -> acc
    Just cur | line == "```" -> finish cur acc
             | "#!" `isPrefixOf` line -> acc
             | otherwise -> acc { caCurrent = Just $ (cur ++ line ++ "\n") }
  finish cur acc = CodeAcc { caCurrent = Nothing,
                             caResult = cur : caResult acc
                           }
  obtainResult CodeAcc { caCurrent = mcur, caResult = ret } = reverse $ maybe ret (\cur -> cur : ret) mcur
  


prefixFor :: Int -> CodeBlock
prefixFor _ = "" -- TODO

makeTestCases :: [CodeBlock] -> [TestCase]
makeTestCases = map f . zip [0 ..] where
  f (i, cb) = TestCase { tcIndex = i,
                         tcPrefix = prefixFor i,
                         tcBody = cb
                       }


checkCompile :: TestCase -> Expectation
checkCompile tc = impl where
  impl = withTempFile tempSource sourceContent doCheck
  sourceContent = tcPrefix tc ++ tcBody tc
  tempSource = "temp_readme_test.hs"
  doCheck = sourceContent `shouldBe` ""  -- TODO.
  

withTempFile :: FilePath -> String -> IO a -> IO a
withTempFile filename content act = (createFile >> act) `finally` cleanFile where
  createFile = writeFile filename content
  cleanFile = bool (return ()) (removeFile filename) =<< doesFileExist filename

