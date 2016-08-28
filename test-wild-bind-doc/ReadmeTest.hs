module Main (main) where

import Control.Applicative ((<$>))
import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.List (isPrefixOf, reverse)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (withFile, IOMode(ReadMode), hGetContents, FilePath, writeFile)
import System.Process (waitForProcess, spawnCommand)
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
checkCompile tc = withTempSourceFile tempSourceBase sourceContent doCheck where
  tempSourceBase = "temp_readme_test"
  sourceContent = tcPrefix tc ++ tcBody tc
  doCheck = (waitForProcess =<< spawnCommand cmdline) `shouldReturn` ExitSuccess
  cmdline = "stack ghc " ++ tempSourceBase ++ ".hs"
  

withTempSourceFile :: FilePath -> String -> IO a -> IO a
withTempSourceFile filebase content act = (createFile >> act) `finally` cleanFiles where
  createFile = writeFile (filebase ++ ".hs") content
  cleanFiles = forM_ [".hs", ".o", ".hi", ""] $ \ext -> cleanFile (filebase ++ ext)
  cleanFile filename = bool (return ()) (removeFile filename) =<< doesFileExist filename

