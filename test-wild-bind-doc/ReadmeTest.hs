{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.List (isPrefixOf, reverse, stripPrefix, dropWhile)
import Data.Char (isSpace)
import System.Directory (doesFileExist, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (withFile, IOMode(ReadMode), hGetContents, FilePath, writeFile, hPutStrLn, stderr)
import System.Process (waitForProcess, spawnCommand)
import Test.Hspec
import Text.RawString.QQ (r)

main :: IO ()
main = do
  stack_opts <- getStackOpts
  withTestCases "../README.md" $ \readme -> do
    hspec $ specFor stack_opts readme

specFor :: String -> [TestCase] -> Spec
specFor stack_opts tcs = sequence_ $ map singleSpec tcs
  where
    singleSpec tc = describe label $ it "should compile OK" $ checkCompile stack_opts tc
      where
        label = "==== " ++ (tcFileName tc) ++ ": " ++ (show $ tcIndex tc)

withTestCases :: String -> ([TestCase] -> IO a) -> IO a
withTestCases filename cont = withFile filename ReadMode $ \h -> cont =<< loadFile h
  where
    loadFile handle = makeTestCases filename <$> extractExamples <$> hGetContents handle

type CodeBlock = String

data TestCase = TestCase { tcIndex :: Int,
                           tcPrefix :: CodeBlock,
                           tcBody :: CodeBlock,
                           tcFileName :: String
                         } deriving (Show,Eq,Ord)

data CodeAcc = CodeAcc { caCurrent :: Maybe CodeBlock,
                         caResult :: [CodeBlock]
                       } deriving (Show,Eq,Ord)

extractExamples :: String -> [CodeBlock]
extractExamples = obtainResult . foldl' f start . lines where
  start = CodeAcc Nothing []
  f acc line = case caCurrent acc of
    Nothing | line == "```haskell" -> acc { caCurrent = Just "" }
            | otherwise -> case mblock of
              Nothing -> acc
              Just block -> acc { caCurrent = Just block }
    Just cur | line == "```" -> finish cur acc
             | "#!" `isPrefixOf` blockline -> acc
             | otherwise -> acc { caCurrent = Just $ (cur ++ blockline ++ "\n") }
    where
      mblock = stripCodeBlockPrefix line
      blockline = maybe line id mblock
  finish cur acc = CodeAcc { caCurrent = Nothing,
                             caResult = cur : caResult acc
                           }
  obtainResult CodeAcc { caCurrent = mcur, caResult = ret } = reverse $ maybe ret (\cur -> cur : ret) mcur

stripCodeBlockPrefix :: String -> Maybe String
stripCodeBlockPrefix = stripPrefix "-- > " . dropWhile isSpace

prefixFor :: Int -> CodeBlock
prefixFor 2 = prefix_basic_process
prefixFor 3 = prefix_basic_process
prefixFor 4 = prefix_with_pushKey
prefixFor 5 = prefix_with_pushKey ++ [r|
myBinding = forFirefox

|]

prefixFor 6 = prefix_basic_process ++ [r|
myBinding = binds $ do
  on NumCenter `run` putStrLn "Hello, world!"

|]

prefixFor 7 = prefix_basic_process
prefixFor 9 = [r|
{-# LANGUAGE OverloadedStrings #-}
import WildBind.Task.X11

main = wildNumPad myBinding

upAction, downAction, centerAction :: StateT Int IO ()  ------------- (3)
upAction = modify (+ 1)
downAction = modify (subtract 1)
centerAction = do
  current_state <- get
  liftIO $ putStrLn ("Current state is = " ++ show current_state)

myBinding :: Binding ActiveWindow NumPadUnlocked
myBinding = startFrom 0 myBinding'  --------------------------------- (4)

|]

prefixFor _ = ""

prefix_basic_process :: CodeBlock
prefix_basic_process =  [r|
{-# LANGUAGE OverloadedStrings #-}
import WildBind.Task.X11
import System.Process (spawnCommand)

main = wildNumPad myBinding

|]

prefix_with_pushKey :: CodeBlock
prefix_with_pushKey = prefix_basic_process ++ [r|

pushKey key = spawnCommand ("xdotool key " <> key)

|]


makeTestCases :: String -> [CodeBlock] -> [TestCase]
makeTestCases filename blocks = map f $ zip [0 ..] blocks where
  f (i, cb) = TestCase { tcIndex = i,
                         tcPrefix = prefixFor i,
                         tcBody = cb,
                         tcFileName = filename
                       }

getStackOpts :: IO String
getStackOpts = maybe notify_and_default return =<< lookupEnv env_name where
  env_name = "WILDBIND_README_TEST_STACK_OPTS"
  notify_and_default = do
    hPutStrLn stderr ("Environment variable " ++ env_name ++ " is not specified. If you encounter an error, set it to the same options as those you used when you built this program.")
    return ""

checkCompile :: String -> TestCase -> Expectation
checkCompile stack_opts tc = withTempSourceFile tempSourceBase sourceContent doCheck where
  tempSourceBase = "temp_readme_test"
  sourceContent = tcPrefix tc ++ tcBody tc
  doCheck = (waitForProcess =<< spawnCommand cmdline) `shouldReturn` ExitSuccess
  cmdline = "stack ghc " ++ stack_opts ++ " " ++ tempSourceBase ++ ".hs"
  

withTempSourceFile :: FilePath -> String -> IO a -> IO a
withTempSourceFile filebase content act = (createFile >> act) `finally` cleanFiles where
  createFile = writeFile (filebase ++ ".hs") content
  cleanFiles = forM_ [".hs", ".o", ".hi", ""] $ \ext -> cleanFile (filebase ++ ext)
  cleanFile filename = bool (return ()) (removeFile filename) =<< doesFileExist filename

