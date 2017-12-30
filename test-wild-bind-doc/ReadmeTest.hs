{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.List (isPrefixOf, reverse, stripPrefix, span)
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
    withTestCases "../wild-bind-x11/src/WildBind/X11/Emulate.hs" $ \emu -> do
      hspec $ specFor stack_opts (readme ++ emu)

specFor :: String -> [TestCase] -> Spec
specFor stack_opts tcs = sequence_ $ map singleSpec tcs
  where
    singleSpec tc = describe label $ it "should compile OK" $ do
      checkCompile stack_opts tc
      where
        label = "==== " ++ (tcFileName tc) ++ ": " ++ (show $ tcIndex tc)

withTestCases :: String -> ([TestCase] -> IO a) -> IO a
withTestCases filename cont = withFile filename ReadMode $ \h -> cont =<< loadFile h
  where
    loadFile handle = makeTestCases filename <$> extractExamples <$> hGetContents handle

type CodeBlock = String
type Prefix = String

data TestCase = TestCase { tcIndex :: Int,
                           tcPrefix :: CodeBlock,
                           tcBody :: CodeBlock,
                           tcFileName :: String
                         } deriving (Show,Eq,Ord)

data CodeAcc = CodeAcc { caCurrent :: Maybe (CodeBlock, Prefix),
                         caResult :: [CodeBlock]
                       } deriving (Show,Eq,Ord)

extractExamples :: String -> [CodeBlock]
extractExamples = obtainResult . foldl' f start . lines where
  start = CodeAcc Nothing []
  f acc line = case caCurrent acc of
    Nothing | line == "```haskell" -> acc { caCurrent = Just ("", "") }
            | otherwise -> case mblock of
              Nothing -> acc
              Just (block, prefix) -> acc { caCurrent = Just $ (block ++ "\n", prefix) }
    Just (cur, prefix) -> case stripPrefix prefix line of
      Nothing -> finish cur acc
      Just line_body | line_body == "```" -> finish cur acc
                     | "#!" `isPrefixOf` line_body -> acc
                     | otherwise -> acc { caCurrent = Just $ (cur ++ line_body ++ "\n", prefix) }
    where
      mblock = stripSpacedPrefix "-- > " line
  finish cur acc = CodeAcc { caCurrent = Nothing,
                             caResult = cur : caResult acc
                           }
  obtainResult CodeAcc { caCurrent = mcur, caResult = ret } = reverse $ maybe ret (\(cur, _) -> cur : ret) mcur

stripSpacedPrefix :: Prefix -> String -> Maybe (String, Prefix)
stripSpacedPrefix prefix = makeResult . span isSpace
  where
    makeResult (spaces, rest) = do
      body <- stripPrefix prefix rest
      return (body, spaces ++ prefix)

prefixFor :: String -> Int -> CodeBlock
prefixFor "../README.md" index = readme index
  where
    readme 2 = prefix_basic_process
    readme 3 = prefix_basic_process
    readme 4 = prefix_with_pushKey
    readme 5 = prefix_with_pushKey ++ [r|
myBinding = forFirefox

|]

    readme 6 = prefix_basic_process ++ [r|
myBinding = binds $ do
  on NumCenter `run` putStrLn "Hello, world!"

|]

    readme 7 = prefix_basic_process
    readme 9 = [r|
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
    readme _ = ""

prefixFor _ _ = ""

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
                         tcPrefix = prefixFor filename i,
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

