{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Prelude hiding (FilePath, readFile)

import Types
import Html (goldenVsHtml)

import Test.Tasty
import Test.Tasty.Golden.Manage as Gold (defaultMain)
import Test.Tasty.Golden.FilePath
import Filesystem.Path.CurrentOS
import Filesystem
import System.Directory as Dir (getTemporaryDirectory)

import Control.Applicative
import Control.Exception (catch)
import Control.Monad ((<=<))
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)

main = join $ Gold.defaultMain <$> execUsualGoldTests

-- | "The usual"
--
-- Where the name of the test file is the name of the test, and the name of
-- the golden file is <test>.gold.html
usualGoldTest :: IO TmpPath -> InputPath -> TestTree
usualGoldTest tmp input@(InputPath inp) =
    goldenVsHtml testName tmp input (GoldPath $ inp <.> "gold.html")

    where
    testName = encodeString (filename inp)


-- | Gather all usual-golden test files.
findUsualGoldInputs :: FilePath -> IO [InputPath]
findUsualGoldInputs f = (map InputPath) <$> findByExtension [".hs"] f

usualGoldTests :: IO TmpPath -> [InputPath] -> [TestTree]
usualGoldTests tmp = map (usualGoldTest tmp)

execUsualGoldTests :: IO TestTree
execUsualGoldTests = do
    ins <- findUsualGoldInputs "test/golden"
    return $ withResource getTmpPath rmTmpPath
        (testGroup "Usual Golden" . flip usualGoldTests ins)

getTmpPath :: IO TmpPath
getTmpPath = do
    dir <- decodeString <$> getTemporaryDirectory
    TmpPath <$> tempDir dir "vim-syntax-test-XXXXX"

rmTmpPath :: TmpPath -> IO ()
rmTmpPath (TmpPath path) = removeTree path

-- FIXME
tempDir :: FilePath -> Text -> IO FilePath
tempDir parentDir template = do
    let retVal = parentDir </> (fromText template)
    createDirectory False retVal
    return retVal
