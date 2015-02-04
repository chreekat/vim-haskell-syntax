{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Prelude hiding (FilePath, readFile)

import Test.Tasty
import Test.Tasty.Golden.FilePath as Gold
import Test.Tasty.Golden.Manage as Gold (defaultMain)
import System.Directory as Dir (getTemporaryDirectory)

import Control.Applicative
import Control.Exception (catch)
import Control.Monad ((<=<))
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 as BSC (pack, concatMap)
import Filesystem
import Filesystem.Path.CurrentOS hiding (concat)
import Shelly hiding ((</>), (<.>))
import qualified Shelly as S
import Text.XML.HXT.Core
import qualified Data.Text as T
import Data.Text (Text)

main = join $ Gold.defaultMain <$> execUsualGoldTests

newtype HtmlPath = HtmlPath FilePath
    deriving (Show)
newtype InputPath = InputPath FilePath
    deriving (Show)
newtype TmpPath = TmpPath FilePath
    deriving (Show)
newtype GoldPath = GoldPath FilePath
    deriving (Show)

newtype HtmlString = HtmlString { fromHtml :: ByteString }

-- | Compare vim-generated html to a golden file.
--
-- Needs a TmpPath for its inner machinery, which comes from IO
goldenVsHtml :: TestName
             -> IO TmpPath
             -> InputPath
             -> GoldPath
             -> TestTree
goldenVsHtml name mPath inp (GoldPath gold) =
    goldenVsStringDiff name (\ref new -> ["diff", "-u", ref, new]) gold (fromHtml <$> htmlString mPath inp)

-- | "The usual"
--
-- Where the name of the test file is the name of the test, and the name of
-- the golden file is <test>.gold.html
usualGoldTest :: IO TmpPath -> InputPath -> TestTree
usualGoldTest tmp input@(InputPath inp) =
    goldenVsHtml testName tmp input (GoldPath $ inp <.> "gold.html")

    where
    testName = encodeString (filename inp)

-- | Generate the html with vim.
htmlString :: IO TmpPath -> InputPath -> IO HtmlString
htmlString mPath (InputPath input) = do
    (TmpPath tmp) <- mPath
    let base = filename input
    rawText <- shelly $ do
        cp input tmp
        toHtml (tmp </> base)
        readfile (tmp </> base <.> "html")
    let finalBytes = stripTitle rawText
    return $ HtmlString finalBytes

    where
    stripTitle :: Text -> ByteString
    stripTitle bs =
        let
            stripArrow = arr T.unpack >>> hread >>> processTopDown (ifA (hasName "title") none this)
        in BSC.pack $ concat $ (runLA . xshow) stripArrow bs


-- | Call vim's toHtml on a file
toHtml :: FilePath -> Sh ()
toHtml path = do
    thisDir <- pwd
    let
        path' = toTextIgnore $ filename path
        syntaxFile = toTextIgnore $ thisDir </> "syntax" </> "haskell.vim"
        toHtmlFile = toTextIgnore $ thisDir </> "test" </> "runner" </> "to-html.vim"
    cd $ directory path
    run_ "vim"
        ["-E", "-S", syntaxFile, "-u", toHtmlFile, "--cmd",
         "view " `T.append` path']


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
