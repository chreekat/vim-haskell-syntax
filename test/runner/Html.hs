{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Html where

import Prelude hiding (FilePath)

import Types
import Test.Tasty
import Test.Tasty.Golden.FilePath as Gold
import qualified Data.Text as T
import Data.Text (Text)

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 as BSC (pack, concatMap)
import Text.XML.HXT.Core
import Filesystem
import Filesystem.Path.CurrentOS hiding (concat)
import Shelly hiding ((</>), (<.>))
import qualified Shelly as S

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
