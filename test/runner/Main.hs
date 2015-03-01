import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Manage as Gold (defaultMain)

import Control.Applicative
import Control.Monad

import Data.Foldable
import Data.List
import Data.Monoid

import System.FilePath
import System.Directory
import System.Process

import Text.XML.HXT.Core

newtype InputPath = InputPath FilePath
newtype TmpPath = TmpPath { fromTmp :: FilePath }

main = join (Gold.defaultMain <$> goldenTests)

goldenInputs :: IO [InputPath]
goldenInputs = map InputPath <$> findByExtension [".hs"] "test/golden"

goldenTest :: IO TmpPath -> InputPath -> TestTree
goldenTest tmp (InputPath inp) = goldenVsFileDiff
    (dropExtension . takeFileName $ inp)
    (\a b -> ["diff", "-u", a, b])
    (inp <.> ".gold.html")
    (inp <.> ".html")
    ((genHtml (InputPath inp) . fromTmp) =<< tmp)

goldenTests :: IO TestTree
goldenTests = do
    is <- goldenInputs
    let ts tmp = testGroup "Usual Gold" (map (goldenTest tmp) is)
    return $ withResource
        mkTemp
        (removeDirectoryRecursive . fromTmp)
        ts

mkTemp = TmpPath <$> do
    tmpdir <- Just <$> getTemporaryDirectory
    openTempDirectory tmpdir "vim-haskell-syntax-test-XXXXX"

-- | Generates <input>.html from <input>, using vim.
--
-- Uses a tmp directory to dump intermediate files.
genHtml :: InputPath -> FilePath -> IO ()
genHtml (InputPath input) tmp = do
    let target = tmp </> takeFileName input
    copyFile input target
    void $ readProcess "vim"
        [ "-Eu", "syntax/haskell.vim"
        , "-S", "test/runner/test-highlight.vim"
        , "--cmd", "set nocp"
        , "+let g:html_no_progress=1"
        , "+set ft=haskell"
        , "+runtime plugin/tohtml.vim"
        , "+TOhtml"
        , "+wqa"
        , target
        ]
        ""
    void $ runX $ stripTitle (target <.> ".html") (input <.> ".html")

stripTitle :: FilePath -> FilePath -> IOSArrow XmlTree XmlTree
stripTitle inp out =
    readX inp
    >>>
    processTopDown (filterA $ neg (hasName "title"))
    >>>
    writeX out

readX :: FilePath -> IOSArrow XmlTree XmlTree
readX = readDocument [withParseHTML True]

writeX :: FilePath -> IOSArrow XmlTree XmlTree
writeX = writeDocument [withOutputHTML]

openTempDirectory :: Maybe FilePath -- ^ optional location
                  -> String -- ^ template
                  -> IO FilePath
openTempDirectory mdir template = do
    nameString <- readProcess "mktemp"
        ([ "-d" ]
            <> maybe [] (\dir -> ["--tmpdir="<>dir]) mdir
            <> [template])
        ""
    return $ chop nameString

    where
    chop = reverse . (!! 1) . tails . reverse
