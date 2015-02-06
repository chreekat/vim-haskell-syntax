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

-- | Minor convenience
(<$/>) :: Functor f => f FilePath -> FilePath -> f FilePath
fa <$/> b = (</> b) <$> fa
infixl 4 <$/>

main = join (Gold.defaultMain <$> goldenTests)

goldenInputs :: IO [FilePath]
goldenInputs = findByExtension [".hs"] "test/golden"

goldenTest :: FilePath -> TestTree
goldenTest inp = goldenVsFileDiff
    (dropExtension . takeFileName $ inp)
    (\a b -> ["diff", "-u", a, b])
    (inp <.> ".gold.html")
    (inp <.> ".html")
    (do
        t <- mkTemp
        genHtml t inp
        -- Ensure a gold file exists, so diff is nice
        callProcess "touch" [inp <.> ".gold.html"]
        removeDirectoryRecursive t)

goldenTests :: IO TestTree
goldenTests = (testGroup "Usual Gold") <$> (map goldenTest) <$> goldenInputs

mkTemp = do
    tmpdir <- Just <$> getTemporaryDirectory
    openTempDirectory tmpdir "vim-haskell-syntax-test-XXXXX"

-- | Generates <input>.html from <input>, using vim.
--
-- Uses a tmp directory to dump intermediate files.
genHtml :: FilePath -> FilePath -> IO ()
genHtml tmp input = do
    let target = tmp </> (takeFileName input)
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
writeX out = writeDocument [withOutputHTML] out

openTempDirectory :: Maybe FilePath -- ^ optional location
                  -> String -- ^ template
                  -> IO FilePath
openTempDirectory mdir template = do
    nameString <- readProcess "mktemp"
        ([ "-d" ]
         <> (maybe [] (\dir -> ["--tmpdir="<>dir]) mdir)
         <> [template])
        ""
    return $ chop nameString

    where
    chop = reverse . (!! 1) . tails . reverse
