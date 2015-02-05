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
goldenTest inp = goldenVsFile inp (inp <.> ".gold.html") (inp <.> ".html") (genHtml inp)

goldenTests :: IO TestTree
goldenTests = (testGroup "Usual Gold") <$> (map goldenTest) <$> goldenInputs

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

tmp = do
    tmpdir <- Just <$> getTemporaryDirectory
    openTempDirectory tmpdir "vim-haskell-syntax-test-XXXXX"

-- | Generates <input>.html from <input>, using vim.
--
-- Also uses a tmp directory to dump intermediate files.
genHtml :: FilePath -> IO ()
genHtml input = do
    target <- tmp <$/> (takeFileName input)
    copyFile input target
    callProcess "vim"
        [ "-E"
        , "-S", "syntax/haskell.vim"
        , "-u", "test/runner/to-html.vim"
        , "--cmd", "view " ++ target
        ]
    void $ runX $ stripTitle (target <.> ".html") (input <.> ".html")
    removeDirectoryRecursive (takeDirectory target)

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
writeX out = writeDocument [withOutputHTML, withIndent True] out
