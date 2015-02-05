import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Manage as Gold (defaultMain)

import Control.Applicative
import Control.Monad

import Data.Foldable

import System.FilePath
import System.Directory
import System.Process

import Text.XML.HXT.Core

main = join (Gold.defaultMain <$> goldenTests)

goldenInputs :: IO [FilePath]
goldenInputs = findByExtension [".hs"] "test/golden"

goldenTest :: FilePath -> TestTree
goldenTest inp = goldenVsFile inp (inp <.> ".gold.html") (inp <.> ".html") (genHtml inp)

goldenTests :: IO TestTree
goldenTests = (testGroup "Usual Gold") <$> (map goldenTest) <$> goldenInputs

tmp = "/tmp/vim-haskell-syntax-test-XXXXXX"

-- | Generates <input>.html from <input>, using vim.
--
-- Also uses a tmp directory to dump intermediate files.
genHtml :: FilePath -> IO ()
genHtml input = do
    let target = tmp </> (takeFileName input)
    copyFile input target
    callProcess "vim"
        [ "-E"
        , "-S", "syntax/haskell.vim"
        , "-u", "test/runner/to-html.vim"
        , "--cmd", "view " ++ target
        ]
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
writeX out = writeDocument [withOutputHTML, withIndent True] out
