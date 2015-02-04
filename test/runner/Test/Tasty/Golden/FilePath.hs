-- | Wrapper over Tasty.Golden using Text and FilePath from system-io.

module Test.Tasty.Golden.FilePath (findByExtension, goldenVsStringDiff)
where

import Prelude hiding (FilePath)
import qualified Prelude
import Control.Applicative ((<$>))
import Data.ByteString.Lazy (ByteString)
import Filesystem.Path.CurrentOS
import Test.Tasty (TestName, TestTree)
import qualified Test.Tasty.Golden as Base
import Data.Text (Text)
import qualified Data.Text as T

findByExtension :: [Text] -> FilePath -> IO [FilePath]
findByExtension exts topDir =
    let
        exts' = map T.unpack exts
        topDir' = encodeString topDir
    in
        (map decodeString) <$> Base.findByExtension exts' topDir'

goldenVsStringDiff :: TestName -> (Prelude.FilePath -> Prelude.FilePath -> [String]) -> FilePath -> IO ByteString -> TestTree
goldenVsStringDiff name cmd gold resultBytes =
    let gold' = encodeString gold
    in Base.goldenVsStringDiff name cmd gold' resultBytes
