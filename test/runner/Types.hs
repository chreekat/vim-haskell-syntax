module Types where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS
import Data.ByteString.Lazy (ByteString)

newtype HtmlPath = HtmlPath FilePath
    deriving (Show)
newtype InputPath = InputPath FilePath
    deriving (Show)
newtype TmpPath = TmpPath FilePath
    deriving (Show)
newtype GoldPath = GoldPath FilePath
    deriving (Show)

newtype HtmlString = HtmlString { fromHtml :: ByteString }
