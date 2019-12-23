{-# LANGUAGE DuplicateRecordFields, DeriveGeneric #-}


-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019  Pacien TRAN-GIRARD
--               2019  Guillaume FOUET
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


module Lib
  ( testRun
  ) where


import GHC.Generics

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (Exception, throwIO)

import Data.Function
import Data.Maybe (fromMaybe)
import Data.List (map)
import Data.Set (fromList, toList)
import Data.Char (toLower)
import Data.Text (Text, empty, pack)
import Data.Yaml (ParseException, decodeFileEither)
import Data.Aeson

import System.FilePath ((</>), dropFileName, dropExtension, isExtensionOf)
import qualified System.FilePath.Posix (joinPath)
import System.Directory.Tree
import System.Directory


encodingOptions :: Options
encodingOptions = defaultOptions
  { fieldLabelModifier = map toLower
  , constructorTagModifier = map toLower
  , sumEncoding = defaultTaggedObject
      { tagFieldName = "type"
      , contentsFieldName = "contents"
      }
  }


-- input structure

data SidecarItemMetadata = SidecarItemMetadata
  { title :: Maybe Text
  , date :: Maybe Text
  , description :: Maybe Text
  , tags :: Maybe [Text]
  } deriving Generic

instance FromJSON SidecarItemMetadata where
  parseJSON = genericParseJSON encodingOptions


-- output structures

type ResourcePath = Text
type Tag = Text
type FileSizeKB = Int


data Resolution = Resolution
  { width :: Int
  , height :: Int
  } deriving Generic

instance ToJSON Resolution where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


data ItemProperties =
    Directory { items :: [Item] }
  | Image { resolution :: Resolution, filesize :: FileSizeKB }
--  | Video { filesize :: FileSizeKB }
  | Unknown
  deriving Generic

instance ToJSON ItemProperties where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


data Item = Item
  { title :: Text
  , date :: Text -- TODO: checked ISO8601 date
  , description :: Text
  , tags :: [Tag]
  , path :: ResourcePath
  , thumbnail :: Maybe ResourcePath
  , properties :: ItemProperties
  } deriving Generic

instance ToJSON Item where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


-- mapping

data LoadException = LoadException String ParseException deriving Show
instance Exception LoadException

decodeYamlFile :: (MonadIO m, FromJSON a) => FilePath -> m a
decodeYamlFile fpath =
  liftIO $ Data.Yaml.decodeFileEither fpath
  >>= either (throwIO . LoadException fpath) return


metadataDirTree :: DirTree FilePath -> IO (DirTree SidecarItemMetadata)
metadataDirTree (Failed _ ferr) = ioError ferr
metadataDirTree f@(File _ fpath) =
  decodeYamlFile fpath
  >>= \metadata -> return f { file = metadata }
metadataDirTree d@(Dir _ dcontents) =
  filter canContainMetadata dcontents
  & mapM metadataDirTree
  >>= \contents -> return d { contents = contents }
  where
     canContainMetadata (Dir _ _) = True
     canContainMetadata (File fname _) = isExtensionOf ".yaml" fname


unique :: Ord a => [a] -> [a]
unique = Data.Set.toList . Data.Set.fromList


joinURLPath :: [FileName] -> Text
joinURLPath = pack . System.FilePath.Posix.joinPath


toItemTree :: (MonadIO m) => FilePath -> FilePath -> DirTree SidecarItemMetadata -> m (Item, DirTree SidecarItemMetadata)
toItemTree itemsDir thumbnailsDir = nodeToItem []
  where
    nodeToItem pathTo d@(Dir dname dcontents) =
      mapM (nodeToItem path) dcontents
      >>= return . unzip
      >>= \(items, _) -> return
        ( Item
          { title = pack dname
          , date = empty
          , description = empty
          , tags = aggregateTags items
          , path = joinURLPath $ itemsDir:path
          , thumbnail = Nothing
          , properties = Directory { items = items } }
        , d)
      where
        path = pathTo ++ [dname]
        aggregateTags = unique . concatMap (\item -> tags (item::Item))

    nodeToItem pathTo f@(File fname metadata) =
      return
        ( Item
          { title = optMeta title $ pack $ dropExtension fname
          , date = optMeta date empty -- TODO: check and normalise dates
          , description = optMeta description empty
          , tags = optMeta tags []
          , path = joinURLPath $ itemsDir:path
          , thumbnail = Just $ joinURLPath $ thumbnailsDir:path
          , properties = Unknown } -- TODO
        , f)
      where
        path = pathTo ++ [fname]
        optMeta get fallback = fromMaybe fallback $ get (metadata::SidecarItemMetadata)


unrooted :: AnchoredDirTree a -> DirTree a
unrooted t = (dirTree t) { name = "" }


writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path obj =
  createDirectoryIfMissing True (dropFileName path)
  >> Data.Aeson.encodeFile path obj


infixl 1 >>>>>>
(>>>>>>) :: Monad m => m a -> (a -> m b) -> m a
a >>>>>> f = a >>= f >>= return a


process :: FilePath -> FilePath -> IO ()
process inputDir outputDir =
  readDirectoryWith return inputDir
  >>= return . unrooted
  >>= metadataDirTree
  >>= toItemTree itemsDir thumbnailsDir
  >>>>>> writeJSON (outputDir </> indexFile) . fst
  >>= return . show . toEncoding . fst
  >>= liftIO . putStrLn
  where
    itemsDir = "items"
    thumbnailsDir = "thumbnails"
    indexFile = "index.json"


testRun :: IO ()
testRun = process "../example" "../out"
