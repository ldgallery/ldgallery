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
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (map)
import Data.Set (fromList, toList)
import Data.Char (toLower)
import Data.Text (Text, empty, pack)
import Data.Yaml (ParseException, decodeFileEither)
import Data.Aeson

import System.FilePath ((</>), joinPath, dropFileName, dropExtension, isExtensionOf)
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
  } deriving (Generic, Show)

instance FromJSON SidecarItemMetadata where
  parseJSON = genericParseJSON encodingOptions


-- output structures

type ResourcePath = Text
type Tag = Text
type FileSizeKB = Int


data Resolution = Resolution
  { width :: Int
  , height :: Int
  } deriving (Generic, Show)

instance ToJSON Resolution where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


data ItemProperties =
    Directory { items :: [Item] }
  | Image { resolution :: Resolution, filesize :: FileSizeKB }
--  | Video { filesize :: FileSizeKB }
  | Unknown
  deriving (Generic, Show)

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
  } deriving (Generic, Show)

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


toMetaTree :: DirTree FilePath -> IO (DirTree SidecarItemMetadata)
toMetaTree tree = return (filterDir canContainMetadata tree) >>= metaNode
  where
    -- TODO: exclude hidden files (name starting with '.')?
    canContainMetadata :: DirTree a -> Bool
    canContainMetadata (File fname _) = isExtensionOf ".yaml" fname
    canContainMetadata (Dir _ _) = True

    metaNode :: DirTree FilePath -> IO (DirTree SidecarItemMetadata)
    metaNode (Failed _ ferr) = ioError ferr
    metaNode file@(File _ fpath) = decodeYamlFile fpath
      >>= \metadata -> return file { file = metadata }
    metaNode dir@(Dir _ dcontents) = mapM metaNode dcontents
      >>= \contents -> return dir { contents = contents }


unique :: Ord a => [a] -> [a]
unique = Data.Set.toList . Data.Set.fromList

joinURLPath :: [FileName] -> Text
joinURLPath = pack . System.FilePath.Posix.joinPath


toItemTree :: FilePath -> FilePath -> DirTree SidecarItemMetadata -> IO Item
toItemTree itemsDir thumbnailsDir = itemNode []
  where
    itemNode :: [FileName] -> DirTree SidecarItemMetadata -> IO Item
    itemNode pathTo (Dir dname dcontents) =
      mapM (itemNode path) dcontents
      >>= \items -> return Item
        { title = pack dname
        , date = empty
        , description = empty
        , tags = aggregateChildTags items
        , path = joinURLPath $ itemsDir:path
        , thumbnail = Nothing
        , properties = Directory items }
      where
        path = pathTo ++ [dname]
        aggregateChildTags = unique . concatMap (\item -> tags (item::Item))

    itemNode pathTo (File fname metadata) =
      return Item
        { title = optMeta title $ pack name
        , date = optMeta date empty -- TODO: check and normalise dates
        , description = optMeta description empty
        , tags = optMeta tags []
        , path = joinURLPath $ itemsDir:path
        , thumbnail = Just $ joinURLPath $ thumbnailsDir:path
        , properties = Unknown } -- TODO
      where
        name = dropExtension fname
        path = pathTo ++ [name]
        optMeta get fallback = fromMaybe fallback $ get (metadata::SidecarItemMetadata)


data ObjectTree = ObjectTree
  { pathTo :: [ObjectTree]
  , meta :: (DirTree SidecarItemMetadata)
  , item :: Item } deriving Show

rootObjectTree :: DirTree SidecarItemMetadata -> Item -> ObjectTree
rootObjectTree = ObjectTree []

toObjectTree :: (DirTree SidecarItemMetadata -> IO Item) -> DirTree SidecarItemMetadata -> IO ObjectTree
toObjectTree itemGen meta = itemGen meta >>= return . (rootObjectTree meta)

flatten :: ObjectTree -> [ObjectTree]
flatten object@(ObjectTree _ (File _ _) _) = [object]
flatten object@(ObjectTree pathTo (Dir _ dcontents) item) =
  zip dcontents (items $ properties item)
  & map (uncurry $ ObjectTree $ pathTo ++ [object])
  & concatMap flatten
  & (:) object

objFileName :: ObjectTree -> FileName
objFileName (ObjectTree _ (Dir name _) _) = name
objFileName (ObjectTree _ (File name _) _) = dropExtension name -- without ".yaml"

objFilePath :: ObjectTree -> FilePath
objFilePath obj@(ObjectTree pathTo _ _) =
  (map (name . meta) pathTo) ++ [objFileName obj]
  & System.FilePath.joinPath


data FileTransform = FileTransform
  { src :: FilePath
  , dst :: FilePath } deriving Show


isUpToDate :: FilePath -> FilePath -> IO Bool
isUpToDate ref target =
  do
    refTime <- getModificationTime ref
    targetTime <- getModificationTime target
    return (target >= ref)


unrooted :: AnchoredDirTree a -> DirTree a
unrooted t = (dirTree t) { name = "" }

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path obj =
  createDirectoryIfMissing True (dropFileName path)
  >> Data.Aeson.encodeFile path obj

passthrough :: Monad m => (a -> m b) -> a -> m a
passthrough f a = return a >>= f >>= \_ -> return a

process :: FilePath -> FilePath -> IO ()
process inputDir outputDir =
  readDirectoryWith return inputDir
  >>= return . unrooted
  >>= toMetaTree
  >>= toObjectTree (toItemTree itemsDir thumbnailsDir)
  >>= passthrough (writeJSON (outputDir </> indexFile) . item)
  >>= return . flatten
--  >>= mapM (return . pathTo)
  >>= return . (map objFilePath)
  >>= return . show
--  >>= return . show . toEncoding . item
  >>= liftIO . putStrLn
  where
    itemsDir = "items"
    thumbnailsDir = "thumbnails"
    indexFile = "index.json"


testRun :: IO ()
testRun = process "../example" "../out"
