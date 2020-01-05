-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2020  Pacien TRAN-GIRARD
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

module Resource
  ( ItemProcessor, ThumbnailProcessor
  , GalleryItem(..), GalleryItemProps(..), Resolution(..)
  , buildGalleryTree, galleryCleanupResourceDir
  ) where


import Control.Concurrent.ParallelIO.Global (parallel)
import Data.List ((\\), sortBy)
import Data.Ord (comparing)
import Data.Char (toLower)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Function ((&))
import qualified Data.Set as Set
import Data.Time.LocalTime (ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Directory (getModificationTime)
import Safe.Foldable (maximumByMay)

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, genericToJSON, genericToEncoding)
import qualified Data.Aeson as JSON

import Files
import Input (InputTree(..), Sidecar(..))


encodingOptions :: JSON.Options
encodingOptions = JSON.defaultOptions
  { JSON.fieldLabelModifier = map toLower
  , JSON.constructorTagModifier = map toLower
  , JSON.sumEncoding = JSON.defaultTaggedObject
      { JSON.tagFieldName = "type"
      , JSON.contentsFieldName = "contents"
      }
  }


type Tag = String

data Resolution = Resolution
  { width :: Int
  , height :: Int
  } deriving (Generic, Show, FromJSON)

instance ToJSON Resolution where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


data GalleryItemProps =
    Directory { items :: [GalleryItem] }
  | Picture { resource :: Path }
  | Other { resource :: Path }
  deriving (Generic, Show)

instance ToJSON GalleryItemProps where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


data GalleryItem = GalleryItem
  { title :: String
  , date :: ZonedTime
  , description :: String
  , tags :: [Tag]
  , path :: Path
  , thumbnail :: Maybe Path
  , properties :: GalleryItemProps
  } deriving (Generic, Show)

instance ToJSON GalleryItem where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


type ItemProcessor = Path -> IO GalleryItemProps
type ThumbnailProcessor = Path -> IO (Maybe Path)


buildGalleryTree ::
     ItemProcessor -> ThumbnailProcessor
  -> Int -> String -> InputTree -> IO GalleryItem
buildGalleryTree processItem processThumbnail tagsFromDirectories galleryName inputTree =
  mkGalleryItem (Just galleryName) (Path []) inputTree
  where
    mkGalleryItem :: Maybe String -> Path -> InputTree -> IO GalleryItem
    mkGalleryItem _ parents InputFile{path, sidecar} =
      do
        properties <- processItem path
        processedThumbnail <- processThumbnail path
        fileModTime <- lastModTime path
        return GalleryItem
          { title = itemTitle
          , date = fromMaybe fileModTime itemDate
          , description = optMeta description ""
          , tags = (optMeta tags []) ++ implicitParentTags parents
          , path = parents </ itemTitle
          , thumbnail = processedThumbnail
          , properties = properties }
      where
        itemTitle :: String
        itemTitle = optMeta title $ fromMaybe "" $ fileName path

        itemDate :: Maybe ZonedTime
        itemDate = Input.date sidecar >>= iso8601ParseM

        optMeta :: (Sidecar -> Maybe a) -> a -> a
        optMeta get fallback = fromMaybe fallback $ get sidecar

    mkGalleryItem rootTitle parents InputDir{path, dirThumbnailPath, items} =
      do
        processedThumbnail <- maybeThumbnail dirThumbnailPath
        processedItems <- parallel $ map (mkGalleryItem Nothing itemPath) items
        dirModTime <- lastModTime path
        return GalleryItem
          { title = itemTitle
          , date = fromMaybe dirModTime $ mostRecentChildModTime processedItems
          , description = ""
          , tags = (aggregateChildTags processedItems) ++ implicitParentTags parents
          , path = itemPath
          , thumbnail = processedThumbnail
          , properties = Directory processedItems }
      where
        itemTitle :: String
        itemTitle = flip fromMaybe rootTitle (fromMaybe "" $ fileName path)

        itemPath :: Path
        itemPath = parents </ itemTitle

        maybeThumbnail :: Maybe Path -> IO (Maybe Path)
        maybeThumbnail Nothing = return Nothing
        maybeThumbnail (Just thumbnailPath) = processThumbnail thumbnailPath

        mostRecentChildModTime :: [GalleryItem] -> Maybe ZonedTime
        mostRecentChildModTime =
          maximumByMay comparingDates . map (date::(GalleryItem -> ZonedTime))

        comparingDates :: ZonedTime -> ZonedTime -> Ordering
        comparingDates l r = compare (zonedTimeToUTC l) (zonedTimeToUTC r)

        aggregateChildTags :: [GalleryItem] -> [Tag]
        aggregateChildTags = unique . concatMap (\item -> tags (item::GalleryItem))

        unique :: Ord a => [a] -> [a]
        unique = Set.toList . Set.fromList

    implicitParentTags :: Path -> [Tag]
    implicitParentTags (Path elements) = take tagsFromDirectories elements

    lastModTime :: Path -> IO ZonedTime
    lastModTime path =
      localPath path
      &   getModificationTime
      >>= return . utcToZonedTime utc


flattenGalleryTree :: GalleryItem -> [GalleryItem]
flattenGalleryTree dir@(GalleryItem _ _ _ _ _ _ (Directory items)) =
  dir : concatMap flattenGalleryTree items
flattenGalleryTree simple = [simple]


galleryOutputDiff :: GalleryItem -> FSNode -> [Path]
galleryOutputDiff resources ref =
  (filesystemPaths ref) \\ (compiledPaths $ flattenGalleryTree resources)
  where
    filesystemPaths :: FSNode -> [Path]
    filesystemPaths = map Files.path . tail . flattenDir

    compiledPaths :: [GalleryItem] -> [Path]
    compiledPaths items =
      resourcePaths items ++ thumbnailPaths items
      & concatMap subPaths

    resourcePaths :: [GalleryItem] -> [Path]
    resourcePaths = mapMaybe (resourcePath . properties)

    resourcePath :: GalleryItemProps -> Maybe Path
    resourcePath Directory{} = Nothing
    resourcePath resourceProps = Just $ resource resourceProps

    thumbnailPaths :: [GalleryItem] -> [Path]
    thumbnailPaths = mapMaybe thumbnail


galleryCleanupResourceDir :: GalleryItem -> FileName -> IO ()
galleryCleanupResourceDir resourceTree outputDir =
  readDirectory outputDir
  >>= return . galleryOutputDiff resourceTree . root
  >>= return . sortBy (flip $ comparing pathLength) -- nested files before dirs
  >>= return . map (localPath . (/>) outputDir)
  >>= mapM_ remove
