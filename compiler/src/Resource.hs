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
  , GalleryItem(..), GalleryItemProps(..), Resolution(..), Resource(..), Thumbnail(..)
  , buildGalleryTree, galleryCleanupResourceDir
  ) where


import Control.Concurrent.ParallelIO.Global (parallel)
import Data.List ((\\), sortBy)
import Data.Ord (comparing)
import Data.Char (toLower)
import Data.Maybe (mapMaybe, fromMaybe, maybeToList)
import Data.Function ((&))
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Time.Format (formatTime, defaultTimeLocale)
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


data Resource = Resource
  { resourcePath :: Path
  , modTime :: UTCTime
  } deriving (Generic, Show)

instance ToJSON Resource where
  toJSON Resource{resourcePath, modTime} =
    JSON.String $ pack (webPath resourcePath ++ "?" ++ timestamp)
    where
      timestamp = formatTime defaultTimeLocale "%s" modTime


data GalleryItemProps =
    Directory { items :: [GalleryItem] }
  | Picture { resource :: Resource }
  | Other { resource :: Resource }
  deriving (Generic, Show)

instance ToJSON GalleryItemProps where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


data Thumbnail = Thumbnail
  { resource :: Resource
  , resolution :: Resolution
  } deriving (Generic, Show)

instance ToJSON Thumbnail where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


data GalleryItem = GalleryItem
  { title :: String
  , datetime :: ZonedTime
  , description :: String
  , tags :: [Tag]
  , path :: Path
  , thumbnail :: Maybe Thumbnail
  , properties :: GalleryItemProps
  } deriving (Generic, Show)

instance ToJSON GalleryItem where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


type ItemProcessor = Path -> IO GalleryItemProps
type ThumbnailProcessor = Path -> IO (Maybe Thumbnail)


buildGalleryTree ::
     ItemProcessor -> ThumbnailProcessor
  -> Int -> String -> InputTree -> IO GalleryItem
buildGalleryTree processItem processThumbnail tagsFromDirectories galleryName inputTree =
  mkGalleryItem [] inputTree
  where
    mkGalleryItem :: [String] -> InputTree -> IO GalleryItem
    mkGalleryItem parentTitles InputFile{path, modTime, sidecar} =
      do
        properties <- processItem path
        processedThumbnail <- processThumbnail path
        return GalleryItem
          { title = fromMeta title $ fromMaybe "" $ fileName path
          , datetime = fromMaybe (toZonedTime modTime) (Input.datetime sidecar)
          , description = fromMeta description ""
          , tags = unique ((fromMeta tags []) ++ implicitParentTags parentTitles)
          , path = "/" /> path
          , thumbnail = processedThumbnail
          , properties = properties }

      where
        fromMeta :: (Sidecar -> Maybe a) -> a -> a
        fromMeta get fallback = fromMaybe fallback $ get sidecar

    mkGalleryItem parentTitles InputDir{path, modTime, dirThumbnailPath, items} =
      do
        processedThumbnail <- maybeThumbnail dirThumbnailPath
        processedItems <- parallel $ map (mkGalleryItem subItemsParents) items
        return GalleryItem
          { title = fromMaybe galleryName (fileName path)
          , datetime = fromMaybe (toZonedTime modTime) (mostRecentModTime processedItems)
          , description = ""
          , tags = unique (aggregateTags processedItems ++ implicitParentTags parentTitles)
          , path = "/" /> path
          , thumbnail = processedThumbnail
          , properties = Directory processedItems }

      where
        subItemsParents :: [String]
        subItemsParents = (maybeToList $ fileName path) ++ parentTitles

    maybeThumbnail :: Maybe Path -> IO (Maybe Thumbnail)
    maybeThumbnail Nothing = return Nothing
    maybeThumbnail (Just thumbnailPath) = processThumbnail thumbnailPath

    mostRecentModTime :: [GalleryItem] -> Maybe ZonedTime
    mostRecentModTime =
      maximumByMay comparingTime . map (datetime::(GalleryItem -> ZonedTime))

    comparingTime :: ZonedTime -> ZonedTime -> Ordering
    comparingTime l r = compare (zonedTimeToUTC l) (zonedTimeToUTC r)

    aggregateTags :: [GalleryItem] -> [Tag]
    aggregateTags = concatMap (\item -> tags (item::GalleryItem))

    unique :: Ord a => [a] -> [a]
    unique = Set.toList . Set.fromList

    implicitParentTags :: [String] -> [Tag]
    implicitParentTags = take tagsFromDirectories

    toZonedTime :: UTCTime -> ZonedTime
    toZonedTime = utcToZonedTime utc


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
      resPaths items ++ thumbnailPaths items
      & concatMap subPaths

    resPaths :: [GalleryItem] -> [Path]
    resPaths = mapMaybe (resPath . properties)

    resPath :: GalleryItemProps -> Maybe Path
    resPath Directory{} = Nothing
    resPath resourceProps =
        Just
      $ resourcePath
      $ (resource :: (GalleryItemProps -> Resource)) resourceProps

    thumbnailPaths :: [GalleryItem] -> [Path]
    thumbnailPaths =
        map resourcePath
      . map (resource :: (Thumbnail -> Resource))
      . mapMaybe thumbnail


galleryCleanupResourceDir :: GalleryItem -> FileName -> IO ()
galleryCleanupResourceDir resourceTree outputDir =
  readDirectory outputDir
  >>= return . galleryOutputDiff resourceTree . root
  >>= return . sortBy (flip $ comparing pathLength) -- nested files before dirs
  >>= return . map (localPath . (/>) outputDir)
  >>= mapM_ remove
