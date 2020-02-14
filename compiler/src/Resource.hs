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
import Data.List (sortOn)
import Data.List.Ordered (minusBy)
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
  -> Int -> InputTree -> IO GalleryItem
buildGalleryTree processItem processThumbnail tagsFromDirectories inputTree =
  mkGalleryItem [] [] inputTree
  where
    mkGalleryItem :: [String] -> [Tag] -> InputTree -> IO GalleryItem
    mkGalleryItem parentDirs inheritedTags InputFile{path, modTime, sidecar} =
      do
        properties <- processItem path
        processedThumbnail <- processThumbnail path
        return GalleryItem
          { title = Input.title sidecar ?? fileName path ?? ""
          , datetime = Input.datetime sidecar ?? toZonedTime modTime
          , description = Input.description sidecar ?? ""
          , tags = unique ((Input.tags sidecar ?? []) ++ inheritedTags ++ parentDirTags parentDirs)
          , path = "/" /> path
          , thumbnail = processedThumbnail
          , properties = properties }

    mkGalleryItem parentDirs inheritedTags InputDir{path, modTime, sidecar, dirThumbnailPath, items} =
      do
        let itemsParents = (maybeToList $ fileName path) ++ parentDirs
        let dirTags = (Input.tags sidecar ?? []) ++ inheritedTags
        processedItems <- parallel $ map (mkGalleryItem itemsParents dirTags) items
        processedThumbnail <- maybeThumbnail dirThumbnailPath
        return GalleryItem
          { title = Input.title sidecar ?? fileName path ?? ""
          , datetime = Input.datetime sidecar ?? mostRecentModTime processedItems
                                              ?? toZonedTime modTime
          , description = Input.description sidecar ?? ""
          , tags = unique (aggregateTags processedItems ++ parentDirTags parentDirs)
          , path = "/" /> path
          , thumbnail = processedThumbnail
          , properties = Directory processedItems }

    infixr ??
    (??) :: Maybe a -> a -> a
    (??) = flip fromMaybe

    unique :: Ord a => [a] -> [a]
    unique = Set.toList . Set.fromList

    parentDirTags :: [String] -> [Tag]
    parentDirTags = take tagsFromDirectories

    aggregateTags :: [GalleryItem] -> [Tag]
    aggregateTags = concatMap (\item -> tags (item::GalleryItem))

    maybeThumbnail :: Maybe Path -> IO (Maybe Thumbnail)
    maybeThumbnail Nothing = return Nothing
    maybeThumbnail (Just thumbnailPath) = processThumbnail thumbnailPath

    mostRecentModTime :: [GalleryItem] -> Maybe ZonedTime
    mostRecentModTime =
      maximumByMay comparingTime . map (datetime::(GalleryItem -> ZonedTime))

    comparingTime :: ZonedTime -> ZonedTime -> Ordering
    comparingTime l r = compare (zonedTimeToUTC l) (zonedTimeToUTC r)

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

    (\\) :: [Path] -> [Path] -> [Path]
    a \\ b = minusOn orderedForm (sortOn orderedForm a) (sortOn orderedForm b)
      where
        orderedForm :: Path -> WebPath
        orderedForm = webPath

        minusOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
        minusOn f l r = map snd $ minusBy comparingFst (packRef f l) (packRef f r)

        packRef :: (a -> b) -> [a] -> [(b, a)]
        packRef f = map (\x -> let y = f x in y `seq` (y, x))

        comparingFst :: Ord b => (b, a) -> (b, a) -> Ordering
        comparingFst (l, _) (r, _) = compare l r


galleryCleanupResourceDir :: GalleryItem -> FileName -> IO ()
galleryCleanupResourceDir resourceTree outputDir =
  readDirectory outputDir
  >>= return . galleryOutputDiff resourceTree . root
  >>= return . sortOn ((0 -) . pathLength) -- nested files before their parent dirs
  >>= return . map (localPath . (/>) outputDir)
  >>= mapM_ remove
