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

{-# LANGUAGE
    DuplicateRecordFields
  , DeriveGeneric
  , DeriveAnyClass
  , NamedFieldPuns
#-}

module Resource
  ( DirProcessor, ItemProcessor, ThumbnailProcessor
  , GalleryItem(..), GalleryItemProps(..), Resolution(..)
  , buildGalleryTree, galleryCleanupResourceDir
  ) where


import Control.Concurrent.ParallelIO.Global (parallel)
import Data.List ((\\), sortBy)
import Data.Ord (comparing)
import Data.Char (toLower)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Set as Set

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
  | Picture
  | Other
  deriving (Generic, Show)

instance ToJSON GalleryItemProps where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


data GalleryItem = GalleryItem
  { title :: String
  , date :: String -- TODO: checked ISO8601 date
  , description :: String
  , tags :: [Tag]
  , path :: Path
  , thumbnail :: Maybe Path
  , properties :: GalleryItemProps
  } deriving (Generic, Show)

instance ToJSON GalleryItem where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


type DirProcessor = Path -> IO Path
type ItemProcessor = Path -> IO (Path, GalleryItemProps)
type ThumbnailProcessor = Path -> IO (Maybe Path)


buildGalleryTree ::
     DirProcessor -> ItemProcessor -> ThumbnailProcessor
  -> Bool -> String -> InputTree -> IO GalleryItem
buildGalleryTree processDir processItem processThumbnail addDirTag galleryName inputTree =
  mkGalleryItem Nothing inputTree >>= return . named galleryName
  where
    named :: String -> GalleryItem -> GalleryItem
    named name item = item { title = name }

    mkGalleryItem :: Maybe String -> InputTree -> IO GalleryItem
    mkGalleryItem parent InputFile{path, sidecar} =
      do
        (processedItemPath, properties) <- processItem path
        processedThumbnail <- processThumbnail path
        return GalleryItem
          { title = optMeta title $ fromMaybe "" $ fileName path
          , date = optMeta date "" -- TODO: check and normalise dates
          , description = optMeta description ""
          , tags = (optMeta tags []) ++ implicitParentTag parent
          , path = processedItemPath
          , thumbnail = processedThumbnail
          , properties = properties } -- TODO
      where
        optMeta :: (Sidecar -> Maybe a) -> a -> a
        optMeta get fallback = fromMaybe fallback $ get sidecar

    mkGalleryItem parent InputDir{path, dirThumbnailPath, items} =
      do
        processedDir <- processDir path
        processedThumbnail <- maybeThumbnail dirThumbnailPath
        processedItems <- parallel $ map (mkGalleryItem $ fileName path) items
        return GalleryItem
          { title = fromMaybe "" $ fileName path
            -- TODO: consider using the most recent item's date? what if empty?
          , date = ""
            -- TODO: consider allowing metadata sidecars for directories too
          , description = ""
          , tags = (aggregateChildTags processedItems) ++ implicitParentTag parent
          , path = processedDir
          , thumbnail = processedThumbnail
          , properties = Directory processedItems }
      where
        maybeThumbnail :: Maybe Path -> IO (Maybe Path)
        maybeThumbnail Nothing = return Nothing
        maybeThumbnail (Just thumbnailPath) = processThumbnail thumbnailPath

        aggregateChildTags :: [GalleryItem] -> [Tag]
        aggregateChildTags = unique . concatMap (\item -> tags (item::GalleryItem))

        unique :: Ord a => [a] -> [a]
        unique = Set.toList . Set.fromList

    implicitParentTag :: Maybe String -> [Tag]
    implicitParentTag Nothing = []
    implicitParentTag (Just parent) = if addDirTag then [parent] else []


flattenGalleryTree :: GalleryItem -> [GalleryItem]
flattenGalleryTree dir@(GalleryItem _ _ _ _ _ _ (Directory items)) =
  dir : concatMap flattenGalleryTree items
flattenGalleryTree simple = [simple]


galleryOutputDiff :: GalleryItem -> FSNode -> [Path]
galleryOutputDiff resources ref =
  (fsPaths ref) \\ (resPaths $ flattenGalleryTree resources)
  where
    resPaths :: [GalleryItem] -> [Path]
    resPaths resList = map (path::(GalleryItem->Path)) resList ++ thumbnailPaths resList

    thumbnailPaths :: [GalleryItem] -> [Path]
    thumbnailPaths = (concatMap subPaths) . (mapMaybe thumbnail)

    fsPaths :: FSNode -> [Path]
    fsPaths = map Files.path . tail . flattenDir


galleryCleanupResourceDir :: GalleryItem -> FileName -> IO ()
galleryCleanupResourceDir resourceTree outputDir =
  readDirectory outputDir
  >>= return . galleryOutputDiff resourceTree . root
  >>= return . sortBy (flip $ comparing pathLength) -- nested files before dirs
  >>= return . map (localPath . (/>) outputDir)
  >>= mapM_ remove
