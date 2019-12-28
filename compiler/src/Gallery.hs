-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019  Pacien TRAN-GIRARD
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
#-}

module Gallery
  ( GalleryItem(..), buildGallery
  ) where


import GHC.Generics (Generic)
import Data.Char (toLower)
import Data.Function ((&))
import Data.Maybe (fromMaybe)

import Data.Aeson (ToJSON, genericToJSON, genericToEncoding)
import qualified Data.Aeson as JSON

import qualified Data.Set as Set

import Files
import Input
import Resource


encodingOptions :: JSON.Options
encodingOptions = JSON.defaultOptions
  { JSON.fieldLabelModifier = map toLower
  , JSON.constructorTagModifier = map toLower
  , JSON.sumEncoding = JSON.defaultTaggedObject
      { JSON.tagFieldName = "type"
      , JSON.contentsFieldName = "contents"
      }
  }


type ResourcePath = String
type Tag = String
type FileSizeKB = Int


data Resolution = Resolution
  { width :: Int
  , height :: Int
  } deriving (Generic, Show)

instance ToJSON Resolution where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


data GalleryItemProps =
    Directory { items :: [GalleryItem] }
--  | Image { resolution :: Resolution, filesize :: FileSizeKB }
--  | Video { filesize :: FileSizeKB }
  | Unknown
  deriving (Generic, Show)

instance ToJSON GalleryItemProps where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


-- TODO: fuse GalleryItem and GalleryItemProps
data GalleryItem = GalleryItem
  { title :: String
  , date :: String -- TODO: checked ISO8601 date
  , description :: String
  , tags :: [Tag]
  , path :: ResourcePath
  , thumbnail :: Maybe ResourcePath
  , properties :: GalleryItemProps
  } deriving (Generic, Show)

instance ToJSON GalleryItem where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions


buildGalleryTree :: ResourceTree -> GalleryItem
buildGalleryTree (ItemResource sidecar path@(filename:_) thumbnail) =
  GalleryItem
    { title = optMeta title filename
    , date = optMeta date "" -- TODO: check and normalise dates
    , description = optMeta description ""
    , tags = optMeta tags []
    , path = webPath path
    , thumbnail = fmap webPath thumbnail
    , properties = Unknown } -- TODO
  where
    optMeta :: (Sidecar -> Maybe a) -> a -> a
    optMeta get fallback = fromMaybe fallback $ get sidecar

buildGalleryTree (DirResource dirItems path@(dirname:_) thumbnail) =
  map buildGalleryTree dirItems
  & \items -> GalleryItem
    { title = dirname
      -- TODO: consider using the most recent item's date? what if empty?
    , date = ""
      -- TODO: consider allowing metadata sidecars for directories too
    , description = ""
    , tags = aggregateChildTags items
    , path = webPath path
    , thumbnail = fmap webPath thumbnail
    , properties = Directory items }
  where
    aggregateChildTags :: [GalleryItem] -> [Tag]
    aggregateChildTags = unique . concatMap (\item -> tags (item::GalleryItem))

    unique :: Ord a => [a] -> [a]
    unique = Set.toList . Set.fromList

buildGallery :: String -> ResourceTree -> GalleryItem
buildGallery galleryName resourceTree =
  (buildGalleryTree resourceTree) { title = galleryName }
