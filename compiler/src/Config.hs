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

module Config
  ( GalleryConfig(..), readConfig
  , ViewerConfig(..), viewerConfig
  , TagsFromDirectoriesConfig(..)
  , Resolution(..)
  ) where


import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, withObject, (.:?), (.!=))
import qualified Data.Aeson as JSON

import Files (FileName)
import Input (decodeYamlFile)


data Resolution = Resolution
  { width :: Int
  , height :: Int
  } deriving (Generic, Show, ToJSON, FromJSON)


data TagsFromDirectoriesConfig = TagsFromDirectoriesConfig
  { fromParents :: Int
  , prefix :: String
  } deriving (Generic, Show)

instance FromJSON TagsFromDirectoriesConfig where
  parseJSON = withObject "TagsFromDirectoriesConfig" $ \v -> TagsFromDirectoriesConfig
    <$> v .:? "fromParents" .!= 0
    <*> v .:? "prefix" .!= ""


data GalleryConfig = GalleryConfig
  { galleryTitle :: String
  , includedDirectories :: [String]
  , excludedDirectories :: [String]
  , includedFiles :: [String]
  , excludedFiles :: [String]
  , includedTags :: [String]
  , excludedTags :: [String]
  , tagCategories :: [String]
  , tagsFromDirectories :: TagsFromDirectoriesConfig
  , thumbnailMaxResolution :: Resolution
  , pictureMaxResolution :: Maybe Resolution
  } deriving (Generic, Show)

instance FromJSON GalleryConfig where
  parseJSON = withObject "GalleryConfig" $ \v -> GalleryConfig
    <$> v .:? "galleryTitle" .!= "ldgallery"
    <*> v .:? "includedDirectories" .!= ["*"]
    <*> v .:? "excludedDirectories" .!= []
    <*> v .:? "includedFiles" .!= ["*"]
    <*> v .:? "excludedFiles" .!= []
    <*> v .:? "includedTags" .!= ["*"]
    <*> v .:? "excludedTags" .!= []
    <*> v .:? "tagCategories" .!= []
    <*> v .:? "tagsFromDirectories" .!= TagsFromDirectoriesConfig 0 ""
    <*> v .:? "thumbnailMaxResolution" .!= Resolution 400 300
    <*> v .:? "pictureMaxResolution"

readConfig :: FileName -> IO GalleryConfig
readConfig = decodeYamlFile


data ViewerConfig = ViewerConfig
  { galleryTitle :: String
  , tagCategories :: [String]
  } deriving (Generic, ToJSON, Show)

viewerConfig :: GalleryConfig -> ViewerConfig
viewerConfig GalleryConfig{galleryTitle, tagCategories} = ViewerConfig galleryTitle tagCategories
