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
  ( GalleryConfig(..)
  , CompilerConfig(..)
  , TagsFromDirectoriesConfig(..)
  , Resolution(..)
  , readConfig
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


data CompilerConfig = CompilerConfig
  { includedDirectories :: [String]
  , excludedDirectories :: [String]
  , includedFiles :: [String]
  , excludedFiles :: [String]
  , tagsFromDirectories :: TagsFromDirectoriesConfig
  , thumbnailMaxResolution :: Resolution
  , pictureMaxResolution :: Maybe Resolution
  } deriving (Generic, Show)

instance FromJSON CompilerConfig where
  parseJSON = withObject "CompilerConfig" $ \v -> CompilerConfig
    <$> v .:? "includedDirectories" .!= ["*"]
    <*> v .:? "excludedDirectories" .!= []
    <*> v .:? "includedFiles" .!= ["*"]
    <*> v .:? "excludedFiles" .!= []
    <*> v .:? "tagsFromDirectories" .!= (TagsFromDirectoriesConfig 0 "")
    <*> v .:? "thumbnailMaxResolution" .!= (Resolution 400 300)
    <*> v .:? "pictureMaxResolution"


data TagsFromDirectoriesConfig = TagsFromDirectoriesConfig
  { fromParents :: Int
  , prefix :: String
  } deriving (Generic, Show)

instance FromJSON TagsFromDirectoriesConfig where
  parseJSON = withObject "TagsFromDirectoriesConfig" $ \v -> TagsFromDirectoriesConfig
    <$> v .:? "fromParents" .!= 0
    <*> v .:? "prefix" .!= ""


data GalleryConfig = GalleryConfig
  { compiler :: CompilerConfig
  , viewer :: JSON.Object
  } deriving (Generic, FromJSON, Show)

readConfig :: FileName -> IO GalleryConfig
readConfig = decodeYamlFile
