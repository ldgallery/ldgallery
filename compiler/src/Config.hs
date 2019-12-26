{-# LANGUAGE DuplicateRecordFields, DeriveGeneric, DeriveAnyClass #-}

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


module Config
  ( GalleryConfig(..)
  , CompilerConfig(..)
  , readConfig
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as JSON

import Files (FileName)
import Input (decodeYamlFile)


data CompilerConfig = CompilerConfig
  { dummy :: Maybe String -- TODO
  } deriving (Generic, FromJSON, Show)

data GalleryConfig = GalleryConfig
  { compiler :: CompilerConfig
  , viewer :: JSON.Object
  } deriving (Generic, FromJSON, Show)

-- TODO: add compiler config keys and their default values


readConfig :: FileName -> IO GalleryConfig
readConfig = decodeYamlFile
