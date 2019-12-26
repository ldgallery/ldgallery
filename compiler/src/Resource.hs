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


module Resource
  ( ResourceTree(..)
  , buildResourceTree
  , flattenResourceTree
  , outputDiff
  ) where


import Data.Function ((&))
import Data.List ((\\))
import Files
import Input


-- | Tree representing the compiled gallery resources.
data ResourceTree =
    ItemResource
      { sidecar :: Sidecar
      , resPath :: Path
      , itemThumbnailPath :: Path }
  | DirResource
      { items :: [ResourceTree]
      , resPath :: Path
      , dirThumbnailPath :: Maybe Path }
  deriving Show


-- TODO: actually generate compilation strategies
buildResourceTree :: InputTree -> ResourceTree
buildResourceTree = resNode
  where
    resNode (InputFile path sidecar) =
      ItemResource
        { sidecar = sidecar
        , resPath = itemsDir /> path
        , itemThumbnailPath = thumbnailsDir /> path }

    resNode (InputDir path thumbnailPath items) =
      map resNode items
      & \dirItems -> DirResource
        { items = dirItems
        , resPath = itemsDir /> path
        , dirThumbnailPath = fmap ((/>) thumbnailsDir) thumbnailPath }

    itemsDir = "items"
    thumbnailsDir = "thumbnails"


flattenResourceTree :: ResourceTree -> [ResourceTree]
flattenResourceTree item@ItemResource{} = [item]
flattenResourceTree dir@(DirResource items _ _) =
  dir:(concatMap flattenResourceTree items)


outputDiff :: ResourceTree -> FSNode -> [Path]
outputDiff resources ref = (fsPaths ref) \\ (resPaths resources)
  where
    resPaths :: ResourceTree -> [Path]
    resPaths = map resPath . flattenResourceTree

    fsPaths :: FSNode -> [Path]
    fsPaths = map nodePath . tail . flattenDir
