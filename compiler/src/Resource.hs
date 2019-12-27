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

module Resource
  ( ResourceTree(..)
  , DirProcessor
  , ItemProcessor
  , ThumbnailProcessor
  , buildResourceTree
  , flattenResourceTree
  , outputDiff
  ) where


import Data.Function ((&))
import Data.List ((\\), subsequences)
import Data.Maybe (mapMaybe)
import Files
import Input (InputTree(..), Sidecar)


-- | Tree representing the compiled gallery resources.
data ResourceTree =
    ItemResource
      { sidecar :: Sidecar
      , resPath :: Path
      , thumbnailPath :: Maybe Path }
  | DirResource
      { items :: [ResourceTree]
      , resPath :: Path
      , thumbnailPath :: Maybe Path }
  deriving Show


type DirProcessor = Path -> IO Path
type ItemProcessor = Path -> IO Path
type ThumbnailProcessor = Path -> IO (Maybe Path)

-- TODO: parallelise this!
buildResourceTree ::
     DirProcessor -> ItemProcessor -> ThumbnailProcessor -> InputTree
  -> IO ResourceTree
buildResourceTree processDir processItem processThumbnail = resNode
  where
    resNode (InputFile path sidecar) =
      do
        processedItem <- processItem path
        processedThumbnail <- processThumbnail path
        return ItemResource
          { sidecar = sidecar
          , resPath = processedItem
          , thumbnailPath = processedThumbnail }

    resNode (InputDir path thumbnailPath items) =
      do
        processedDir <- processDir path
        processedThumbnail <- maybeThumbnail thumbnailPath
        dirItems <- mapM resNode items
        return DirResource
          { items = dirItems
          , resPath = processedDir
          , thumbnailPath = processedThumbnail }

    maybeThumbnail :: Maybe Path -> IO (Maybe Path)
    maybeThumbnail Nothing = return Nothing
    maybeThumbnail (Just path) = processThumbnail path


flattenResourceTree :: ResourceTree -> [ResourceTree]
flattenResourceTree item@ItemResource{} = [item]
flattenResourceTree dir@(DirResource items _ _) =
  dir:(concatMap flattenResourceTree items)

outputDiff :: ResourceTree -> FSNode -> [Path]
outputDiff resources ref =
  (fsPaths ref) \\ (resPaths $ flattenResourceTree resources)
  where
    resPaths :: [ResourceTree] -> [Path]
    resPaths resList = map resPath resList ++ thumbnailPaths resList

    thumbnailPaths :: [ResourceTree] -> [Path]
    thumbnailPaths = (concatMap subsequences) . (mapMaybe thumbnailPath)

    fsPaths :: FSNode -> [Path]
    fsPaths = map nodePath . tail . flattenDir
