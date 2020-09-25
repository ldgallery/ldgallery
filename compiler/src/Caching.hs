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

module Caching
  ( Cache
  , noCache
  , ItemCache
  , buildItemCache
  , useCached
  ) where


import Control.Monad (when)
import qualified Data.Map.Strict as Map
import System.Directory (removePathForcibly, doesDirectoryExist, doesFileExist)

import FileProcessors (FileProcessor)
import Resource (GalleryItem(..), flattenGalleryTree)
import Files


type Cache a = FileProcessor a -> FileProcessor a


noCache :: Cache a
noCache processor itemPath resPath inputFsPath outputFsPath =
  removePathForcibly outputFsPath
  >> processor itemPath resPath inputFsPath outputFsPath


type ItemCache = Path -> Maybe GalleryItem

buildItemCache :: Maybe GalleryItem -> ItemCache
buildItemCache cachedItems = lookupCache
  where
    withKey item = (webPath $ Resource.path item, item)
    cachedItemList = maybe [] flattenGalleryTree cachedItems
    cachedMap = Map.fromList (map withKey cachedItemList)
    lookupCache path = Map.lookup (webPath path) cachedMap

useCached :: ItemCache -> (GalleryItem -> a) -> Cache a
useCached cache propGetter processor itemPath resPath inputFsPath outputFsPath =
  do
    isDir <- doesDirectoryExist outputFsPath
    when isDir $ removePathForcibly outputFsPath

    fileExists <- doesFileExist outputFsPath
    if fileExists then
      do
        needUpdate <- isOutdated True inputFsPath outputFsPath
        case (needUpdate, cache itemPath) of
          (False, Just props) -> fromCache props
          _ -> update
    else
      update

  where
    update = processor itemPath resPath inputFsPath outputFsPath
    fromCache props =
      putStrLn ("From cache:\t" ++ outputFsPath)
      >> return (propGetter props)
