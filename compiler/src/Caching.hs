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
  , skipCache
  , withCache
  ) where


import Control.Monad (when)
import System.Directory (removePathForcibly, doesDirectoryExist, doesFileExist)

import FileProcessors (FileProcessor)
import Files


type Cache = FileProcessor -> FileProcessor

skipCache :: Cache
skipCache processor inputPath outputPath =
  removePathForcibly outputPath
  >> processor inputPath outputPath

withCache :: Cache
withCache processor inputPath outputPath =
  do
    isDir <- doesDirectoryExist outputPath
    when isDir $ removePathForcibly outputPath

    fileExists <- doesFileExist outputPath
    if fileExists then
      do
        needUpdate <- isOutdated True inputPath outputPath
        if needUpdate then update else skip
    else
      update

  where
    update = processor inputPath outputPath
    skip = putStrLn $ "Skipping:\t" ++ outputPath
