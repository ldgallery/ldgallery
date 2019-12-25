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


module Lib
  ( testRun
  ) where


import GHC.Generics (Generic)
import Data.Function ((&))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName, (</>))
import Data.Aeson (ToJSON, encodeFile)

import Files (FileName, readDirectory)
import Input (readInputTree)
import Resource (buildResourceTree)
import Gallery (buildGalleryTree)


writeJSON :: ToJSON a => FileName -> a -> IO ()
writeJSON path obj =
  createDirectoryIfMissing True (dropFileName path)
  >> encodeFile path obj


process :: FilePath -> FilePath -> IO ()
process inputDirPath outputDirPath =
  do
    inputDir <- readDirectory inputDirPath
    putStrLn "\nINPUT DIR"
    putStrLn (show inputDir)

    outputDir <- readDirectory outputDirPath
    putStrLn "\nOUTPUT DIR"
    putStrLn (show outputDir)

    inputTree <- readInputTree inputDir
    putStrLn "\nINPUT TREE"
    putStrLn (show inputTree)

    let resourceTree = buildResourceTree inputTree
    putStrLn "\nRESOURCE TREE"
    putStrLn (show resourceTree)

    -- TODO: make buildResourceTree build a resource compilation strategy
    -- TODO: clean up output dir by comparing its content with the resource tree
    -- TODO: execute (in parallel) the resource compilation strategy list

    buildGalleryTree resourceTree & writeJSON (outputDirPath </> "index.json")


testRun :: IO ()
testRun = process "../../example" "../../out"
