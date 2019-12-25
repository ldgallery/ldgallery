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


module Lib
  ( testRun
  ) where


import GHC.Generics (Generic)
import Data.Function ((&))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName, (</>))

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as JSON

import Files (FileName, readDirectory)
import Input (decodeYamlFile, readInputTree)
import Resource (buildResourceTree)
import Gallery (buildGalleryTree)


data CompilerConfig = CompilerConfig
  { dummy :: Maybe String -- TODO
  } deriving (Generic, FromJSON, Show)

data GalleryConfig = GalleryConfig
  { compiler :: CompilerConfig
  , viewer :: JSON.Object
  } deriving (Generic, FromJSON, Show)

readConfig :: FileName -> IO GalleryConfig
readConfig = decodeYamlFile


process :: FilePath -> FilePath -> IO ()
process inputDirPath outputDirPath =
  do
    config <- readConfig (inputDirPath </> "gallery.yaml")

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
    -- (need to know the settings)
    -- flatten the tree of resources and their strategies
    -- filter resources that are already up to date
    --   (or recompile everything if the config file has changed!)
    -- execute in parallel

    -- TODO: clean up output dir by comparing its content with the resource tree
    -- aggregate both trees as list
    -- compute the difference
    -- sort by deepest and erase files and dirs

    -- TODO: execute (in parallel) the resource compilation strategy list
    -- need to find a good library for that

    buildGalleryTree resourceTree & writeJSON (outputDirPath </> "index.json")
    writeJSON (outputDirPath </> "viewer.json") (viewer config)

  where
    writeJSON :: ToJSON a => FileName -> a -> IO ()
    writeJSON path obj =
      createDirectoryIfMissing True (dropFileName path)
      >> JSON.encodeFile path obj


testRun :: IO ()
testRun = process "../../example" "../../out"
