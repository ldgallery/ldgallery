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


import Data.Function ((&))
import Data.Ord (comparing)
import Data.List (sortBy, length)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (dropFileName, (</>))

import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON

import Config
import Files (FileName, readDirectory, localPath, flattenDir, root, (/>))
import Input (decodeYamlFile, readInputTree)
import Resource (ResourceTree, buildResourceTree, outputDiff)
import Gallery (buildGalleryTree)


process :: FilePath -> FilePath -> IO ()
process inputDirPath outputDirPath =
  do
    config <- readConfig (inputDirPath </> "gallery.yaml")
    inputDir <- readDirectory inputDirPath
    inputTree <- readInputTree inputDir

    let resourceTree = buildResourceTree inputTree
    putStrLn "\nRESOURCE TREE"
    putStrLn (show resourceTree)

    -- TODO: make buildResourceTree build a resource compilation strategy
    -- (need to know the settings)
    -- flatten the tree of resources and their strategies
    -- filter resources that are already up to date
    --   (or recompile everything if the config file has changed!)
    -- execute in parallel

    -- TODO: execute (in parallel) the resource compilation strategy list
    -- need to find a good library for that

    cleanup resourceTree outputDirPath

    buildGalleryTree resourceTree
      & writeJSON (outputDirPath </> "index.json")

    viewer config
      & writeJSON (outputDirPath </> "viewer.json")

  where
    cleanup :: ResourceTree -> FileName -> IO ()
    cleanup resourceTree outputDir =
      readDirectory outputDir
      >>= return . outputDiff resourceTree . root
      >>= return . sortBy (flip $ comparing length) -- nested files before dirs
      >>= return . map (localPath . (/>) outputDir)
      >>= mapM_ remove

    remove :: FileName -> IO ()
    remove path =
      do
        putStrLn $ "Removing: " ++ path
        removePathForcibly path

    writeJSON :: ToJSON a => FileName -> a -> IO ()
    writeJSON path obj =
      createDirectoryIfMissing True (dropFileName path)
      >> JSON.encodeFile path obj


testRun :: IO ()
testRun = process "../../example" "../../out"
