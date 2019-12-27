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

module Compiler
  ( compileGallery
  ) where


import Control.Monad
import Data.Function ((&))
import Data.Ord (comparing)
import Data.List (sortBy, length)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (dropFileName, (</>))

import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON

import Config
import Files (FileName, readDirectory, localPath, isHidden, nodeName, filterDir, flattenDir, root, (/>), ensureParentDir)
import Input (decodeYamlFile, readInputTree)
import Resource (ResourceTree, buildResourceTree, outputDiff)
import Gallery (buildGalleryTree)
import Processors


itemsDir :: String
itemsDir = "items"

thumbnailsDir :: String
thumbnailsDir = "thumbnails"


compileGallery :: FilePath -> FilePath -> IO ()
compileGallery inputDirPath outputDirPath =
  do
    config <- readConfig (inputDirPath </> "gallery.yaml")
    inputDir <- readDirectory inputDirPath

    let isGalleryFile = \n -> nodeName n == "gallery.yaml"
    let galleryTree = filterDir (liftM2 (&&) (not . isGalleryFile) (not . isHidden)) inputDir

    inputTree <- readInputTree galleryTree

    let dirProc = dirFileProcessor inputDirPath outputDirPath itemsDir
    let itemProc = itemFileProcessor Nothing skipCached inputDirPath outputDirPath itemsDir
    let thumbnailProc = thumbnailFileProcessor (Resolution 150 50) skipCached inputDirPath outputDirPath thumbnailsDir
    resourceTree <- buildResourceTree dirProc itemProc thumbnailProc inputTree

    putStrLn "\nRESOURCE TREE"
    putStrLn (show resourceTree)

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
    writeJSON outputPath object =
      do
        putStrLn $ "Generating: " ++ outputPath
        ensureParentDir JSON.encodeFile outputPath object
