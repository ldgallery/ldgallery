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
import System.FilePath ((</>))

import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON

import Config
import Files (FileName, readDirectory, localPath, isHidden, nodeName, filterDir, flattenDir, root, (/>), ensureParentDir)
import Input (decodeYamlFile, readInputTree)
import Resource (ResourceTree, buildResourceTree, cleanupResourceDir)
import Gallery (buildGalleryTree)
import Processors


writeJSON :: ToJSON a => FileName -> a -> IO ()
writeJSON outputPath object =
  do
    putStrLn $ "Generating:\t" ++ outputPath
    ensureParentDir JSON.encodeFile outputPath object


compileGallery :: FilePath -> FilePath -> IO ()
compileGallery inputDirPath outputDirPath =
  do
    config <- readConfig (inputDirPath </> galleryConf)
    inputDir <- readDirectory inputDirPath

    let isGalleryFile = \n -> nodeName n == galleryConf
    let galleryTree = filterDir (liftM2 (&&) (not . isGalleryFile) (not . isHidden)) inputDir

    inputTree <- readInputTree galleryTree

    let dirProc = dirFileProcessor inputDirPath outputDirPath itemsDir
    let itemProc = itemFileProcessor Nothing skipCached inputDirPath outputDirPath itemsDir
    let thumbnailProc = thumbnailFileProcessor (Resolution 150 50) skipCached inputDirPath outputDirPath thumbnailsDir
    resourceTree <- buildResourceTree dirProc itemProc thumbnailProc inputTree

    cleanupResourceDir resourceTree outputDirPath

    buildGalleryTree resourceTree
      & writeJSON (outputDirPath </> "index.json")

    viewer config
      & writeJSON (outputDirPath </> "viewer.json")

  where
    galleryConf = "gallery.yaml"
    itemsDir = "items"
    thumbnailsDir = "thumbnails"
