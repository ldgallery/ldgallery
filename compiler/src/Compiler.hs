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

{-# LANGUAGE
    DuplicateRecordFields
  , DeriveGeneric
  , DeriveAnyClass
  , NamedFieldPuns
#-}

module Compiler
  ( compileGallery
  ) where


import Control.Monad (liftM2)
import Data.List (any)
import Data.Maybe (isJust, fromMaybe)
import Text.Regex (Regex, mkRegex, matchRegex)
import System.FilePath ((</>))

import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON

import Config
import Input (readInputTree)
import Resource (buildGalleryTree, galleryCleanupResourceDir)
import Files
  ( FileName
  , FSNode(..)
  , readDirectory
  , isHidden
  , nodeName
  , filterDir
  , ensureParentDir
  , isOutdated )
import Processors
  ( dirFileProcessor, itemFileProcessor, thumbnailFileProcessor
  , skipCached, withCached )


galleryConf :: String
galleryConf = "gallery.yaml"

indexFile :: String
indexFile = "index.json"

viewerMainFile :: String
viewerMainFile = "index.html"

viewerConfFile :: String
viewerConfFile = "viewer.json"

itemsDir :: String
itemsDir = "items"

thumbnailsDir :: String
thumbnailsDir = "thumbnails"


writeJSON :: ToJSON a => FileName -> a -> IO ()
writeJSON outputPath object =
  do
    putStrLn $ "Generating:\t" ++ outputPath
    ensureParentDir JSON.encodeFile outputPath object


galleryDirFilter :: Regex -> FSNode -> Bool
galleryDirFilter excludeRegex =
      (not . isHidden)
  &&& (not . isConfigFile)
  &&& (not . containsOutputGallery)
  &&& (not . excludedName)

  where
    (&&&) = liftM2 (&&)
    (|||) = liftM2 (||)

    matchName :: (FileName -> Bool) -> FSNode -> Bool
    matchName cond = maybe False cond . nodeName

    isConfigFile = matchName (== galleryConf)
    isGalleryIndex = matchName (== indexFile)
    isViewerIndex = matchName (== viewerMainFile)
    containsOutputGallery File{} = False
    containsOutputGallery Dir{items} = any (isGalleryIndex ||| isViewerIndex) items
    excludedName = isJust . matchRegex excludeRegex . fromMaybe "" . nodeName


compileGallery :: FilePath -> FilePath -> Bool -> IO ()
compileGallery inputDirPath outputDirPath rebuildAll =
  do
    fullConfig <- readConfig inputGalleryConf
    let config = compiler fullConfig

    inputDir <- readDirectory inputDirPath
    let sourceFilter = galleryDirFilter (mkRegex $ ignoreFiles config)
    let sourceTree = filterDir sourceFilter inputDir
    inputTree <- readInputTree sourceTree

    invalidateCache <- isOutdated False inputGalleryConf outputIndex
    let cache = if invalidateCache || rebuildAll then skipCached else withCached

    let itemProc = itemProcessor (pictureMaxResolution config) cache
    let thumbnailProc = thumbnailProcessor (thumbnailMaxResolution config) cache
    let galleryBuilder = buildGalleryTree dirProcessor itemProc thumbnailProc (implicitDirectoryTag config)
    resources <- galleryBuilder (galleryName config) inputTree

    galleryCleanupResourceDir resources outputDirPath
    writeJSON outputIndex resources
    writeJSON outputViewerConf $ viewer fullConfig

  where
    inputGalleryConf = inputDirPath </> galleryConf
    outputIndex = outputDirPath </> indexFile
    outputViewerConf = outputDirPath </> viewerConfFile

    dirProcessor = dirFileProcessor inputDirPath outputDirPath itemsDir
    itemProcessor maxRes cache =
      itemFileProcessor maxRes cache inputDirPath outputDirPath itemsDir
    thumbnailProcessor thumbRes cache =
      thumbnailFileProcessor thumbRes cache inputDirPath outputDirPath thumbnailsDir
