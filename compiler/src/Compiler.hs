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

module Compiler
  ( compileGallery
  ) where


import Control.Monad (liftM2)
import Data.List (any)
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob

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
  , ensureParentDir )
import Processors
  ( itemFileProcessor, thumbnailFileProcessor
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


galleryDirFilter :: ([Glob.Pattern], [Glob.Pattern]) -> FSNode -> Bool
galleryDirFilter (inclusionPatterns, exclusionPatterns) =
      (not . isHidden)
  &&& (matchName True $ anyPattern inclusionPatterns)
  &&& (not . isConfigFile)
  &&& (not . containsOutputGallery)
  &&& (not . (matchName False $ anyPattern exclusionPatterns))

  where
    (&&&) = liftM2 (&&)
    (|||) = liftM2 (||)

    matchName :: Bool -> (FileName -> Bool) -> FSNode -> Bool
    matchName matchDir _ Dir{} = matchDir
    matchName _ cond file@File{} = maybe False cond $ nodeName file

    anyPattern :: [Glob.Pattern] -> FileName -> Bool
    anyPattern patterns filename = any (flip Glob.match filename) patterns

    isConfigFile = matchName False (== galleryConf)
    isGalleryIndex = matchName False (== indexFile)
    isViewerIndex = matchName False (== viewerMainFile)
    containsOutputGallery File{} = False
    containsOutputGallery Dir{items} = any (isGalleryIndex ||| isViewerIndex) items


compileGallery :: FilePath -> FilePath -> Bool -> IO ()
compileGallery inputDirPath outputDirPath rebuildAll =
  do
    fullConfig <- readConfig inputGalleryConf
    let config = compiler fullConfig

    inputDir <- readDirectory inputDirPath
    let inclusionPatterns = map Glob.compile $ includeFiles config
    let exclusionPatterns = map Glob.compile $ excludeFiles config
    let sourceFilter = galleryDirFilter (inclusionPatterns, exclusionPatterns)
    let sourceTree = filterDir sourceFilter inputDir
    inputTree <- readInputTree sourceTree

    let cache = if rebuildAll then skipCached else withCached
    let itemProc = itemProcessor (pictureMaxResolution config) cache
    let thumbnailProc = thumbnailProcessor (thumbnailMaxResolution config) cache
    let galleryBuilder = buildGalleryTree itemProc thumbnailProc (tagsFromDirectories config)
    resources <- galleryBuilder (galleryName config) inputTree

    galleryCleanupResourceDir resources outputDirPath
    writeJSON outputIndex resources
    writeJSON outputViewerConf $ viewer fullConfig

  where
    inputGalleryConf = inputDirPath </> galleryConf
    outputIndex = outputDirPath </> indexFile
    outputViewerConf = outputDirPath </> viewerConfFile

    itemProcessor maxRes cache =
      itemFileProcessor maxRes cache inputDirPath outputDirPath itemsDir
    thumbnailProcessor thumbRes cache =
      thumbnailFileProcessor thumbRes cache inputDirPath outputDirPath thumbnailsDir
