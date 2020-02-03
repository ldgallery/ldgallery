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
  , writeJSON
  ) where


import Control.Monad (liftM2)
import Data.List (any)
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob
import System.Directory (canonicalizePath)

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


galleryDirFilter :: CompilerConfig -> [FilePath] -> FSNode -> Bool
galleryDirFilter config excludedCanonicalDirs =
      (not . isHidden)
  &&& (not . isExcludedDir)
  &&& (not . matchesFile (== galleryConf))
  &&& ((matchesDir $ anyPattern $ includedDirectories config) |||
       (matchesFile $ anyPattern $ includedFiles config))
  &&& (not . ((matchesDir $ anyPattern $ excludedDirectories config) |||
              (matchesFile $ anyPattern $ excludedFiles config)))

  where
    (&&&) = liftM2 (&&)
    (|||) = liftM2 (||)

    matchesDir :: (FileName -> Bool) -> FSNode -> Bool
    matchesDir cond dir@Dir{} = maybe False cond $ nodeName dir
    matchesDir _ File{} = False

    matchesFile :: (FileName -> Bool) -> FSNode -> Bool
    matchesFile cond file@File{} = maybe False cond $ nodeName file
    matchesFile _ Dir{} = False

    anyPattern :: [String] -> FileName -> Bool
    anyPattern patterns filename = any (flip Glob.match filename) (map Glob.compile patterns)

    isExcludedDir :: FSNode -> Bool
    isExcludedDir Dir{canonicalPath} = any (canonicalPath ==) excludedCanonicalDirs
    isExcludedDir File{} = False


compileGallery :: FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> IO ()
compileGallery inputDirPath outputDirPath excludedDirs rebuildAll cleanOutput =
  do
    fullConfig <- readConfig inputGalleryConf
    let config = compiler fullConfig

    inputDir <- readDirectory inputDirPath
    excludedCanonicalDirs <- mapM canonicalizePath excludedDirs
    let sourceFilter = galleryDirFilter config excludedCanonicalDirs
    let sourceTree = filterDir sourceFilter inputDir
    inputTree <- readInputTree sourceTree

    let cache = if rebuildAll then skipCached else withCached
    let itemProc = itemProcessor config cache
    let thumbnailProc = thumbnailProcessor config cache
    let galleryBuilder = buildGalleryTree itemProc thumbnailProc (tagsFromDirectories config)
    resources <- galleryBuilder (galleryName config) inputTree

    if cleanOutput then
      galleryCleanupResourceDir resources outputDirPath
    else
      return ()

    writeJSON outputIndex resources
    writeJSON outputViewerConf $ viewer fullConfig

  where
    inputGalleryConf = inputDirPath </> galleryConf
    outputIndex = outputDirPath </> indexFile
    outputViewerConf = outputDirPath </> viewerConfFile

    itemProcessor config cache =
      itemFileProcessor
        (pictureMaxResolution config) cache
        inputDirPath outputDirPath itemsDir
    thumbnailProcessor config cache =
      thumbnailFileProcessor
        (thumbnailMaxResolution config) cache
        inputDirPath outputDirPath thumbnailsDir
