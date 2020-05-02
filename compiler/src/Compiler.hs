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


import GHC.Generics (Generic)
import Control.Monad (liftM2, when)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob
import System.Directory (canonicalizePath)

import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON

import Config
import Input (InputTree, readInputTree, filterInputTree, sidecar, tags)
import Resource (GalleryItem, buildGalleryTree, galleryCleanupResourceDir)
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


defaultGalleryConf :: String
defaultGalleryConf = "gallery.yaml"

defaultIndexFile :: String
defaultIndexFile = "index.json"

itemsDir :: String
itemsDir = "items"

thumbnailsDir :: String
thumbnailsDir = "thumbnails"


data GalleryIndex = GalleryIndex
  { properties :: ViewerConfig
  , tree :: GalleryItem
  } deriving (Generic, Show, ToJSON)


writeJSON :: ToJSON a => FileName -> a -> IO ()
writeJSON outputPath object =
  do
    putStrLn $ "Generating:\t" ++ outputPath
    ensureParentDir JSON.encodeFile outputPath object


(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = liftM2 (&&)

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = liftM2 (||)

anyPattern :: [String] -> String -> Bool
anyPattern patterns string = any (flip Glob.match string) (map Glob.compile patterns)

galleryDirFilter :: GalleryConfig -> [FilePath] -> FSNode -> Bool
galleryDirFilter config excludedCanonicalDirs =
      (not . isHidden)
  &&& (not . isExcludedDir)
  &&& ((matchesDir $ anyPattern $ includedDirectories config) |||
       (matchesFile $ anyPattern $ includedFiles config))
  &&& (not . ((matchesDir $ anyPattern $ excludedDirectories config) |||
              (matchesFile $ anyPattern $ excludedFiles config)))

  where
    matchesDir :: (FileName -> Bool) -> FSNode -> Bool
    matchesDir cond dir@Dir{} = maybe False cond $ nodeName dir
    matchesDir _ File{} = False

    matchesFile :: (FileName -> Bool) -> FSNode -> Bool
    matchesFile cond file@File{} = maybe False cond $ nodeName file
    matchesFile _ Dir{} = False

    isExcludedDir :: FSNode -> Bool
    isExcludedDir Dir{canonicalPath} = any (canonicalPath ==) excludedCanonicalDirs
    isExcludedDir File{} = False

inputTreeFilter :: GalleryConfig -> InputTree -> Bool
inputTreeFilter GalleryConfig{includedTags, excludedTags} =
      (hasTagMatching $ anyPattern includedTags)
  &&& (not . (hasTagMatching $ anyPattern excludedTags))

  where
    hasTagMatching :: (String -> Bool) -> InputTree -> Bool
    hasTagMatching cond = (any cond) . (fromMaybe [""] . tags) . sidecar


compileGallery :: FilePath -> FilePath -> FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> IO ()
compileGallery configPath inputDirPath outputDirPath outputIndexPath excludedDirs rebuildAll cleanOutput =
  do
    config <- readConfig $ inputGalleryConf configPath

    inputDir <- readDirectory inputDirPath
    excludedCanonicalDirs <- mapM canonicalizePath excludedDirs
    let sourceFilter = galleryDirFilter config excludedCanonicalDirs
    let sourceTree = filterDir sourceFilter inputDir
    inputTree <- readInputTree sourceTree
    let curatedInputTree = filterInputTree (inputTreeFilter config) inputTree

    let cache = if rebuildAll then skipCached else withCached
    let itemProc = itemProcessor config cache
    let thumbnailProc = thumbnailProcessor config cache
    let galleryBuilder = buildGalleryTree itemProc thumbnailProc (tagsFromDirectories config)
    resources <- galleryBuilder curatedInputTree

    when cleanOutput $ galleryCleanupResourceDir resources outputDirPath
    writeJSON (outputGalleryIndex outputIndexPath) $ GalleryIndex (viewerConfig config) resources

  where
    inputGalleryConf :: FilePath -> FilePath
    inputGalleryConf "" = inputDirPath </> defaultGalleryConf
    inputGalleryConf file = file

    outputGalleryIndex :: FilePath -> FilePath
    outputGalleryIndex "" = outputDirPath </> defaultIndexFile
    outputGalleryIndex file = file

    itemProcessor config cache =
      itemFileProcessor
        (pictureMaxResolution config) cache
        inputDirPath outputDirPath itemsDir
    thumbnailProcessor config cache =
      thumbnailFileProcessor
        (thumbnailMaxResolution config) cache
        inputDirPath outputDirPath thumbnailsDir
