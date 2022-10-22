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
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Map (fromAscList)
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob
import System.Directory (canonicalizePath, doesFileExist)

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as JSON

import Config
import Input
  ( InputTree
  , readInputTree
  , filterInputTree
  , sidecar
  , aggregateTags )
import qualified Input (tags)
import Resource
  ( GalleryItem
  , GalleryItemProps
  , Thumbnail
  , Tag
  , buildGalleryTree
  , galleryCleanupResourceDir
  , properties
  , thumbnail)
import Files
  ( FileName
  , FSNode(..)
  , readDirectory
  , isHidden
  , nodeName
  , filterDir
  , ensureParentDir )
import ItemProcessors (ItemProcessor, itemFileProcessor, thumbnailFileProcessor)
import Caching (Cache, noCache, buildItemCache, useCached)


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
  , tags :: [Tag]
  , tree :: GalleryItem
  } deriving (Generic, Show, ToJSON, FromJSON)


writeJSON :: ToJSON a => FileName -> a -> IO ()
writeJSON outputPath object =
  do
    putStrLn $ "Generating:\t" ++ outputPath
    ensureParentDir JSON.encodeFile outputPath object

loadGalleryIndex :: FilePath -> IO (Maybe GalleryIndex)
loadGalleryIndex path =
  doesFileExist path >>= bool (return Nothing) decodeIndex
  where
    decodeIndex =
      JSON.eitherDecodeFileStrict path
      >>= either (\err -> warn err >> return Nothing) (return . Just)
    warn = putStrLn . ("Warning:\tUnable to reuse existing index as cache: " ++)


(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = liftM2 (&&)

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = liftM2 (||)

anyPattern :: [String] -> String -> Bool
anyPattern patterns string = any (flip Glob.match string . Glob.compile) patterns

galleryDirFilter :: GalleryConfig -> [FilePath] -> FSNode -> Bool
galleryDirFilter config excludedCanonicalDirs =
      (not . isHidden)
  &&& (not . isExcludedDir)
  &&& (matchesDir (anyPattern $ includedDirectories config) |||
       matchesFile (anyPattern $ includedFiles config))
  &&& (not . (matchesDir (anyPattern $ excludedDirectories config) |||
              matchesFile (anyPattern $ excludedFiles config)))

  where
    matchesDir :: (FileName -> Bool) -> FSNode -> Bool
    matchesDir cond dir@Dir{} = maybe False cond $ nodeName dir
    matchesDir _ File{} = False

    matchesFile :: (FileName -> Bool) -> FSNode -> Bool
    matchesFile cond file@File{} = maybe False cond $ nodeName file
    matchesFile _ Dir{} = False

    isExcludedDir :: FSNode -> Bool
    isExcludedDir Dir{canonicalPath} = canonicalPath `elem` excludedCanonicalDirs
    isExcludedDir File{} = False

inputTreeFilter :: GalleryConfig -> InputTree -> Bool
inputTreeFilter GalleryConfig{includedTags, excludedTags} =
      hasTagMatching (anyPattern includedTags)
  &&& (not . hasTagMatching (anyPattern excludedTags))

  where
    hasTagMatching :: (String -> Bool) -> InputTree -> Bool
    hasTagMatching cond = any cond . (fromMaybe [""] . Input.tags) . sidecar


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

    let galleryIndexPath = outputGalleryIndex outputIndexPath
    cachedIndex <- loadCachedIndex galleryIndexPath
    let cache = mkCache cachedIndex

    let tagsFromDirsCfg = tagsFromDirectories config
    let tagList = aggregateTags tagsFromDirsCfg inputTree
    let tagIdMap = fromAscList (zip tagList [0..])

    let itemProc = itemProcessor config (cache $ return . Resource.properties)
    let thumbnailProc = thumbnailProcessor config (cache $ fmap return . Resource.thumbnail)
    let galleryBuilder = buildGalleryTree itemProc thumbnailProc tagsFromDirsCfg
    resources <- galleryBuilder tagIdMap curatedInputTree

    when cleanOutput $ galleryCleanupResourceDir resources outputDirPath
    writeJSON galleryIndexPath $ GalleryIndex (viewerConfig config) tagList resources

  where
    inputGalleryConf :: FilePath -> FilePath
    inputGalleryConf "" = inputDirPath </> defaultGalleryConf
    inputGalleryConf file = file

    outputGalleryIndex :: FilePath -> FilePath
    outputGalleryIndex "" = outputDirPath </> defaultIndexFile
    outputGalleryIndex file = file

    loadCachedIndex :: FilePath -> IO (Maybe GalleryIndex)
    loadCachedIndex galleryIndexPath =
      if rebuildAll
        then return Nothing
        else loadGalleryIndex galleryIndexPath

    mkCache :: Maybe GalleryIndex -> (GalleryItem -> Maybe a) -> Cache a
    mkCache refGalleryIndex =
      if rebuildAll
        then const noCache
        else useCached (buildItemCache $ fmap tree refGalleryIndex)

    itemProcessor :: GalleryConfig -> Cache GalleryItemProps -> ItemProcessor GalleryItemProps
    itemProcessor config cache =
      itemFileProcessor
        (pictureMaxResolution config) cache
        inputDirPath outputDirPath itemsDir

    thumbnailProcessor :: GalleryConfig -> Cache (Maybe Thumbnail) -> ItemProcessor (Maybe Thumbnail)
    thumbnailProcessor config cache =
      thumbnailFileProcessor
        (thumbnailMaxResolution config) cache
        inputDirPath outputDirPath thumbnailsDir
