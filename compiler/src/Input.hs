-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2022  Pacien TRAN-GIRARD
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

module Input
  ( Sidecar(..)
  , InputTree(..), readInputTree, filterInputTree
  , aggregateTags
  ) where


import GHC.Generics (Generic)
import Control.Exception (AssertionFailed(..), throw)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Bool (bool)
import Data.List (find, isSuffixOf, sort, group)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime)
import Data.Aeson (FromJSON)
import qualified Data.Map.Strict as Map
import System.FilePath (isExtensionOf, dropExtension)
import System.Directory (doesFileExist, getModificationTime)

import Files
import Config (TagsFromDirectoriesConfig(..))


-- | Tree representing the input from the input directory.
data InputTree =
    InputFile
      { path :: Path
      , modTime :: UTCTime
      , sidecar :: Sidecar
      , thumbnailPath :: Maybe Path }
    | InputDir
      { path :: Path
      , modTime :: UTCTime
      , sidecar :: Sidecar
      , thumbnailPath :: Maybe Path
      , items :: [InputTree] }
  deriving Show

data Sidecar = Sidecar
  { title :: Maybe String
  , datetime :: Maybe ZonedTime
  , description :: Maybe String
  , tags :: Maybe [String]
  } deriving (Generic, FromJSON, Show)

emptySidecar :: Sidecar
emptySidecar = Sidecar
  { title = Nothing
  , datetime = Nothing
  , description = Nothing
  , tags = Nothing }

sidecarExt :: String
sidecarExt = "yaml"

thumbnailSuffix :: String
thumbnailSuffix = "_thumbnail"

dirPropFile :: String
dirPropFile = "_directory"

dirSidecar :: Path
dirSidecar = Path [dirPropFile] <.> sidecarExt

readSidecarFile :: FilePath -> IO Sidecar
readSidecarFile filepath =
  doesFileExist filepath
  >>= bool (return Nothing) (decodeYamlFile filepath)
  <&> fromMaybe emptySidecar


readInputTree :: AnchoredFSNode -> IO InputTree
readInputTree (AnchoredFSNode anchor root) = mkDirNode root
  where
    mkInputNode :: Map.Map FileName FSNode -> FSNode -> IO (Maybe InputTree)
    mkInputNode dir file@File{path} | not (isSidecar file) && not (isThumbnail file) =
      do
        sidecar <- readSidecarFile $ localPath (anchor /> path <.> sidecarExt)
        modTime <- getModificationTime $ localPath (anchor /> path)
        let thumbnail = findFileThumbnail (fromMaybe "" $ fileName path) dir
        return $ Just $ InputFile path modTime sidecar thumbnail
    mkInputNode _ File{} = return Nothing
    mkInputNode _ dir@Dir{} = Just <$> mkDirNode dir

    mkDirNode :: FSNode -> IO InputTree
    mkDirNode File{} = throw $ AssertionFailed "Input directory is a file"
    mkDirNode Dir{path, items} =
      do
        dirItems <- mapM (mkInputNode $ Map.fromList (map withBaseName items)) items
        modTime <- getModificationTime $ localPath (anchor /> path)
        sidecar <- readSidecarFile $ localPath (anchor /> path </> dirSidecar)
        return $ InputDir path modTime sidecar (findDirThumbnail items) (catMaybes dirItems)

    withBaseName :: FSNode -> (FileName, FSNode)
    withBaseName node = (fromMaybe "" $ baseName $ Files.path node, node)

    findFileThumbnail :: FileName -> Map.Map FileName FSNode -> Maybe Path
    findFileThumbnail name dict = Files.path <$> Map.lookup (name ++ thumbnailSuffix) dict

    isSidecar :: FSNode -> Bool
    isSidecar Dir{} = False
    isSidecar File{path} = fileName path & maybe False (isExtensionOf sidecarExt)

    baseName :: Path -> Maybe FileName
    baseName = fmap dropExtension . fileName

    isThumbnail :: FSNode -> Bool
    isThumbnail Dir{} = False
    isThumbnail File{path} = baseName path & maybe False (thumbnailSuffix `isSuffixOf`)

    isDirThumbnail :: FSNode -> Bool
    isDirThumbnail Dir{} = False
    isDirThumbnail File{path} = baseName path & (== Just thumbnailSuffix)

    findDirThumbnail :: [FSNode] -> Maybe Path
    findDirThumbnail = fmap Files.path . find isDirThumbnail


-- | Filters an InputTree. The root is always returned.
filterInputTree :: (InputTree -> Bool) -> InputTree -> InputTree
filterInputTree cond = filterNode
  where
    filterNode :: InputTree -> InputTree
    filterNode inputFile@InputFile{} = inputFile
    filterNode inputDir@InputDir{items} =
        filter cond items
      & map filterNode
      & \curatedItems -> inputDir { items = curatedItems } :: InputTree


-- | Aggregates distinct tags from all the sidecars of an InputTree.
--   The list is stable (sorted alphabetically).
aggregateTags :: TagsFromDirectoriesConfig -> InputTree -> [String]
aggregateTags tagsFromDirsCfg treeNode =
  map head $ group $ sort $ aggregateNode treeNode

  where
    aggregateNode :: InputTree -> [String]
    aggregateNode (InputFile { sidecar }) = extractFromSidecar sidecar
    aggregateNode (InputDir { sidecar, items, path }) =
      (extractFromSidecar sidecar)
      ++ (concatMap aggregateNode items)
      ++ (directoryNameTags path)

    directoryNameTags :: Path -> [String]
    directoryNameTags (Path (name:_)) | fromParents tagsFromDirsCfg > 0 =
      [(prefix tagsFromDirsCfg) ++ name]
    directoryNameTags _ = []

    extractFromSidecar :: Sidecar -> [String]
    extractFromSidecar (Sidecar { tags }) = fromMaybe [] tags
