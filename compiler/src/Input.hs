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
  ( decodeYamlFile
  , Sidecar(..)
  , InputTree(..), readInputTree, filterInputTree
  ) where


import GHC.Generics (Generic)
import Control.Exception (Exception, AssertionFailed(..), throw, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Bool (bool)
import Data.List (find, isSuffixOf)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime)
import Data.Yaml (ParseException, decodeFileEither)
import Data.Aeson (FromJSON)
import qualified Data.Map.Strict as Map
import System.FilePath (isExtensionOf, dropExtension)
import System.Directory (doesFileExist, getModificationTime)

import Files


data LoadException = LoadException String ParseException deriving Show
instance Exception LoadException

decodeYamlFile :: (MonadIO m, FromJSON a) => FileName -> m a
decodeYamlFile path =
  liftIO $ Data.Yaml.decodeFileEither path
  >>= either (throwIO . LoadException path) return


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
      inputDir { Input.items = filter cond items & map filterNode }
