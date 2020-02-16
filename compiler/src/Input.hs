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

module Input
  ( decodeYamlFile
  , Sidecar(..)
  , InputTree(..), readInputTree
  ) where


import GHC.Generics (Generic)
import Control.Exception (Exception, AssertionFailed(..), throw, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.Bool (bool)
import Data.List (find)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime)
import Data.Yaml (ParseException, decodeFileEither)
import Data.Aeson (FromJSON)
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
      , sidecar :: Sidecar }
    | InputDir
      { path :: Path
      , modTime :: UTCTime
      , sidecar :: Sidecar
      , dirThumbnailPath :: Maybe Path
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

dirPropFile :: String
dirPropFile = "_directory"

dirSidecar :: Path
dirSidecar = Path [dirPropFile] <.> sidecarExt

readSidecarFile :: FilePath -> IO Sidecar
readSidecarFile filepath =
  doesFileExist filepath
  >>= bool (return Nothing) (decodeYamlFile filepath)
  >>= return . maybe emptySidecar id


readInputTree :: AnchoredFSNode -> IO InputTree
readInputTree (AnchoredFSNode _ File{}) =
  throw $ AssertionFailed "Input directory is a file"
readInputTree (AnchoredFSNode anchor root@Dir{}) = mkDirNode root
  where
    mkInputNode :: FSNode -> IO (Maybe InputTree)
    mkInputNode file@File{path}
      | (not $ isSidecar file) && (not $ isThumbnail file) =
        do
          sidecar <- readSidecarFile $ localPath (anchor /> path <.> sidecarExt)
          modTime <- getModificationTime $ localPath (anchor /> path)
          return $ Just $ InputFile path modTime sidecar
    mkInputNode File{} = return Nothing
    mkInputNode dir@Dir{} = mkDirNode dir >>= return . Just

    mkDirNode :: FSNode -> IO InputTree
    mkDirNode File{} = throw $ AssertionFailed "Input directory is a file"
    mkDirNode Dir{path, items} =
      do
        dirItems <- mapM mkInputNode items
        modTime <- getModificationTime $ localPath (anchor /> path)
        sidecar <- readSidecarFile $ localPath (anchor /> path </> dirSidecar)
        return $ InputDir path modTime sidecar (findThumbnail items) (catMaybes dirItems)

    isSidecar :: FSNode -> Bool
    isSidecar Dir{} = False
    isSidecar File{path} =
      fileName path
      & (maybe False $ isExtensionOf sidecarExt)

    isThumbnail :: FSNode -> Bool
    isThumbnail Dir{} = False
    isThumbnail File{path} =
      fileName path
      & fmap dropExtension
      & (maybe False (dirPropFile ==))

    findThumbnail :: [FSNode] -> Maybe Path
    findThumbnail = (fmap Files.path) . (find isThumbnail)
