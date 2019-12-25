{-# LANGUAGE DuplicateRecordFields, DeriveGeneric, DeriveAnyClass #-}

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


module Input
  ( decodeYamlFile
  , Sidecar, title, date, description, tags
  , InputTree(..), readInputTree
  ) where


import GHC.Generics (Generic)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (find)
import Data.Yaml (ParseException, decodeFileEither)
import Data.Aeson (FromJSON)
import System.FilePath (isExtensionOf, dropExtension)

import Files
import Utils


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
      , sidecar :: Sidecar }
    | InputDir
      { path :: Path
      , thumbnailPath :: Maybe Path
      , items :: [InputTree] }
  deriving Show

data Sidecar = Sidecar
  { title :: Maybe String
  , date :: Maybe String
  , description :: Maybe String
  , tags :: Maybe [String]
  } deriving (Generic, FromJSON, Show)


readInputTree :: AnchoredFSNode -> IO InputTree
readInputTree (AnchoredFSNode anchor root@Dir{}) =
  filterDir (neg isHidden) root & mkDirNode
  where
    mkInputNode :: FSNode -> IO (Maybe InputTree)
    mkInputNode (File path@(filename:pathto)) | ".yaml" `isExtensionOf` filename =
      decodeYamlFile (localPath $ anchor /> path)
      >>= return . InputFile ((dropExtension filename):pathto)
      >>= return . Just
    mkInputNode File{} = return Nothing
    mkInputNode dir@Dir{} = mkDirNode dir >>= return . Just

    mkDirNode :: FSNode -> IO InputTree
    mkDirNode (Dir path items) =
      mapM mkInputNode items
      >>= return . catMaybes
      >>= return . InputDir path (findThumbnail items)
      where
        findThumbnail :: [FSNode] -> Maybe Path
        findThumbnail = (fmap nodePath) . (find matchThumbnail)

        matchThumbnail :: FSNode -> Bool
        matchThumbnail Dir{} = False
        matchThumbnail (File (filename:_)) = (dropExtension filename) == "thumbnail"
