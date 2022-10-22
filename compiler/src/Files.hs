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

module Files
  ( FileName, LocalPath, WebPath, Path(..)
  , (</>), (</), (/>), (<.>)
  , fileName, subPaths, pathLength
  , localPath, webPath, fromWebPath
  , FSNode(..), AnchoredFSNode(..)
  , nodeName, isHidden, flattenDir, filterDir
  , readDirectory, copyTo
  , ensureParentDir, remove, isOutdated
  , decodeYamlFile
  ) where


import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (isPrefixOf, length, sortOn)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (pack, unpack)
import Data.Yaml (ParseException, decodeFileEither)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as JSON

import System.Directory
  ( doesDirectoryExist
  , doesPathExist
  , canonicalizePath
  , getModificationTime
  , listDirectory
  , createDirectoryIfMissing
  , removePathForcibly
  , copyFile )

import qualified System.FilePath
import qualified System.FilePath.Posix


type FileName = String
type LocalPath = String
type WebPath = String

-- | Reversed path component list
newtype Path = Path [FileName] deriving Show

instance ToJSON Path where
  toJSON = JSON.String . pack . webPath

instance FromJSON Path where
  parseJSON = JSON.withText "Path" (return . fromWebPath . unpack)

instance Eq Path where
  left == right = webPath left == webPath right

(</>) :: Path -> Path -> Path
(Path l) </> (Path r) = Path (r ++ l)

(</) :: Path -> FileName -> Path
(Path path) </ file = Path (file:path)

(/>) :: FileName -> Path -> Path
file /> (Path path) = Path (path ++ [file])

(<.>) :: Path -> String -> Path
(Path (filename:pathto)) <.> ext =
  Path $ System.FilePath.addExtension filename ext : pathto
(Path _) <.> ext = Path [ext]

fileName :: Path -> Maybe FileName
fileName (Path (name:_)) = Just name
fileName _ = Nothing

subPaths :: Path -> [Path]
subPaths (Path path) = map Path $ subpaths path
  where
    subpaths [] = []
    subpaths full@(_:r) = full : subpaths r

pathLength :: Path -> Int
pathLength (Path path) = Data.List.length path

localPath :: Path -> LocalPath
localPath (Path path) = System.FilePath.joinPath $ reverse path

webPath :: Path -> WebPath
webPath (Path path) = System.FilePath.Posix.joinPath $ reverse path

fromWebPath :: WebPath -> Path
fromWebPath = Path . reverse . System.FilePath.Posix.splitDirectories


data FSNode =
    File
      { path :: Path
      , canonicalPath :: FilePath }
  | Dir
      { path :: Path
      , canonicalPath :: FilePath
      , items :: [FSNode] }
  deriving Show

data AnchoredFSNode = AnchoredFSNode
  { anchor :: LocalPath
  , root :: FSNode }
  deriving Show

nodeName :: FSNode -> Maybe FileName
nodeName = fileName . path

isHidden :: FSNode -> Bool
isHidden = hiddenName . nodeName
  where
    hiddenName :: Maybe FileName -> Bool
    hiddenName Nothing = False
    hiddenName (Just filename) = "." `isPrefixOf` filename && length filename > 1

-- | DFS with intermediate dirs first.
flattenDir :: FSNode -> [FSNode]
flattenDir file@File{} = [file]
flattenDir dir@Dir{items} = dir:concatMap flattenDir items

-- | Filters a dir tree. The root is always returned.
filterDir :: (FSNode -> Bool) -> AnchoredFSNode -> AnchoredFSNode
filterDir cond (AnchoredFSNode anchor root) =
  AnchoredFSNode anchor (filterNode root)
  where
  filterNode :: FSNode -> FSNode
  filterNode file@File{} = file
  filterNode Dir{path, canonicalPath, items} =
    filter cond items & map filterNode & Dir path canonicalPath

readDirectory :: LocalPath -> IO AnchoredFSNode
readDirectory root = AnchoredFSNode root <$> mkNode (Path [])
  where
    mkNode :: Path -> IO FSNode
    mkNode path =
      do
        let relPath = localPath (root /> path)
        canonicalPath <- canonicalizePath relPath
        isDir <- doesDirectoryExist relPath
        if isDir then
          mkDirNode path canonicalPath
        else
          mkFileNode path canonicalPath

    mkFileNode :: Path -> FilePath -> IO FSNode
    mkFileNode path canonicalPath = return $ File path canonicalPath

    mkDirNode :: Path -> FilePath -> IO FSNode
    mkDirNode path canonicalPath =
      listDirectory (localPath (root /> path))
      >>= mapM (mkNode . (path </))
      <&> sortOn nodeName
      <&> Dir path canonicalPath

copyTo :: FilePath -> AnchoredFSNode -> IO ()
copyTo target AnchoredFSNode{anchor, root} = copyNode root
  where
    copyNode :: FSNode -> IO ()
    copyNode File{path} =
      copyFile (localPath $ anchor /> path) (localPath $ target /> path)

    copyNode Dir{path, items} =
      createDirectoryIfMissing True (localPath $ target /> path)
      >> mapM_ copyNode items

ensureParentDir :: (FileName -> a -> IO b) -> FileName -> a -> IO b
ensureParentDir writer filePath a =
  createDirectoryIfMissing True parentDir
  >> writer filePath a
  where
    parentDir = System.FilePath.dropFileName filePath

remove :: FileName -> IO ()
remove path =
  do
    putStrLn $ "Removing:\t" ++ path
    removePathForcibly path

isOutdated :: Bool -> FilePath -> FilePath -> IO Bool
isOutdated onMissingTarget ref target =
  do
    refExists <- doesPathExist ref
    targetExists <- doesPathExist target
    if refExists && targetExists then
      do
        refTime <- getModificationTime ref
        targetTime <- getModificationTime target
        return (targetTime < refTime)
    else
      return onMissingTarget


data LoadException = LoadException String ParseException deriving Show
instance Exception LoadException

decodeYamlFile :: (MonadIO m, FromJSON a) => FileName -> m a
decodeYamlFile path =
  liftIO $ Data.Yaml.decodeFileEither path
  >>= either (throwIO . LoadException path) return
