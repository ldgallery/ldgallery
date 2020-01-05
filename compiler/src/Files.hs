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
  , NamedFieldPuns
#-}

module Files
  ( FileName, LocalPath, WebPath, Path
  , (</>), (</), (/>), (<.>)
  , fileName, subPaths, pathLength
  , localPath, webPath
  , FSNode(..), AnchoredFSNode(..)
  , nodeName, isHidden, flattenDir, filterDir
  , readDirectory, copyTo
  , ensureParentDir, remove, isOutdated
  ) where


import Control.Monad (filterM, mapM)
import Data.Bool (bool)
import Data.List (isPrefixOf, length, deleteBy, subsequences)
import Data.Function ((&))
import Data.Text (pack)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON

import System.Directory
  ( doesDirectoryExist
  , doesPathExist
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
data Path = Path [FileName] deriving Show

instance ToJSON Path where
  toJSON = JSON.String . pack . webPath

instance Eq Path where
  (Path left) == (Path right) = left == right

(</>) :: Path -> Path -> Path
(Path l) </> (Path r) = Path (r ++ l)

(</) :: Path -> FileName -> Path
(Path path) </ file = Path (file:path)

(/>) :: FileName -> Path -> Path
file /> (Path path) = Path (path ++ [file])

(<.>) :: Path -> String -> Path
(Path (filename:pathto)) <.> ext =
  Path $ System.FilePath.addExtension filename ext : pathto

fileName :: Path -> Maybe FileName
fileName (Path (name:_)) = Just name
fileName _ = Nothing

subPaths :: Path -> [Path]
subPaths (Path path) = map Path $ subsequences path

pathLength :: Path -> Int
pathLength (Path path) = Data.List.length path

localPath :: Path -> LocalPath
localPath (Path path) = System.FilePath.joinPath $ reverse path

webPath :: Path -> WebPath
webPath (Path path) = System.FilePath.Posix.joinPath $ reverse path


data FSNode =
    File { path :: Path }
  | Dir { path :: Path, items :: [FSNode] }
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
flattenDir file@(File _) = [file]
flattenDir dir@(Dir _ items) = dir:(concatMap flattenDir items)

-- | Filters a dir tree. The root is always returned.
filterDir :: (FSNode -> Bool) -> AnchoredFSNode -> AnchoredFSNode
filterDir cond (AnchoredFSNode anchor root) =
  AnchoredFSNode anchor (filterNode root)
  where
  filterNode :: FSNode -> FSNode
  filterNode file@(File _) = file
  filterNode (Dir path items) =
    filter cond items & map filterNode & Dir path

readDirectory :: LocalPath -> IO AnchoredFSNode
readDirectory root = mkNode (Path []) >>= return . AnchoredFSNode root
  where
    mkNode :: Path -> IO FSNode
    mkNode path =
      (doesDirectoryExist $ localPath (root /> path))
      >>= bool (mkFileNode path) (mkDirNode path)

    mkFileNode :: Path -> IO FSNode
    mkFileNode path = return $ File path

    mkDirNode :: Path -> IO FSNode
    mkDirNode path =
      (listDirectory $ localPath (root /> path))
      >>= mapM (mkNode . ((</) path))
      >>= return . Dir path

copyTo :: FilePath -> AnchoredFSNode -> IO ()
copyTo target AnchoredFSNode{anchor, root} = copyNode root
  where
    copyNode :: FSNode -> IO ()
    copyNode (File path) =
      copyFile (localPath $ anchor /> path) (localPath $ target /> path)

    copyNode (Dir path items) =
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
