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

{-# LANGUAGE
    DuplicateRecordFields
  , DeriveGeneric
#-}

module Files
  ( FileName, LocalPath, WebPath, Path
  , (</>), (</), (/>), localPath, webPath
  , FSNode(..), AnchoredFSNode(..)
  , nodePath, nodeName, isHidden, flattenDir, filterDir, readDirectory
  , ensureParentDir, remove
  ) where


import Control.Monad (filterM, mapM)
import Data.Bool (bool)
import Data.List (isPrefixOf, length, deleteBy)
import Data.Function ((&))
import System.Directory
  ( doesDirectoryExist
  , listDirectory
  , createDirectoryIfMissing
  , removePathForcibly )

import qualified System.FilePath
import qualified System.FilePath.Posix


type FileName = String
type LocalPath = String
type WebPath = String

 -- | Reversed path component list
type Path = [FileName]

(</>) :: Path -> Path -> Path
l </> r = r ++ l

(</) :: Path -> FileName -> Path
path </ file = file:path

(/>) :: FileName -> Path -> Path
file /> path = path ++ [file]

localPath :: Path -> LocalPath
localPath = System.FilePath.joinPath . reverse

webPath :: Path -> WebPath
webPath = System.FilePath.Posix.joinPath . reverse


data FSNode = File Path | Dir Path [FSNode] deriving Show
data AnchoredFSNode = AnchoredFSNode
  { anchor :: LocalPath
  , root :: FSNode } deriving Show

nodePath :: FSNode -> Path
nodePath (File path) = path
nodePath (Dir path _) = path

nodeName :: FSNode -> FileName
nodeName = head . nodePath

isHidden :: FSNode -> Bool
isHidden node = "." `isPrefixOf` filename && length filename > 1
  where filename = nodeName node

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
readDirectory root = mkNode [] >>= return . AnchoredFSNode root
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
