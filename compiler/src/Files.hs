{-# LANGUAGE DuplicateRecordFields, DeriveGeneric #-}

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


module Files
  ( FileName, LocalPath, WebPath, Path
  , (</>), (</), (/>), localPath, webPath
  , FSNode(..), AnchoredFSNode(..)
  , nodePath, nodeName, isHidden, flatten, filterDir, readDirectory
  ) where


import Control.Monad (filterM, mapM)
import Data.Bool (bool)
import Data.List (isPrefixOf, length, deleteBy)
import Data.Function ((&))
import System.Directory (doesDirectoryExist, listDirectory)

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

flatten :: FSNode -> [FSNode]
flatten file@(File _) = [file]
flatten dir@(Dir _ childs) = dir:(concatMap flatten childs)

-- | Filters a dir tree. The root is always returned.
filterDir :: (FSNode -> Bool) -> FSNode -> FSNode
filterDir _ file@(File _) = file
filterDir cond (Dir path childs) =
  filter cond childs & map (filterDir cond) & Dir path

readDirectory :: LocalPath -> IO AnchoredFSNode
readDirectory root = mkNode [""] >>= return . AnchoredFSNode root
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
