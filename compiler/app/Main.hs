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
    DeriveDataTypeable
#-}

module Main where

import Paths_ldgallery_compiler (version)
import Data.Version (showVersion)
import System.Console.CmdArgs
import Compiler


data Options = Options
  { inputDir :: String
  , outputDir :: String
  , rebuild :: Bool
  } deriving (Show, Data, Typeable)

options = Options
  { inputDir = "./"
      &= typDir
      &= help "Gallery source directory (default=./)"
  , outputDir = "./out"
      &= typDir
      &= help "Generated gallery output path (default=./out)"
  , rebuild = False
      &= help "Invalidate cache and recompile everything"
  }
    &= summary ("ldgallery v" ++ (showVersion version) ++ " - a static gallery generator with tags")
    &= program "ldgallery"
    &= help "Compile a gallery"
    &= helpArg [explicit, name "h", name "help"]
    &= versionArg [explicit, name "v", name "version"]


main :: IO ()
main =
  do
    opts <- cmdArgs options
    compileGallery (inputDir opts) (outputDir opts) (rebuild opts)
