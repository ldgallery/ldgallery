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

module Main where

import Paths_ldgallery_compiler (version, getDataFileName)
import Data.Version (showVersion)
import System.FilePath ((</>))
import System.Console.CmdArgs

import Compiler
import Files (readDirectory, copyTo)


data Options = Options
  { inputDir :: String
  , outputDir :: String
  , rebuilAll :: Bool
  , withViewer :: Bool
  } deriving (Show, Data, Typeable)

options :: Options
options = Options
  { inputDir = "./"
      &= typDir
      &= name "i"
      &= name "input-dir"
      &= explicit
      &= help "Gallery source directory (default=./)"
  , outputDir = "./out"
      &= typDir
      &= name "o"
      &= name "output-dir"
      &= explicit
      &= help "Generated gallery output path (default=./out)"
  , rebuilAll = False
      &= name "r"
      &= name "rebuild-all"
      &= explicit
      &= help "Invalidate cache and recompile everything"
  , withViewer = False
      &= name "w"
      &= name "with-viewer"
      &= explicit
      &= help "Include the static web viewer in the output"
  }

  &= summary ("ldgallery v" ++ (showVersion version) ++ " - a static web gallery generator with tags")
  &= program "ldgallery"
  &= help "Compile a gallery"
  &= helpArg [explicit, name "h", name "help"]
  &= versionArg [explicit, name "version"]


main :: IO ()
main =
  do
    opts <- cmdArgs options
    compileGallery (inputDir opts) (galleryOutputDir "gallery" opts) (rebuilAll opts)
    if (withViewer opts) then copyViewer (outputDir opts) else noop

  where
    galleryOutputDir :: FilePath -> Options -> FilePath
    galleryOutputDir gallerySubdir opts =
      if withViewer opts then outputBase </> gallerySubdir else outputBase
      where outputBase = outputDir opts

    copyViewer :: FilePath -> IO ()
    copyViewer target =
      putStrLn "Copying viewer webapp"
      >>  getDataFileName "viewer"
      >>= readDirectory
      >>= copyTo target

    noop :: IO ()
    noop = return ()
