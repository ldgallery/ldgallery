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

import GHC.Generics (Generic)
import Paths_ldgallery_compiler (version, getDataFileName)
import Control.Monad (when)
import Data.Version (showVersion)
import Data.Aeson (ToJSON)
import System.FilePath ((</>))
import System.Directory (canonicalizePath, listDirectory)
import System.Console.CmdArgs

import Compiler
import Files (readDirectory, copyTo, remove)


data ViewerConfig = ViewerConfig
  { galleryRoot :: String
  } deriving (Generic, Show, ToJSON)


data Options = Options
  { inputDir :: String
  , outputDir :: String
  , rebuilAll :: Bool
  , cleanOutput :: Bool
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
  , cleanOutput = False
      &= name "c"
      &= name "clean-output"
      &= explicit
      &= help "Remove unnecessary files from the output directory"
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
    buildGallery opts
    when (withViewer opts) $ do
      when (cleanOutput opts) $ cleanViewerDir (outputDir opts)
      copyViewer (outputDir opts)
      writeViewerConfig (outputDir opts </> "config.json")

  where
    gallerySubdir :: String
    gallerySubdir = "gallery"

    buildGallery :: Options -> IO ()
    buildGallery opts =
      checkDistinctPaths (inputDir opts) (outputDir opts)
      >>  compileGallery
            (inputDir opts)
            (galleryOutputDir opts)
            [outputDir opts]
            (rebuilAll opts)
            (cleanOutput opts)
      where
        checkDistinctPaths a b = do
          canonicalA <- canonicalizePath a
          canonicalB <- canonicalizePath b
          when (canonicalA == canonicalB) $ error "Input and output paths refer to the same location."

    galleryOutputDir :: Options -> FilePath
    galleryOutputDir opts =
      if withViewer opts then outputBase </> gallerySubdir else outputBase
      where outputBase = outputDir opts

    cleanViewerDir :: FilePath -> IO ()
    cleanViewerDir target =
      listDirectory target
      >>= return . filter (/= gallerySubdir)
      >>= mapM_ remove . map (target </>)

    copyViewer :: FilePath -> IO ()
    copyViewer target =
      putStrLn "Copying viewer webapp"
      >>  getDataFileName "viewer"
      >>= readDirectory
      >>= copyTo target

    writeViewerConfig :: FilePath -> IO ()
    writeViewerConfig fileName = writeJSON fileName $ ViewerConfig (gallerySubdir ++ "/")
