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
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Version (showVersion)
import Data.Aeson (ToJSON)
import System.FilePath ((</>))
import System.Directory (canonicalizePath, listDirectory)
import System.Console.CmdArgs

import Compiler
import Files (readDirectory, copyTo, remove)


newtype ViewerConfig = ViewerConfig
  { galleryRoot :: String
  } deriving (Generic, Show, ToJSON)


data Options = Options
  { inputDir :: FilePath
  , outputDir :: FilePath
  , outputIndex :: FilePath
  , galleryConfig :: FilePath
  , rebuildAll :: Bool
  , cleanOutput :: Bool
  , withViewer :: Maybe FilePath
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
  , outputIndex = ""
      &= typFile
      &= name "x"
      &= name "output-index"
      &= explicit
      &= help "Generated gallery index output path (default=<output-dir>/index.json)"
  , galleryConfig = ""
      &= typFile
      &= name "g"
      &= name "gallery-config"
      &= explicit
      &= help "Gallery configuration file (default=<input-dir>/gallery.yaml)"
  , rebuildAll = False
      &= name "r"
      &= name "rebuild-all"
      &= explicit
      &= help "Invalidate cache and recompile everything"
  , cleanOutput = False
      &= name "c"
      &= name "clean-output"
      &= explicit
      &= help "Remove unnecessary files from the output directory"
  , withViewer = Nothing
      &= typDir
      &= opt ("" :: FilePath)
      &= name "w"
      &= name "with-viewer"
      &= explicit
      &= help "Deploy either the bundled or the given static web viewer to the output directory"
  }

  &= summary ("ldgallery v" ++ showVersion version ++ " - a static web gallery generator with tags")
  &= program "ldgallery"
  &= help "Compile a gallery"
  &= helpArg [explicit, name "h", name "help"]
  &= versionArg [explicit, name "version"]


main :: IO ()
main =
  do
    opts <- cmdArgs options
    buildGallery opts

    when (isJust $ withViewer opts) $ do
      viewerDist <- viewerDistPath $ withViewer opts
      deployViewer viewerDist opts

  where
    gallerySubdir :: String
    gallerySubdir = "gallery"

    viewerConfig :: ViewerConfig
    viewerConfig = ViewerConfig (gallerySubdir ++ "/")

    viewerDistPath :: Maybe FilePath -> IO FilePath
    viewerDistPath (Just "") = getDataFileName "viewer"
    viewerDistPath (Just dist) = return dist
    viewerDistPath Nothing = fail "No viewer distribution"

    buildGallery :: Options -> IO ()
    buildGallery opts =
      checkDistinctPaths (inputDir opts) (outputDir opts)
      >>  compileGallery
            (galleryConfig opts)
            (inputDir opts)
            (galleryOutputDir opts)
            (outputIndex opts)
            [outputDir opts]
            (rebuildAll opts)
            (cleanOutput opts)
      where
        checkDistinctPaths :: FilePath -> FilePath -> IO ()
        checkDistinctPaths a b = do
          canonicalA <- canonicalizePath a
          canonicalB <- canonicalizePath b
          when (canonicalA == canonicalB) $ error "Input and output paths refer to the same location."

        galleryOutputDir :: Options -> FilePath
        galleryOutputDir Options{withViewer, outputDir}
          | isJust withViewer = outputDir </> gallerySubdir
          | otherwise = outputDir

    deployViewer :: FilePath -> Options -> IO ()
    deployViewer distPath Options{outputDir, cleanOutput} =
      when cleanOutput (cleanViewerDir outputDir)
      >> copyViewer distPath outputDir
      >> writeJSON (outputDir </> "config.json") viewerConfig

      where
        cleanViewerDir :: FilePath -> IO ()
        cleanViewerDir target =
          listDirectory target
          <&> filter (/= gallerySubdir)
          >>= mapM_ (remove . (target </>))

        copyViewer :: FilePath -> FilePath -> IO ()
        copyViewer dist target =
          putStrLn "Copying viewer webapp"
          >>  readDirectory dist
          >>= copyTo target
