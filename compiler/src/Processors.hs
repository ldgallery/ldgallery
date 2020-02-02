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

module Processors
  ( Resolution(..)
  , ItemFileProcessor, itemFileProcessor
  , ThumbnailFileProcessor, thumbnailFileProcessor
  , skipCached, withCached
  ) where


import Control.Exception (Exception)
import Data.Function ((&))
import Data.Char (toLower)
import Data.List (break)

import System.Directory hiding (copyFile)
import qualified System.Directory
import System.FilePath

import System.Process (callProcess, readProcess)

import Resource
  ( ItemProcessor, ThumbnailProcessor
  , GalleryItemProps(..), Resolution(..), Resource(..), Thumbnail(..) )

import Files


data ProcessingException = ProcessingException FilePath String deriving Show
instance Exception ProcessingException


-- TODO: handle video, music, text...
data Format = PictureFormat | Unknown

formatFromPath :: Path -> Format
formatFromPath =
  maybe Unknown fromExt
  . fmap (map toLower)
  . fmap takeExtension
  . fileName
  where
    fromExt :: String -> Format
    fromExt ext = case ext of
      ".bmp" -> PictureFormat
      ".jpg" -> PictureFormat
      ".jpeg" -> PictureFormat
      ".png" -> PictureFormat
      ".tiff" -> PictureFormat
      ".hdr" -> PictureFormat
      ".gif" -> PictureFormat
      _ -> Unknown


type FileProcessor =
     FileName        -- ^ Input path
  -> FileName        -- ^ Output path
  -> IO ()

copyFileProcessor :: FileProcessor
copyFileProcessor inputPath outputPath =
  (putStrLn $ "Copying:\t" ++ outputPath)
  >> ensureParentDir (flip System.Directory.copyFile) outputPath inputPath

resizePictureUpTo :: Resolution -> FileProcessor
resizePictureUpTo maxResolution inputPath outputPath =
  (putStrLn $ "Generating:\t" ++ outputPath)
  >> ensureParentDir (flip resize) outputPath inputPath
  where
    maxSize :: Resolution -> String
    maxSize Resolution{width, height} = show width ++ "x" ++ show height ++ ">"

    resize :: FileName -> FileName -> IO ()
    resize input output = callProcess "magick"
      [ input
      , "-auto-orient"
      , "-resize", maxSize maxResolution
      , output ]


type Cache = FileProcessor -> FileProcessor

skipCached :: Cache
skipCached processor inputPath outputPath =
  removePathForcibly outputPath
  >> processor inputPath outputPath

withCached :: Cache
withCached processor inputPath outputPath =
  do
    isDir <- doesDirectoryExist outputPath
    if isDir then removePathForcibly outputPath else noop

    fileExists <- doesFileExist outputPath
    if fileExists then
      do
        needUpdate <- isOutdated True inputPath outputPath
        if needUpdate then update else skip
    else
      update

  where
    noop = return ()
    update = processor inputPath outputPath
    skip = putStrLn $ "Skipping:\t" ++ outputPath


resourceAt :: FilePath -> Path -> IO Resource
resourceAt fsPath resPath = getModificationTime fsPath >>= return . Resource resPath

getImageResolution :: FilePath -> IO Resolution
getImageResolution fsPath =
  readProcess "identify" ["-format", "%w %h", fsPath] []
  >>= return . break (== ' ')
  >>= return . \(w, h) -> Resolution (read w) (read h)


type ItemFileProcessor =
     FileName        -- ^ Input base path
  -> FileName        -- ^ Output base path
  -> FileName        -- ^ Output class (subdir)
  -> ItemProcessor

itemFileProcessor :: Maybe Resolution -> Cache -> ItemFileProcessor
itemFileProcessor maxResolution cached inputBase outputBase resClass inputRes =
  cached processor inPath outPath
  >> resourceAt outPath relOutPath
  >>= return . props
  where
    relOutPath = resClass /> inputRes
    inPath = localPath $ inputBase /> inputRes
    outPath = localPath $ outputBase /> relOutPath
    (processor, props) = processorFor maxResolution $ formatFromPath inputRes

    processorFor :: Maybe Resolution -> Format -> (FileProcessor, Resource -> GalleryItemProps)
    processorFor (Just maxRes) PictureFormat = (resizePictureUpTo maxRes, Picture)
    processorFor Nothing PictureFormat = (copyFileProcessor, Picture)
    processorFor _ Unknown = (copyFileProcessor, Other) -- TODO: handle video reencoding and others?


type ThumbnailFileProcessor =
     FileName        -- ^ Input base path
  -> FileName        -- ^ Output base path
  -> FileName        -- ^ Output class (subdir)
  -> ThumbnailProcessor

thumbnailFileProcessor :: Resolution -> Cache -> ThumbnailFileProcessor
thumbnailFileProcessor maxRes cached inputBase outputBase resClass inputRes =
  cached <$> processorFor (formatFromPath inputRes)
  & process
  where
    relOutPath = resClass /> inputRes
    inPath = localPath $ inputBase /> inputRes
    outPath = localPath $ outputBase /> relOutPath

    process :: Maybe FileProcessor -> IO (Maybe Thumbnail)
    process Nothing = return Nothing
    process (Just proc) =
      do
        proc inPath outPath
        resource <- resourceAt outPath relOutPath
        resolution <- getImageResolution outPath
        return $ Just $ Thumbnail resource resolution

    processorFor :: Format -> Maybe FileProcessor
    processorFor PictureFormat = Just $ resizePictureUpTo maxRes
    processorFor _ = Nothing
