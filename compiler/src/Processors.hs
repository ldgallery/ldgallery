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


import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Function ((&))
import Data.Char (toLower)
import Text.Read (readMaybe)

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


data Format =
    PictureFormat
  | PlainTextFormat
  | PortableDocumentFormat
  | VideoFormat
  | AudioFormat
  | Unknown

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
      ".txt" -> PlainTextFormat
      ".md" -> PlainTextFormat -- TODO: handle markdown separately
      ".pdf" -> PortableDocumentFormat
      ".wav" -> AudioFormat
      ".oga" -> AudioFormat
      ".ogg" -> AudioFormat
      ".spx" -> AudioFormat
      ".opus" -> AudioFormat
      ".flac" -> AudioFormat
      ".m4a" -> AudioFormat
      ".mp3" -> AudioFormat
      ".ogv" -> VideoFormat
      ".ogx" -> VideoFormat
      ".webm" -> VideoFormat
      ".mkv" -> VideoFormat
      ".mp4" -> VideoFormat
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
    when isDir $ removePathForcibly outputPath

    fileExists <- doesFileExist outputPath
    if fileExists then
      do
        needUpdate <- isOutdated True inputPath outputPath
        if needUpdate then update else skip
    else
      update

  where
    update = processor inputPath outputPath
    skip = putStrLn $ "Skipping:\t" ++ outputPath


resourceAt :: FilePath -> Path -> IO Resource
resourceAt fsPath resPath = getModificationTime fsPath >>= return . Resource resPath

getImageResolution :: FilePath -> IO Resolution
getImageResolution fsPath =
  readProcess "magick" ["identify", "-format", "%w %h", firstFrame] []
  >>= parseResolution . break (== ' ')
  where
    firstFrame :: FilePath
    firstFrame = fsPath ++ "[0]"

    parseResolution :: (String, String) -> IO Resolution
    parseResolution (widthString, heightString) =
      case (readMaybe widthString, readMaybe heightString) of
        (Just w, Just h) -> return $ Resolution w h
        _ -> throwIO $ ProcessingException fsPath "Unable to read image resolution."

getPictureProps :: ItemDescriber
getPictureProps fsPath resource =
      getImageResolution fsPath
  >>= return . Picture resource


type ItemDescriber =
     FilePath
  -> Resource
  -> IO GalleryItemProps


type ItemFileProcessor =
     FileName        -- ^ Input base path
  -> FileName        -- ^ Output base path
  -> FileName        -- ^ Output class (subdir)
  -> ItemProcessor

itemFileProcessor :: Maybe Resolution -> Cache -> ItemFileProcessor
itemFileProcessor maxResolution cached inputBase outputBase resClass inputRes =
      cached processor inPath outPath
  >>  resourceAt outPath relOutPath
  >>= descriptor outPath
  where
    relOutPath = resClass /> inputRes
    inPath = localPath $ inputBase /> inputRes
    outPath = localPath $ outputBase /> relOutPath
    (processor, descriptor) = processorFor (formatFromPath inputRes) maxResolution

    processorFor :: Format -> Maybe Resolution -> (FileProcessor, ItemDescriber)
    processorFor PictureFormat (Just maxRes) = (resizePictureUpTo maxRes, getPictureProps)
    processorFor PictureFormat Nothing = (copyFileProcessor, getPictureProps)
    processorFor PlainTextFormat _ = (copyFileProcessor, const $ return . PlainText)
    processorFor PortableDocumentFormat _ = (copyFileProcessor, const $ return . PDF)
    processorFor VideoFormat _ = (copyFileProcessor, const $ return . Video)
    processorFor AudioFormat _ = (copyFileProcessor, const $ return . Audio)
    -- TODO: handle video reencoding and others?
    processorFor Unknown _ = (copyFileProcessor, const $ return . Other)


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
