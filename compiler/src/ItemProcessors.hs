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

module ItemProcessors
  ( ItemProcessor
  , itemFileProcessor
  , ThumbnailProcessor
  , thumbnailFileProcessor
  ) where


import Data.Function ((&))
import Data.Char (toLower)
import System.FilePath (takeExtension)

import Config (Resolution(..))
import Resource (ItemProcessor, ThumbnailProcessor, Thumbnail(..), GalleryItemProps(..))
import Caching (Cache)
import FileProcessors
import Files


data Format =
    PictureFormat
  | PlainTextFormat
  | PortableDocumentFormat
  | VideoFormat
  | AudioFormat
  | Unknown

formatFromPath :: Path -> Format
formatFromPath =
  maybe Unknown ((fromExt . map toLower) . takeExtension) . fileName
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
