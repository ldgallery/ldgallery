-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2022  Pacien TRAN-GIRARD
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

module FileProcessors
  ( FileProcessor
  , transformThenDescribe
  , copyResource
  , noopProcessor
  , FileTransformer
  , copyFileProcessor
  , resizePictureUpTo
  , resourceAt
  , getImageResolution
  , FileDescriber
  , getResProps
  , getPictureProps
  , getThumbnailProps
  ) where


import Control.Exception (Exception, throwIO)
import System.Process (readProcess, callProcess)
import Text.Read (readMaybe)

import System.Directory (getModificationTime)
import qualified System.Directory

import Config (Resolution(..))
import Resource (Resource(..), GalleryItemProps(..), Thumbnail(..))
import Files


data ProcessingException = ProcessingException FilePath String deriving Show
instance Exception ProcessingException

type FileProcessor a =
     Path     -- ^ Item path
  -> Path     -- ^ Target resource path
  -> FilePath -- ^ Filesystem input path
  -> FilePath -- ^ Filesystem output path
  -> IO a

transformThenDescribe :: FileTransformer -> FileDescriber a -> FileProcessor a
transformThenDescribe transformer describer _itemPath resPath fsInPath fsOutPath =
  transformer fsInPath fsOutPath >> describer resPath fsOutPath

copyResource :: (Resource -> a) -> FileProcessor a
copyResource resPropConstructor =
  transformThenDescribe copyFileProcessor (getResProps resPropConstructor)

noopProcessor :: FileProcessor (Maybe a)
noopProcessor _ _ _ _ = return Nothing


type FileTransformer =
     FileName -- ^ Input path
  -> FileName -- ^ Output path
  -> IO ()

copyFileProcessor :: FileTransformer
copyFileProcessor inputPath outputPath =
  putStrLn ("Copying:\t" ++ outputPath)
  >> ensureParentDir (flip System.Directory.copyFile) outputPath inputPath

resizePictureUpTo :: Resolution -> FileTransformer
resizePictureUpTo maxResolution inputPath outputPath =
  putStrLn ("Processing:\t" ++ outputPath)
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


type FileDescriber a =
      Path     -- ^ Target resource path
   -> FilePath -- ^ Filesystem path
   -> IO a

getImageResolution :: FilePath -> IO Resolution
getImageResolution fsPath =
  readProcess "magick"
    [ "identify"
    , "-ping"
    , "-format", "%[orientation] %w %h"
    , firstFrame
    ] []
  >>= parseOutput . words

  where
    firstFrame :: FilePath
    firstFrame = fsPath ++ "[0]"

    -- Flip the dimensions when necessary according to the metadata.
    -- ImageMagick's `-auto-orient` flag does the same, but isn't compatible
    -- with `-ping` and causes the whole image file to be loaded.
    parseOutput :: [String] -> IO Resolution
    parseOutput ["RightTop", w, h] = parseResolution (h, w)
    parseOutput ["LeftBottom", w, h] = parseResolution (h, w)
    parseOutput [_, w, h] = parseResolution (w, h)
    parseOutput _ = throwIO failedRead

    parseResolution :: (String, String) -> IO Resolution
    parseResolution (widthString, heightString) =
      case (readMaybe widthString, readMaybe heightString) of
        (Just w, Just h) -> return $ Resolution w h
        _ -> throwIO failedRead

    failedRead :: ProcessingException
    failedRead = ProcessingException fsPath "Unable to read image resolution."

resourceAt :: FileDescriber Resource
resourceAt resPath fsPath = Resource resPath <$> getModificationTime fsPath

getResProps :: (Resource -> a) -> FileDescriber a
getResProps resPropsConstructor resPath fsPath =
  resPropsConstructor <$> resourceAt resPath fsPath

getPictureProps :: FileDescriber GalleryItemProps
getPictureProps resPath fsPath =
  Picture <$> resourceAt resPath fsPath <*> getImageResolution fsPath

getThumbnailProps :: FileDescriber (Maybe Thumbnail)
getThumbnailProps resPath fsPath =
  Just <$> (Thumbnail <$> resourceAt resPath fsPath <*> getImageResolution fsPath)
