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
    RecordWildCards
  , ApplicativeDo
#-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Compiler

data Args = Args
  { inputDir :: String
  , outputDir :: String
  , rebuild :: Bool }

args :: Parser Args
args = Args
  <$> strOption
     ( long "input"
    <> short 'i'
    <> metavar "SOURCE DIR"
    <> value "./"
    <> showDefault
    <> help "Gallery source directory" )
  <*> strOption
     ( long "output"
    <> short 'o'
    <> metavar "OUTPUT DIR"
    <> value "./out"
    <> showDefault
    <> help "Generated gallery output path" )
  <*> switch
     ( long "rebuild"
    <> short 'r'
    <> help "Invalidate cache and recompile everything" )

main :: IO ()
main =
  do
    options <- execParser opts
    compileGallery (inputDir options) (outputDir options) (rebuild options)

  where
    opts = info (args <**> helper)
       ( fullDesc
      <> progDesc "Compile a gallery"
      <> header "ldgallery - a static gallery generator with tags" )
