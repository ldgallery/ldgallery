/* ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2022  Guillaume FOUET
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
*/

import { ItemType } from './itemType';
import { SplashScreenConfig } from './splashscreen';

export type ItemSortStr = 'title_asc' | 'date_asc' | 'date_desc';

export interface Config {
  galleryRoot: string;
  galleryIndex?: string;
  initialItemSort?: ItemSortStr;
  initialTagDisplayLimit?: number;
  splashScreen?: SplashScreenConfig;
}

// ---

export interface Resolution {
  width: number;
  height: number;
}
export interface Thumbnail {
  resource: string;
  resolution: Resolution;
}
export type RawTag = string;

// ---

export interface Downloadable {
  resource: string;
  type: ItemType; // unknown
}
export interface OtherProperties extends Downloadable {
  type: ItemType.OTHER;
}
export interface PictureProperties extends Downloadable {
  type: ItemType.PICTURE;
  resolution: Resolution;
}
export interface PlainTextProperties extends Downloadable {
  type: ItemType.PLAINTEXT;
}
export interface MarkdownProperties extends Downloadable {
  type: ItemType.MARKDOWN;
}
export interface PDFProperties extends Downloadable {
  type: ItemType.PDF;
}
export interface VideoProperties extends Downloadable {
  type: ItemType.VIDEO;
}
export interface AudioProperties extends Downloadable {
  type: ItemType.AUDIO;
}
export interface DirectoryProperties {
  type: ItemType.DIRECTORY;
  // eslint-disable-next-line no-use-before-define
  items: Item[];
}

// ---

export interface Item {
  title: string;
  datetime: string;
  description: string;
  tags: RawTag[];
  path: string;
  thumbnail?: Thumbnail;
  properties:
    | Downloadable
    | DirectoryProperties;
}
export interface DownloadableItem extends Item { // Special unknown item type
  properties: Downloadable;
}
export interface OtherItem extends Item {
  properties: OtherProperties;
}
export interface PictureItem extends Item {
  properties: PictureProperties;
}
export interface PlainTextItem extends Item {
  properties: PlainTextProperties;
}
export interface MarkdownItem extends Item {
  properties: MarkdownProperties;
}
export interface PDFItem extends Item {
  properties: PDFProperties;
}
export interface VideoItem extends Item {
  properties: VideoProperties;
}
export interface AudioItem extends Item {
  properties: AudioProperties;
}
export interface DirectoryItem extends Item {
  properties: DirectoryProperties;
}

// ---

export interface IndexProperties {
  galleryTitle: string;
  tagCategories: RawTag[];
}
export interface Index {
  properties: IndexProperties;
  tree: DirectoryItem;
}
