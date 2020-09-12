/* ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2020  Guillaume FOUET
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

declare namespace Gallery {
  type ItemSortStr = "name_asc" | "date_desc";

  interface Config {
    galleryRoot: string;
    galleryIndex?: string;
    initialItemSort?: ItemSortStr;
    initialTagDisplayLimit?: number;
  }

  interface GalleryProperties {
    galleryTitle: string;
    tagCategories: RawTag[];
  }
  interface Index {
    properties: GalleryProperties;
    tree: Directory;
  }

  interface Other extends Item {
    properties: OtherProperties;
  }
  interface Picture extends Item {
    properties: PictureProperties;
  }
  interface PlainText extends Item {
    properties: PlainTextProperties;
  }
  interface PDF extends Item {
    properties: PDFProperties;
  }
  interface Video extends Item {
    properties: VideoProperties;
  }
  interface Audio extends Item {
    properties: AudioProperties;
  }
  interface Directory extends Item {
    properties: DirectoryProperties;
  }
  interface Item {
    title: string;
    datetime: string;
    description: string;
    tags: RawTag[];
    path: string;
    thumbnail?: Thumbnail;
    properties:
      | OtherProperties
      | PictureProperties
      | PlainTextProperties
      | PDFProperties
      | VideoProperties
      | AudioProperties
      | DirectoryProperties;
  }
  interface Resolution {
    width: number;
    height: number;
  }
  interface OtherProperties {
    type: import("./ItemType").ItemType.OTHER;
    resource: string;
  }
  interface PictureProperties {
    type: import("./ItemType").ItemType.PICTURE;
    resource: string;
    resolution: Resolution;
  }
  interface PlainTextProperties {
    type: import("./ItemType").ItemType.PLAINTEXT;
    resource: string;
  }
  interface PDFProperties {
    type: import("./ItemType").ItemType.PDF;
    resource: string;
  }
  interface VideoProperties {
    type: import("./ItemType").ItemType.VIDEO;
    resource: string;
  }
  interface AudioProperties {
    type: import("./ItemType").ItemType.AUDIO;
    resource: string;
  }
  interface DirectoryProperties {
    type: import("./ItemType").ItemType.DIRECTORY;
    items: Item[];
  }
  interface Thumbnail {
    resource: string;
    resolution: Resolution;
  }
  type RawTag = string;
}
