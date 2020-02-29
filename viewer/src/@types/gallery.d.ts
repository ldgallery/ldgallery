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
    interface Config {
        galleryRoot: string,
    }

    interface GalleryProperties {
        galleryTitle: string,
        tagCategories: RawTag[]
    }
    interface Index {
        properties: GalleryProperties,
        tree: Directory
    }

    interface Other extends Item {
        properties: OtherProperties,
    }
    interface Picture extends Item {
        properties: PictureProperties,
    }
    interface Directory extends Item {
        properties: DirectoryProperties,
    }
    interface Item {
        title: string,
        datetime: string,
        description: string,
        tags: RawTag[],
        path: string,
        thumbnail?: Thumbnail
        properties: OtherProperties | PictureProperties | DirectoryProperties,
    }
    interface OtherProperties {
        type: "other",
    }
    interface PictureProperties {
        type: "picture",
        resource: string,
    }
    interface DirectoryProperties {
        type: "directory",
        items: Item[]
    }
    interface Thumbnail {
        resource: string,
        resolution: {
            width: number,
            height: number,
        }
    }
    type RawTag = string;
    type ItemType = "other" | "picture" | "directory";
}
