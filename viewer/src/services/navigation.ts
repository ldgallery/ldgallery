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

import { DirectoryItem, DownloadableItem, Item } from '@/@types/gallery';
import { ItemType } from '@/@types/itemType';
import { faFile, faFileAlt, faFileAudio, faFilePdf, faFileVideo, faFolder, faHome, faImage, IconDefinition } from '@fortawesome/free-solid-svg-icons';
import { isDirectory } from './itemGuards';

const ICON_BY_TYPE: Record<ItemType, IconDefinition> = {
  directory: faFolder,
  picture: faImage,
  plaintext: faFileAlt,
  markdown: faFileAlt,
  pdf: faFilePdf,
  video: faFileVideo,
  audio: faFileAudio,
  other: faFile,
};

// ---

export const useNavigation = () => {
  // Searches for an item by path from a root item (navigation)
  function searchCurrentItemPath(root: Item, path: string): Item[] {
    if (path === root.path) return [root];
    if (isDirectory(root) && path.startsWith(root.path)) {
      const itemChain = root.properties.items
        .map(item => searchCurrentItemPath(item, path))
        .find(itemChain => itemChain.length > 0);
      if (itemChain) return [root, ...itemChain];
    }
    return [];
  }

  // Normalize a string to lowercase, no-accents
  function normalize(value: string) {
    return value
      .normalize('NFD')
      .replace(/[\u0300-\u036f]/g, '')
      .toLowerCase();
  }

  function getLastDirectory(itemPath: Item[]): DirectoryItem {
    for (let idx = itemPath.length - 1; idx >= 0; idx--) {
      const item = itemPath[idx];
      if (isDirectory(item)) return item;
    }
    throw new Error('No directory found');
  }

  // Get the icon for an item
  function getIcon(item: Item): IconDefinition {
    if (item.path.length <= 1) return faHome;
    return ICON_BY_TYPE[item.properties.type];
  }

  // Get the file name of an item, without its cache timestamp
  function getFileName(item: Item): string {
    if (isDirectory(item)) return item.title;
    const timeStamped = (item as DownloadableItem).properties.resource.split('/').pop() ?? '';
    return timeStamped.split('?')[0];
  }

  return {
    searchCurrentItemPath,
    normalize,
    getLastDirectory,
    getIcon,
    getFileName,
  };
};
