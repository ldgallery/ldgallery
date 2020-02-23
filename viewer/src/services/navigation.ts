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

export default class Navigation {

  // Searches for an item by path from a root item (navigation)
  public static searchCurrentItemPath(root: Gallery.Item, path: string): Gallery.Item[] {
    if (path === root.path) return [root];
    if (root.properties.type === "directory" && path.startsWith(root.path)) {
      const itemChain = root.properties.items
        .map(item => this.searchCurrentItemPath(item, path))
        .find(itemChain => itemChain.length > 0);
      if (itemChain) return [root, ...itemChain];
    }
    return [];
  }

  // Normalize a string to lowercase, no-accents
  public static normalize(value: string) {
    return value
      .normalize("NFD")
      .replace(/[\u0300-\u036f]/g, "")
      .toLowerCase();
  }

  // Checks if the type of an item matches
  public static checkType(item: Gallery.Item | null, type: Gallery.ItemType): boolean {
    return item?.properties.type === type ?? false;
  }

  public static getLastDirectory(itemPath: Gallery.Item[]): Gallery.Directory {
    for (let idx = itemPath.length - 1; idx >= 0; idx--) {
      const item = itemPath[idx];
      if (Navigation.checkType(item, "directory")) return item as Gallery.Directory;
    }
    throw new Error("No directory found");
  }

  // Sort a list of items, moving the directories to the beginning of the list
  public static directoriesFirst(items: Gallery.Item[]) {
    return [
      ...items
        .filter(child => Navigation.checkType(child, "directory"))
        .sort((a, b) => a.title.localeCompare(b.title)),

      ...items
        .filter(child => !Navigation.checkType(child, "directory")),
    ];
  }

  // Get the icon for an item
  public static getIcon(item: Gallery.Item): string {
    if (item.path.length <= 1) return "home";
    switch (item.properties.type) {
      case "picture":
        return "image";
      case "directory":
        return "folder";
      case "other":
      default:
        return "file";
    }
  }
}
