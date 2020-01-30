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

export default class Tools {

  // Normalize a string to lowercase, no-accents
  public static normalize(value: string) {
    return value
      .normalize("NFD")
      .replace(/[\u0300-\u036f]/g, "")
      .toLowerCase();
  }


  public static checkType(item: Gallery.Item | null, type: Gallery.ItemType): boolean {
    return item?.properties.type === type ?? false;
  }

  public static directoriesFirst(items: Gallery.Item[]) {
    return [
      ...items
        .filter(child => Tools.checkType(child, "directory"))
        .sort((a, b) => a.title.localeCompare(b.title)),

      ...items
        .filter(child => !Tools.checkType(child, "directory")),
    ];
  }


}