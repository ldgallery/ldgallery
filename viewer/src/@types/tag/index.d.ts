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

declare namespace Tag {
    interface Node {
        tag: Gallery.RawTag;
        tagfiltered: Gallery.RawTag;
        items: Gallery.Item[];
        children: Index;
    }
    interface Search extends Node {
        parent?: Node;
        operation: string; // Enum Operation
        display: string;
    }
    type SearchByOperation = { [index: string]: Tag.Search[] };
    type Index = { [index: string]: Node };
}