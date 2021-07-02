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

import { Item } from "@/@types/gallery";
import { Operation } from "@/@types/Operation";
import { TagSearch, TagSearchByOperation } from "@/@types/tag";

export default class IndexSearch {
  // Results of the search (by tags)
  public static search(searchTags: TagSearch[]): Item[] {
    const byOperation = this.extractTagsByOperation(searchTags);
    const intersection = this.extractIntersection(byOperation);
    const substraction = this.extractSubstraction(byOperation);
    return this.aggregateAll(byOperation, intersection, substraction);
  }

  private static extractTagsByOperation(searchTags: TagSearch[]): TagSearchByOperation {
    const byOperation: TagSearchByOperation = {};
    Object.values(Operation).forEach(
      operation => (byOperation[operation] = searchTags.filter(tag => tag.operation === operation))
    );
    return byOperation;
  }

  private static extractIntersection(byOperation: TagSearchByOperation): Set<Item> {
    const intersection = new Set<Item>();
    if (byOperation[Operation.INTERSECTION].length > 0) {
      byOperation[Operation.INTERSECTION]
        .map(tag => tag.items)
        .reduce((a, b) => a.filter(c => b.includes(c)))
        .flatMap(items => items)
        .forEach(item => intersection.add(item));
    }
    return intersection;
  }

  private static extractSubstraction(byOperation: TagSearchByOperation): Set<Item> {
    const substraction = new Set<Item>();
    if (byOperation[Operation.SUBSTRACTION].length > 0) {
      byOperation[Operation.SUBSTRACTION].flatMap(tag => tag.items).forEach(item => substraction.add(item));
    }
    return substraction;
  }

  private static aggregateAll(
    byOperation: TagSearchByOperation,
    intersection: Set<Item>,
    substraction: Set<Item>
  ): Item[] {
    byOperation[Operation.ADDITION].flatMap(tag => tag.items).forEach(item => intersection.add(item));
    substraction.forEach(item => intersection.delete(item));
    return [...intersection];
  }
}
