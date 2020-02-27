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

import { Operation } from '@/@types/Operation';
import Navigation from '@/services/navigation';

export default class IndexFactory {

  public static generateTags(root: Gallery.Item | null): Tag.Index {
    let tagsIndex: Tag.Index = {};
    if (root) IndexFactory.pushTagsForItem(tagsIndex, root);
    return tagsIndex;
  }

  // Pushes all tags for a root item (and its children) to the index
  private static pushTagsForItem(tagsIndex: Tag.Index, item: Gallery.Item): void {
    console.log("IndexingTagsFor: ", item.path);
    if (item.properties.type === "directory") {
      item.properties.items.forEach(item => this.pushTagsForItem(tagsIndex, item));
      return; // Directories are not indexed
    }
    for (const tag of item.tags) {
      const parts = tag.split(':');
      let lastPart: string | null = null;
      for (const part of parts) {
        if (!tagsIndex[part]) tagsIndex[part] = { tag: part, tagfiltered: Navigation.normalize(part), items: [], children: {} };
        if (!tagsIndex[part].items.includes(item)) tagsIndex[part].items.push(item);
        if (lastPart) tagsIndex[lastPart].children[part] = tagsIndex[part];
        lastPart = part;
      }
    }
  }

  // ---


  public static searchTags(tagsIndex: Tag.Index, filter: string): Tag.Search[] {
    let search: Tag.Search[] = [];
    if (tagsIndex && filter) {
      const operation = IndexFactory.extractOperation(filter);
      if (operation !== Operation.INTERSECTION) filter = filter.slice(1);
      if (filter.includes(":")) {
        const filterParts = filter.split(":");
        search = this.searchTagsFromFilterWithCategory(tagsIndex, operation, filterParts[0], filterParts[1]);
      } else {
        search = this.searchTagsFromFilter(tagsIndex, operation, filter);
      }
    }
    return search;
  }

  private static extractOperation(filter: string): Operation {
    const first = filter.slice(0, 1);
    switch (first) {
      case Operation.ADDITION:
      case Operation.SUBSTRACTION:
        return first;
      default:
        return Operation.INTERSECTION;
    }
  }

  private static searchTagsFromFilterWithCategory(
    tagsIndex: Tag.Index,
    operation: Operation,
    category: string,
    disambiguation: string
  ): Tag.Search[] {
    disambiguation = Navigation.normalize(disambiguation);
    return Object.values(tagsIndex)
      .filter(node => node.tag.includes(category))
      .flatMap(node =>
        Object.values(node.children)
          .filter(child => child.tagfiltered.includes(disambiguation))
          .map(child => ({ ...child, parent: node, operation, display: `${operation}${node.tag}:${child.tag}` }))
      );
  }

  private static searchTagsFromFilter(tagsIndex: Tag.Index, operation: Operation, filter: string): Tag.Search[] {
    filter = Navigation.normalize(filter);
    return Object.values(tagsIndex)
      .filter(node => node.tagfiltered.includes(filter))
      .map(node => ({ ...node, operation, display: `${operation}${node.tag}` }));
  }
}
