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
    if (item.properties.type === "directory") {
      item.properties.items.forEach(item => this.pushTagsForItem(tagsIndex, item));
      return; // Directories are not indexed
    }
    for (const tag of item.tags) {
      const parts = tag.split(':');
      let lastPart: string | null = null;
      for (const part of parts) {
        tagsIndex[part] = IndexFactory.pushPartToIndex(tagsIndex[part], part, item);
        if (lastPart) {
          const children = tagsIndex[lastPart].children;
          children[part] = IndexFactory.pushPartToIndex(children[part], part, item);
        }
        lastPart = part;
      }
    }
  }

  private static pushPartToIndex(index: Tag.Node, part: string, item: Gallery.Item): Tag.Node {
    if (!index) index = { tag: part, tagfiltered: Navigation.normalize(part), items: [], children: {} };
    if (!index.items.includes(item)) index.items.push(item);
    return index;
  }

  // ---


  public static searchTags(tagsIndex: Tag.Index, filter: string, strict: boolean): Tag.Search[] {
    let search: Tag.Search[] = [];
    if (tagsIndex && filter) {
      const operation = IndexFactory.extractOperation(filter);
      if (operation !== Operation.INTERSECTION) filter = filter.slice(1);
      if (filter.includes(":")) {
        const filterParts = filter.split(":");
        search = this.searchTagsFromFilterWithCategory(tagsIndex, operation, filterParts[0], filterParts[1], strict);
      } else {
        search = this.searchTagsFromFilter(tagsIndex, operation, filter, strict);
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
    disambiguation: string,
    strict: boolean
  ): Tag.Search[] {
    category = Navigation.normalize(category);
    disambiguation = Navigation.normalize(disambiguation);
    return Object.values(tagsIndex)
      .filter(node => IndexFactory.matches(node, category, strict))
      .flatMap(node =>
        Object.values(node.children)
          .filter(child => IndexFactory.matches(child, disambiguation, strict))
          .map(child => ({ ...child, parent: node, operation, display: `${operation}${node.tag}:${child.tag}` }))
      );
  }

  private static searchTagsFromFilter(tagsIndex: Tag.Index, operation: Operation, filter: string, strict: boolean): Tag.Search[] {
    filter = Navigation.normalize(filter);
    return Object.values(tagsIndex)
      .filter(node => IndexFactory.matches(node, filter, strict))
      .map(node => ({ ...node, operation, display: `${operation}${node.tag}` }));
  }

  private static matches(node: Tag.Node, filter: string, strict: boolean): boolean {
    if (strict) return node.tagfiltered === filter;
    return node.tagfiltered.includes(filter)
  }

  // ---

  public static generateCategories(tagsIndex: Tag.Index, tags?: Gallery.RawTag[]): Tag.Category[] {
    if (!tags?.length) return [{ tag: "", index: tagsIndex }];

    const tagsCategories: Tag.Category[] = [];
    const tagsRemaining = new Map(Object.entries(tagsIndex));
    tags
      .map(tag => ({ tag, index: tagsIndex[tag]?.children }))
      .filter(category => category.index && Object.keys(category.index).length)
      .forEach(category => {
        tagsCategories.push(category);
        tagsRemaining.delete(category.tag);
        Object.values(category.index).map(node => node.tag).forEach(tag => tagsRemaining.delete(tag));
      });
    tagsCategories.push({ tag: "", index: Object.fromEntries(tagsRemaining) });
    return tagsCategories;
  }
}
