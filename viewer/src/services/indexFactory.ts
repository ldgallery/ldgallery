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

import { Item, RawTag } from '@/@types/gallery';
import { Operation } from '@/@types/operation';
import { TagCategory, TagIndex, TagNode, TagSearch } from '@/@types/tag';
import { isDirectory } from './itemGuards';
import { useNavigation } from './navigation';

const navigation = useNavigation();

function _pushPartToIndex(index: TagNode, part: string, item: Item, rootPart: boolean): TagNode {
  if (!index) {
    index = {
      tag: part,
      tagfiltered: navigation.normalize(part),
      rootPart,
      childPart: !rootPart,
      items: [],
      children: {},
    };
  } else if (rootPart) index.rootPart = true;
  else index.childPart = true;

  if (!index.items.includes(item)) index.items.push(item);
  return index;
}

// Pushes all tags for a root item (and its children) to the index
function _pushTagsForItem(tagArray: RawTag[], tagsIndex: TagIndex, item: Item): void {
  if (isDirectory(item)) {
    item.properties.items.forEach(item => _pushTagsForItem(tagArray, tagsIndex, item));
    return; // Directories are not indexed
  }
  for (const tagId of item.tags) {
    const tag = tagArray[tagId];
    const parts = tag.split(':');
    let lastPart: string | null = null;
    for (const part of parts) {
      tagsIndex[part] = _pushPartToIndex(tagsIndex[part], part, item, !lastPart);
      if (lastPart) {
        const children = tagsIndex[lastPart].children;
        children[part] = _pushPartToIndex(children[part], part, item, false);
      }
      lastPart = part;
    }
    if (lastPart) tagsIndex[lastPart].childPart = true;
  }
}

function _extractOperation(filter: string): Operation {
  const first = filter.slice(0, 1);
  switch (first) {
    case Operation.ADDITION:
    case Operation.SUBSTRACTION:
      return first;
    default:
      return Operation.INTERSECTION;
  }
}

function _searchTagsFromFilterWithCategory(
  tagsIndex: TagIndex,
  operation: Operation,
  category: string,
  disambiguation: string,
  strict: boolean,
): TagSearch[] {
  category = navigation.normalize(category);
  disambiguation = navigation.normalize(disambiguation);
  return Object.values(tagsIndex)
    .filter(node => _matches(node, category, strict))
    .flatMap(node =>
      Object.values(node.children)
        .filter(child => _matches(child, disambiguation, strict))
        .map(child => ({ ...child, parent: node, operation, display: `${operation}${node.tag}:${child.tag}` })),
    );
}

function _searchTagsFromFilter(
  tagsIndex: TagIndex,
  operation: Operation,
  filter: string,
  strict: boolean,
): TagSearch[] {
  filter = navigation.normalize(filter);
  return Object.values(tagsIndex)
    .filter(node => _matches(node, filter, strict))
    .map(node => ({ ...node, operation, display: `${operation}${node.tag}` }));
}

function _matches(node: TagNode, filter: string, strict: boolean): boolean {
  if (strict) return node.tagfiltered === filter;
  return node.tagfiltered.includes(filter);
}

function _isDiscriminantTagOnly(tags: RawTag[], node: TagNode): boolean {
  return !tags.includes(node.tag) || !node.childPart;
}

// ---

export const useIndexFactory = () => {
  function generateTags(root?: Item, tagArray?: RawTag[]): TagIndex {
    const tagsIndex: TagIndex = {};
    if (!tagArray) return tagsIndex;
    if (root) _pushTagsForItem(tagArray, tagsIndex, root);
    return tagsIndex;
  }

  function searchTags(tagsIndex: TagIndex, filter: string, strict: boolean): TagSearch[] {
    let search: TagSearch[] = [];
    if (tagsIndex && filter) {
      const operation = _extractOperation(filter);
      if (operation !== Operation.INTERSECTION) filter = filter.slice(1);
      if (filter.includes(':')) {
        const filterParts = filter.split(':');
        search = _searchTagsFromFilterWithCategory(tagsIndex, operation, filterParts[0], filterParts[1], strict);
      } else {
        search = _searchTagsFromFilter(tagsIndex, operation, filter, strict);
      }
    }
    return search;
  }

  function generateCategories(tagsIndex: TagIndex, categoryTags?: RawTag[]): TagCategory[] {
    if (!categoryTags?.length) return [{ tag: '', index: tagsIndex }];

    const tagsCategories: TagCategory[] = [];
    const tagsRemaining = new Map(Object.entries(tagsIndex));
    categoryTags
      .map(tag => ({ tag, index: tagsIndex[tag]?.children }))
      .filter(category => category.index && Object.keys(category.index).length)
      .forEach(category => {
        tagsCategories.push(category);
        [category.tag, ...Object.values(category.index).map(node => node.tag)]
          .filter(tag => _isDiscriminantTagOnly(categoryTags, tagsIndex[tag]))
          .forEach(tag => tagsRemaining.delete(tag));
      });
    tagsCategories.push({ tag: '', index: Object.fromEntries(tagsRemaining) });
    return tagsCategories;
  }

  return {
    generateTags,
    searchTags,
    generateCategories,
  };
};
