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

import { Item, ItemSortStr } from '@/@types/gallery';
import i18n from '@/plugins/i18n';
import { isDirectory } from './itemGuards';

const { t } = i18n.global;

export type ItemComparator = (left: Item, right: Item) => number;
export type ItemSort = { name: ItemSortStr, text: string; fn: ItemComparator };

function _sortByPathAsc(left: Item, right: Item): number {
  return left.path.localeCompare(right.path, undefined, {
    sensitivity: 'base',
    ignorePunctuation: true,
    numeric: true,
  });
}

function _sortByTitleAsc(left: Item, right: Item): number {
  return left.title.localeCompare(right.title, undefined, {
    sensitivity: 'base',
    ignorePunctuation: true,
    numeric: true,
  });
}

function _sortByDateAsc(left: Item, right: Item): number {
  return left.datetime.localeCompare(right.datetime); // TODO: handle timezones
}

function _sortDirectoryFirst(left: Item, right: Item): number {
  const dLeft = isDirectory(left) ? 1 : 0;
  const dRight = isDirectory(right) ? 1 : 0;
  return dRight - dLeft;
}

function _reverse(fn: ItemComparator): ItemComparator {
  return (l, r) => -fn(l, r);
}

function _chain(comparators: ItemComparator[]): ItemComparator {
  return comparators.reduce((primary, tieBreaker) => (l, r) => {
    const primaryComparison = primary(l, r);
    return primaryComparison !== 0 ? primaryComparison : tieBreaker(l, r);
  });
}

// ---

export const useItemComparator = () => {
  const ITEM_SORTS: ItemSort[] = [
    {
      name: 'title_asc',
      text: t('command.sort.byTitleAsc'),
      fn: _chain([_sortDirectoryFirst, _sortByTitleAsc, _sortByPathAsc]),
    },
    {
      name: 'date_asc',
      text: t('command.sort.byDateAsc'),
      fn: _chain([_sortDirectoryFirst, _sortByDateAsc, _sortByPathAsc]),
    },
    {
      name: 'date_desc',
      text: t('command.sort.byDateDesc'),
      fn: _chain([_sortDirectoryFirst, _reverse(_sortByDateAsc), _sortByPathAsc]),
    },
  ];

  const DEFAULT = ITEM_SORTS[2]; // date_desc

  return {
    ITEM_SORTS,
    DEFAULT,
  };
};
