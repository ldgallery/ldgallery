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
import { Item, ItemSortStr } from "@/@types/gallery";
import i18n from "@/plugins/i18n";
import { TranslateResult } from "vue-i18n";

export type ItemComparator = (left: Item, right: Item) => number;
export type ItemSort = { text: TranslateResult; fn: ItemComparator };

export default class ItemComparators {
  static readonly ITEM_SORTS: Record<ItemSortStr, ItemSort> = {
    title_asc: {
      text: i18n.t("command.sort.byTitleAsc"),
      fn: ItemComparators.chain(ItemComparators.sortByTitleAsc, ItemComparators.sortByPathAsc),
    },
    date_asc: {
      text: i18n.t("command.sort.byDateAsc"),
      fn: ItemComparators.chain(ItemComparators.sortByDateAsc, ItemComparators.sortByPathAsc),
    },
    date_desc: {
      text: i18n.t("command.sort.byDateDesc"),
      fn: ItemComparators.reverse(ItemComparators.chain(ItemComparators.sortByDateAsc, ItemComparators.sortByPathAsc)),
    },
  };

  static readonly DEFAULT = ItemComparators.ITEM_SORTS.date_asc;

  static sortByPathAsc(left: Item, right: Item): number {
    return left.path.localeCompare(right.path, undefined, {
      sensitivity: "base",
      ignorePunctuation: true,
      numeric: true,
    });
  }

  static sortByTitleAsc(left: Item, right: Item): number {
    return left.title.localeCompare(right.title, undefined, {
      sensitivity: "base",
      ignorePunctuation: true,
      numeric: true,
    });
  }

  static sortByDateAsc(left: Item, right: Item): number {
    return left.datetime.localeCompare(right.datetime); // TODO: handle timezones
  }

  static reverse(fn: ItemComparator): ItemComparator {
    return (l, r) => -fn(l, r);
  }

  static chain(primary: ItemComparator, tieBreaker: ItemComparator): ItemComparator {
    return (l, r) => {
      const primaryComparison = primary(l, r);
      return primaryComparison !== 0 ? primaryComparison : tieBreaker(l, r);
    };
  }
}
