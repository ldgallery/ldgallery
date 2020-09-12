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

import { createModule, mutation, action } from "vuex-class-component";
import ItemComparators, { ItemComparator } from "@/services/itemComparators";

const VuexModule = createModule({
  namespaced: "uiStore",
  strict: true,
});

export default class UIStore extends VuexModule {
  fullscreen: boolean = false;
  fullWidth: boolean = window.innerWidth < Number(process.env.VUE_APP_FULLWIDTH_LIMIT);
  searchMode: boolean = false;
  sortFn: ItemComparator = ItemComparators.sortByNameAsc;

  // ---

  @mutation toggleFullscreen(value?: boolean) {
    this.fullscreen = value ?? !this.fullscreen;
  }

  @mutation toggleFullWidth(value?: boolean) {
    this.fullWidth = value ?? !this.fullWidth;
  }

  @mutation toggleSearchMode(value?: boolean) {
    this.searchMode = value ?? !this.searchMode;
  }

  @mutation setSortFn(sortFn: ItemComparator) {
    this.sortFn = sortFn;
  }

  @action async initFromConfig(config: Gallery.Config) {
    switch (config.initialSort ?? "") {
      case "date_desc":
        this.setSortFn(ItemComparators.sortByDateDesc);
        break;
      case "name_asc":
      case "":
        break;
      default:
        throw new Error("Unknown sort type: " + config.initialSort);
    }
  }
}
