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

import { Config } from "@/@types/gallery";
import { SplashScreenConfig } from "@/@types/splashscreen";
import ItemComparators, { ItemSort } from "@/services/itemComparators";
import { action, createModule, mutation } from "vuex-class-component";

const VuexModule = createModule({
  namespaced: "uiStore",
  strict: true,
});

const STORAGE_SPLASHSCREEN_ACKNOWLEDGMENT = "splashScreenAcknowledgment";

export default class UIStore extends VuexModule {
  fullscreen: boolean = false;
  fullWidth: boolean = window.innerWidth < Number(process.env.VUE_APP_FULLWIDTH_LIMIT);
  searchMode: boolean = false;
  sort: ItemSort = ItemComparators.DEFAULT;

  splashScreenConfig: SplashScreenConfig | null = null;
  splashScreenEnabled: boolean = false;

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

  @mutation setSort(sort: ItemSort) {
    this.sort = sort;
  }

  @mutation setSplashScreenConfig(splashScreenConfig: SplashScreenConfig) {
    this.splashScreenConfig = splashScreenConfig;
  }

  @mutation setSplashScreenEnabled(enabled: boolean) {
    this.splashScreenEnabled = enabled;
  }

  // ---

  @action async initFromConfig(config: Config) {
    if (config.initialItemSort) {
      const itemSort = ItemComparators.ITEM_SORTS[config.initialItemSort];
      if (itemSort) this.setSort(itemSort);
      else throw new Error("Unknown sort type: " + config.initialItemSort);
    }
    if (config.splashScreen) {
      this.setSplashScreenConfig(config.splashScreen);
      const uid = config.splashScreen.acknowledgmentKey;
      this.setSplashScreenEnabled(!uid || localStorage.getItem(STORAGE_SPLASHSCREEN_ACKNOWLEDGMENT) !== uid);
    }
  }

  // ---

  @action async validateSpashScreen() {
    this.setSplashScreenEnabled(false);
    const uid = this.splashScreenConfig?.acknowledgmentKey;
    if (uid) localStorage.setItem(STORAGE_SPLASHSCREEN_ACKNOWLEDGMENT, String(uid));
  }
}
