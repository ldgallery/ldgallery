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

import { Config } from '@/@types/gallery';
import { SplashScreenConfig } from '@/@types/splashscreen';
import { ItemSort, useItemComparator } from '@/services/itemComparator';
import { useLocalStorage } from '@vueuse/core';
import { defineStore } from 'pinia';

const itemComparator = useItemComparator();
const splashScreenAcknowledgment = useLocalStorage('splashScreenAcknowledgment', '');

export const useUiStore = defineStore('ui', {
  state: () => ({
    fullscreen: false,
    fullWidth: window.innerWidth < Number(process.env.VUE_APP_FULLWIDTH_LIMIT),
    searchMode: false,
    sort: itemComparator.DEFAULT as ItemSort,

    splashScreenConfig: null as SplashScreenConfig | null,
    splashScreenEnabled: false,
  }),
  getters: {
  },
  actions: {
    toggleFullscreen(value?: boolean) {
      this.fullscreen = value ?? !this.fullscreen;
    },
    toggleFullWidth(value?: boolean) {
      this.fullWidth = value ?? !this.fullWidth;
    },
    validateSpashScreen() {
      this.splashScreenEnabled = false;
      splashScreenAcknowledgment.value = this.splashScreenConfig?.acknowledgmentKey ?? '';
    },
    async initFromConfig(config: Config) {
      if (config.initialItemSort) {
        const itemSort = itemComparator.ITEM_SORTS.find(sort => sort.name === config.initialItemSort);
        if (itemSort) this.sort = itemSort;
        else throw new Error('Unknown sort type: ' + config.initialItemSort);
      }
      if (config.splashScreen) {
        this.splashScreenConfig = config.splashScreen;
        const uid = config.splashScreen.acknowledgmentKey;
        this.splashScreenEnabled = !uid || splashScreenAcknowledgment.value !== uid;
      }
    },
  },
});
