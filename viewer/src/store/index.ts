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

import Vue from "vue";
import Vuex from "vuex";
import { extractVuexModule } from "vuex-class-component";
import { createProxy } from "vuex-class-component";
import UIStore from "@/store/uiStore";
import GalleryStore from "@/store/galleryStore";

Vue.use(Vuex);

const store = new Vuex.Store({
  modules: {
    ...extractVuexModule(UIStore),
    ...extractVuexModule(GalleryStore),
  },
  strict: process.env.NODE_ENV !== "production",
});

Vue.use(vue => (vue.prototype.$uiStore = createProxy(store, UIStore)));
Vue.use(vue => (vue.prototype.$galleryStore = createProxy(store, GalleryStore)));

declare module "vue/types/vue" {
  interface Vue {
    $uiStore: UIStore;
    $galleryStore: GalleryStore;
  }
}

export default store;
