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
import "@/assets/scss/global.scss";
import "@/assets/scss/scrollbar.scss";
import "@/assets/scss/transition.scss";
import store from "@/store";
import i18n from "@/plugins/i18n";
import router from "@/plugins/router";
Vue.config.productionTip = false;

import(/* webpackChunkName: "ui" */ "@/plugins/buefy");
import(/* webpackChunkName: "ui" */ "@/components");
import(/* webpackChunkName: "ui" */ "@/plugins/lazyimage");
import(/* webpackChunkName: "ui" */ "@/plugins/dragscroll");
import(/* webpackChunkName: "ui" */ "@/plugins/fontawesome");
const MainLayout = () => import(/* webpackChunkName: "ui" */ "@/views/MainLayout.vue");

declare module "vue/types/vue" {
  interface Vue {
    $style: any; // SCSS modules
  }
}

new Vue({
  router,
  i18n,
  store,
  render: h => h(MainLayout),
}).$mount("#ldgallery");
