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

// @ts-ignore
import Taginput from "buefy/src/components/taginput";
// @ts-ignore
import Loading from "buefy/src/components/loading";
// @ts-ignore
import Button from "buefy/src/components/button";
// @ts-ignore
import SnackBar from "buefy/src/components/snackbar";

import "@/assets/scss/buefy.scss";

Vue.use(Taginput);
Vue.use(Loading);
Vue.use(Button);
Vue.use(SnackBar);

declare module "vue/types/vue" {
  interface Vue {
    $buefy: any;
  }
}
