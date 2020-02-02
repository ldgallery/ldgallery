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

import { library, config } from "@fortawesome/fontawesome-svg-core";
import { FontAwesomeIcon } from "@fortawesome/vue-fontawesome";
import {
    faFolder,
    faSearch,
    faPlus,
    faMinus,
    faImage,
    faHome,
    faArrowLeft,
    faLevelUpAlt,
    faTags,
    faAngleRight,
    faWindowClose,
    faFile,
} from "@fortawesome/free-solid-svg-icons";

library.add(
    faFolder,
    faSearch,
    faPlus,
    faMinus,
    faImage,
    faHome,
    faArrowLeft,
    faLevelUpAlt,
    faTags,
    faAngleRight,
    faWindowClose,
    faFile,
);

config.autoAddCss = false;
import "@fortawesome/fontawesome-svg-core/styles.css";

Vue.component("fa-icon", FontAwesomeIcon);
