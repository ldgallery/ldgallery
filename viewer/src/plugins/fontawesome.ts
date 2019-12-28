import Vue from "vue";

import { library } from "@fortawesome/fontawesome-svg-core";
import { FontAwesomeIcon } from "@fortawesome/vue-fontawesome";
import {
    faExpandArrowsAlt,
    faFolder,
    faSearch,
    faTag,
    faPlus,
    faMinus,
} from "@fortawesome/free-solid-svg-icons";

library.add(
    faExpandArrowsAlt,
    faFolder,
    faSearch,
    faTag,
    faPlus,
    faMinus,
);

Vue.component("fa-icon", FontAwesomeIcon);
