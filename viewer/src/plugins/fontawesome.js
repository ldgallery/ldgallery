import Vue from "vue";

import { library } from "@fortawesome/fontawesome-svg-core";
import { faExpandArrowsAlt } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/vue-fontawesome";

library.add(faExpandArrowsAlt);

Vue.component("fa-icon", FontAwesomeIcon);
