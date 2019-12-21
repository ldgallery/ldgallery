import Vue from "vue";
import "@/assets/scss/global.scss";
import "@/components"
import "@/plugins/fontawesome";
import "@/plugins/buefy";
import store from '@/store'
import i18n from "@/plugins/i18n";
import router from "@/router";
import LdGallery from "@/views/LdGallery.vue";

Vue.config.productionTip = false;

new Vue({
  router,
  i18n,
  store,
  render: h => h(LdGallery)
}).$mount("#ldgallery");
