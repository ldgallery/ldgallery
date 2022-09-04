/* eslint-disable import/extensions */
import '@/assets/scss/global.scss';
import '@/assets/scss/scrollbar.scss';
import '@/assets/scss/transition.scss';
import 'mosha-vue-toastify/dist/style.css';
import { createPinia } from 'pinia';
import { createApp, defineAsyncComponent } from 'vue';
import VueDragscroll from 'vue-dragscroll';
import { importFaIcon } from './plugins/asyncLib';
import i18n from './plugins/i18n';
import router from './plugins/router';
import { useLdFullscreen } from './services/ui/ldFullscreen';
import { useLdKeyboard } from './services/ui/ldKeyboard';
import { useLdTitle } from './services/ui/ldTitle';
import App from './views/MainLayout.vue';

createApp(App)
  .use(createPinia())
  .use(i18n)
  .use(router)
  .use(VueDragscroll)
  .component('fa-icon', defineAsyncComponent(importFaIcon))
  .mount('#app');

useLdTitle();
useLdKeyboard();
useLdFullscreen();
