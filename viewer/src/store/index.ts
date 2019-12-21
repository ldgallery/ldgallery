import Vue from 'vue'
import Vuex from 'vuex'
import { extractVuexModule } from "vuex-class-component";
import { createProxy } from "vuex-class-component";
import UIStore from '@/store/uiStore';
import GalleryStore from '@/store/galleryStore';

Vue.use(Vuex)

const store = new Vuex.Store({
  modules: {
    ...extractVuexModule(UIStore),
    ...extractVuexModule(GalleryStore)
  }
});

Vue.use((vue) => vue.prototype.$uiStore = createProxy(store, UIStore));
Vue.use((vue) => vue.prototype.$galleryStore = createProxy(store, GalleryStore));

declare module 'vue/types/vue' {
  interface Vue {
    $uiStore: UIStore,
    $galleryStore: GalleryStore
  }
}

export default store;
