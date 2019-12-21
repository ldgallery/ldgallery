import Vue from 'vue'
import Vuex from 'vuex'
import { extractVuexModule } from "vuex-class-component";
import { createProxy } from "vuex-class-component";
import UIStore from '@/store/uiStore';

Vue.use(Vuex)

const store = new Vuex.Store({
  modules: {
    ...extractVuexModule(UIStore)
  }
});

Vue.use((vue) => vue.prototype.$uiStore = createProxy(store, UIStore));

declare module 'vue/types/vue' {
  interface Vue {
    $uiStore: UIStore
  }
}

export default store;
