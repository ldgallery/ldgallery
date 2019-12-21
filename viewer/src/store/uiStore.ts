import { createModule, mutation, action } from "vuex-class-component";

const VuexModule = createModule({
    namespaced: "uiStore",
    strict: true
})

export default class UIStore extends VuexModule {

    fullscreen: boolean = false;

    @mutation toggleFullscreen() {
        this.fullscreen = !this.fullscreen;
    }
}
