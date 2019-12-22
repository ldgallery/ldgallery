import { createModule, mutation, action } from "vuex-class-component";

const VuexModule = createModule({
    namespaced: "uiStore",
    strict: false
})

export default class UIStore extends VuexModule {

    fullscreen: boolean = false;
    currentTags: Tag.Node[] = [];

    // ---

    @mutation toggleFullscreen() {
        this.fullscreen = !this.fullscreen;
    }
}
