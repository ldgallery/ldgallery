import { createModule, mutation, action } from "vuex-class-component";

const VuexModule = createModule({
    namespaced: "uiStore",
    strict: false
})

export default class UIStore extends VuexModule {

    fullscreen: boolean = false;
    mode: "navigation" | "search" = "navigation";
    currentTags: Tag.Node[] = [];

    // ---

    get isModeSearch() {
        return this.mode === "search";
    }

    get isModeNavigation() {
        return this.mode === "navigation";
    }

    // ---

    @mutation toggleFullscreen() {
        this.fullscreen = !this.fullscreen;
    }

    @mutation setModeNavigation() {
        this.mode = "navigation";
    }

    @mutation setModeSearch() {
        this.mode = "search";
    }
}
