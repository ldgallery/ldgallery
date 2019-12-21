import { createModule, mutation, action } from "vuex-class-component";

const VuexModule = createModule({
    namespaced: "galleryStore",
    strict: true
})

export default class GalleryStore extends VuexModule {

    galleryItems: Gallery.Item | null = null;

    @mutation setGalleryItems(galleryItems: Gallery.Item) {
        this.galleryItems = galleryItems;
    }

    @action async fetchGalleryItems(url: string) {
        fetch(url)
            .then(response => response.json())
            .then(this.setGalleryItems);
    }

}