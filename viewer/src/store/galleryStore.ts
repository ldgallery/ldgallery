import { createModule, mutation, action } from "vuex-class-component";

const VuexModule = createModule({
    namespaced: "galleryStore",
    strict: true
})

export default class GalleryStore extends VuexModule {

    galleryItemsRoot: Gallery.Item | null = null;
    tags: Tag.Index = {};

    // ---

    @mutation setGalleryItemsRoot(galleryItemsRoot: Gallery.Item) {
        this.galleryItemsRoot = galleryItemsRoot;
    }

    @mutation private setTags(tags: Tag.Index) {
        this.tags = tags;
    }

    // ---

    // Fetches the gallery's JSON metadata
    @action async fetchGalleryItems(url: string) {
        fetch(url)
            .then(response => response.json())
            .then(this.setGalleryItemsRoot)
            .then(this.indexTags);
    }

    // Indexes the gallery
    @action async indexTags() {
        let index = {};
        if (this.galleryItemsRoot)
            GalleryStore.pushTagsForItem(index, this.galleryItemsRoot);
        console.log("Index: ", index);
        this.setTags(index);
    }

    // ---

    // Pushes all tags for a root item (and its children) to the index
    private static pushTagsForItem(index: Tag.Index, item: Gallery.Item) {
        console.log("IndexingTagsFor: ", item.path);
        if (item.properties.type === "directory") {
            item.properties.items.forEach(item => this.pushTagsForItem(index, item));
            return; // Directories are not indexed
        }
        for (const tag of item.tags) {
            const parts = tag.split('.');
            let lastPart: string | null = null;
            for (const part of parts) {
                if (!index[part]) index[part] = { tag: part, items: [], children: {} };
                if (!index[part].items.includes(item)) index[part].items.push(item);
                if (lastPart) index[lastPart].children[part] = index[part];
                lastPart = part;
            }
        }
    }

    // Searches for an item by path from a root item (navigation)
    static searchCurrentItem(item: Gallery.Item, path: string): Gallery.Item | null {
        if (path === item.path) return item;
        if (item.properties.type === "directory" && path.startsWith(item.path)) {
            const itemFound = item.properties.items
                .map(item => this.searchCurrentItem(item, path))
                .find(item => Boolean(item));
            return itemFound ?? null;
        }
        return null;
    }
}