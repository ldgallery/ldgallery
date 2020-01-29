/* ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2020  Guillaume FOUET
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

import { createModule, mutation, action } from "vuex-class-component";
import Tools from '@/tools';

const VuexModule = createModule({
    namespaced: "galleryStore",
    strict: true
})

export default class GalleryStore extends VuexModule {

    galleryItemsRoot: Gallery.Item | null = null;
    tags: Tag.Index = {};
    currentPath: string = "/";

    // ---

    @mutation setGalleryItemsRoot(galleryItemsRoot: Gallery.Item) {
        this.galleryItemsRoot = galleryItemsRoot;
    }

    @mutation private setTags(tags: Tag.Index) {
        this.tags = tags;
    }

    @mutation setCurrentPath(currentPath: string) {
        this.currentPath = currentPath;
    }

    get currentItemPath(): Gallery.Item[] {
        const galleryItemsRoot = this.galleryItemsRoot;
        if (galleryItemsRoot)
            return GalleryStore.searchCurrentItemPath(galleryItemsRoot, this.currentPath);
        return [];
    }

    get currentItem(): Gallery.Item | null {
        const currentItemPath = this.currentItemPath;
        return currentItemPath.length > 0 ? currentItemPath[currentItemPath.length - 1] : null;
    }

    // ---

    // Fetches the gallery's JSON metadata
    @action async fetchGalleryItems(url: string) {
        return fetch(url)
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
                if (!index[part]) index[part] = { tag: part, tagfiltered: Tools.normalize(part), items: [], children: {} };
                if (!index[part].items.includes(item)) index[part].items.push(item);
                if (lastPart) index[lastPart].children[part] = index[part];
                lastPart = part;
            }
        }
    }

    // Searches for an item by path from a root item (navigation)
    private static searchCurrentItemPath(item: Gallery.Item, path: string): Gallery.Item[] {
        if (path === item.path) return [item];
        if (item.properties.type === "directory" && path.startsWith(item.path)) {
            const itemChain = item.properties.items
                .map(item => this.searchCurrentItemPath(item, path))
                .find(itemChain => itemChain.length > 0);
            if (itemChain) return [item, ...itemChain];
        }
        return [];
    }
}