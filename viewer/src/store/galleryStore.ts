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
import IndexFactory from '@/services/indexfactory';
import Navigation from '@/services/navigation';

const VuexModule = createModule({
    namespaced: "galleryStore",
    strict: true
})

export default class GalleryStore extends VuexModule {

    config: Gallery.Config | null = null;
    galleryItemsRoot: Gallery.Item | null = null;
    tagsIndex: Tag.Index = {};
    currentPath: string = "/";

    // ---

    @mutation setConfig(config: Gallery.Config) {
        this.config = config;
    }

    @mutation setGalleryItemsRoot(galleryItemsRoot: Gallery.Item) {
        this.galleryItemsRoot = galleryItemsRoot;
    }

    @mutation private setTagsIndex(tagsIndex: Tag.Index) {
        this.tagsIndex = tagsIndex;
    }

    @mutation setCurrentPath(currentPath: string) {
        this.currentPath = currentPath;
    }

    get currentItemPath(): Gallery.Item[] {
        const root = this.galleryItemsRoot;
        if (root)
            return Navigation.searchCurrentItemPath(root, this.currentPath);
        return [];
    }

    get currentItem(): Gallery.Item | null {
        const path = this.currentItemPath;
        return path.length > 0 ? path[path.length - 1] : null;
    }

    // ---

    // Fetches the gallery's JSON config
    @action async fetchConfig() {
        return fetch(`${process.env.VUE_APP_DATA_URL}config.json`, { cache: "no-cache" })
            .then(response => response.json())
            .then(this.setConfig);
    }

    // Fetches the gallery's JSON metadata
    @action async fetchGalleryItems() {
        const root = this.config?.galleryRoot ?? '';
        return fetch(`${process.env.VUE_APP_DATA_URL}${root}index.json`, { cache: "no-cache" })
            .then(response => response.json())
            .then(index => index.tree)
            .then(this.setGalleryItemsRoot)
            .then(this.indexTags);
    }

    // Indexes the gallery
    @action async indexTags() {
        this.setTagsIndex(IndexFactory.generateTags(this.galleryItemsRoot));
    }

}
