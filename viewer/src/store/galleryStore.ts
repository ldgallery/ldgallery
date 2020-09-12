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
import IndexFactory from "@/services/indexfactory";
import Navigation from "@/services/navigation";

const VuexModule = createModule({
  namespaced: "galleryStore",
  strict: true,
});

export default class GalleryStore extends VuexModule {
  config: Gallery.Config | null = null;
  galleryIndex: Gallery.Index | null = null;
  tagsIndex: Tag.Index = {};
  tagsCategories: Tag.Category[] = [];
  currentPath: string | null = null;
  currentSearch: Tag.Search[] = [];

  // ---

  @mutation private setConfig(config: Gallery.Config) {
    this.config = config;
  }

  @mutation setGalleryIndex(galleryIndex: Gallery.Index) {
    this.galleryIndex = Object.freeze(galleryIndex);
  }

  @mutation private setTagsIndex(tagsIndex: Tag.Index) {
    this.tagsIndex = Object.freeze(tagsIndex);
  }

  @mutation private setTagsCategories(tagsCategories: Tag.Category[]) {
    this.tagsCategories = tagsCategories;
  }

  @mutation setCurrentPath(currentPath: string) {
    this.currentPath = currentPath;
  }

  @mutation setCurrentSearch(currentSearch: Tag.Search[]) {
    this.currentSearch = currentSearch;
  }

  // ---

  get currentItemPath(): Gallery.Item[] {
    const root = this.galleryIndex?.tree;
    if (root && this.currentPath) return Navigation.searchCurrentItemPath(root, this.currentPath);
    return [];
  }

  get currentItem(): Gallery.Item | null {
    const path = this.currentItemPath;
    return path.length > 0 ? path[path.length - 1] : null;
  }

  get galleryTitle(): string {
    return this.galleryIndex?.properties.galleryTitle ?? "ldgallery";
  }

  get resourceRoot(): string {
    return process.env.VUE_APP_DATA_URL + this.config!.galleryRoot;
  }

  // ---

  // Fetches the gallery's JSON config
  @action async fetchConfig() {
    await fetch(`${process.env.VUE_APP_DATA_URL}${GalleryStore.getUrlConfig()}`, { cache: "no-cache" })
      .then(GalleryStore.responseToJson)
      .then(this.setConfig);
    return this.config!;
  }

  // Fetches the gallery's JSON metadata
  @action async fetchGalleryItems() {
    const root = this.config?.galleryRoot ?? "";
    const index = this.config?.galleryIndex ?? "index.json";
    await fetch(`${process.env.VUE_APP_DATA_URL}${root}${index}`, { cache: "no-cache" })
      .then(GalleryStore.responseToJson)
      .then(this.setGalleryIndex)
      .then(this.indexTags)
      .then(this.indexTagCategories);
    return this.galleryIndex!;
  }

  // Indexes the gallery
  @action async indexTags() {
    const root = this.galleryIndex?.tree ?? null;
    const index = IndexFactory.generateTags(root);
    this.setTagsIndex(index);
    return index;
  }

  // Indexes the proposed categories
  @action async indexTagCategories() {
    const categories = IndexFactory.generateCategories(this.tagsIndex, this.galleryIndex?.properties.tagCategories);
    this.setTagsCategories(categories);
    return categories;
  }

  // Searches for tags
  @action async search(filters: string[]) {
    const results = filters.flatMap(filter => IndexFactory.searchTags(this.tagsIndex, filter, true));
    this.setCurrentSearch(results);
    return results;
  }

  private static getUrlConfig() {
    let search = window.location.search;
    if (search.length > 1) return search.substr(1) + ".json";
    return "config.json";
  }

  private static responseToJson(response: Response) {
    if (!response.ok) throw new Error(`${response.status}: ${response.statusText}`);
    return response.json();
  }
}
