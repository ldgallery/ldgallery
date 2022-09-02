/* ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2022  Guillaume FOUET
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

import { Config, Index, Item } from '@/@types/gallery';
import { TagCategory, TagIndex, TagSearch } from '@/@types/tag';
import { useIndexFactory } from '@/services/indexFactory';
import { useNavigation } from '@/services/navigation';
import { defineStore } from 'pinia';

const navigation = useNavigation();
const indexFactory = useIndexFactory();

function getUrlConfig() {
  const search = window.location.search;
  if (search.length > 1) return search.substring(1) + '.json';
  return 'config.json';
}

function responseToJson(response: Response) {
  if (!response.ok) throw new Error(`${response.status}: ${response.statusText}`);
  return response.json();
}

export const useGalleryStore = defineStore('gallery', {
  state: () => ({
    config: null as Config | null,
    galleryIndex: null as Index | null,
    tagsIndex: {} as TagIndex,
    tagsCategories: [] as TagCategory[],
    currentPath: null as string | null,
    currentSearch: [] as TagSearch[],
  }),
  getters: {
    currentItemPath(): Item[] {
      const root = this.galleryIndex?.tree;
      if (root && this.currentPath) return navigation.searchCurrentItemPath(root, this.currentPath);
      return [];
    },
    currentItem(): Item | null {
      const path = this.currentItemPath;
      return path.length > 0 ? path[path.length - 1] : null;
    },
    galleryTitle(): string {
      return this.galleryIndex?.properties.galleryTitle ?? 'ldgallery';
    },
    resourceRoot(): string {
      return process.env.VUE_APP_DATA_URL + (this.config?.galleryRoot ?? '');
    },
  },
  actions: {
    // Fetches the gallery's JSON config
    async fetchConfig() {
      await fetch(`${process.env.VUE_APP_DATA_URL}${getUrlConfig()}`, { cache: 'no-cache' })
        .then(responseToJson)
        .then(v => (this.config = v));
      return this.config as Config;
    },
    // Fetches the gallery's JSON metadata
    async fetchGalleryItems() {
      const root = this.config?.galleryRoot ?? '';
      const index = this.config?.galleryIndex ?? 'index.json';
      await fetch(`${process.env.VUE_APP_DATA_URL}${root}${index}`, { cache: 'no-cache' })
        .then(responseToJson)
        .then(v => (this.galleryIndex = v))
        .then(this.indexTags)
        .then(this.indexTagCategories);
      return this.galleryIndex;
    },
    // Indexes the gallery
    async indexTags() {
      const root = this.galleryIndex?.tree ?? null;
      const index = indexFactory.generateTags(root);
      this.tagsIndex = index;
      return index;
    },
    // Indexes the proposed categories
    async indexTagCategories() {
      const categories = indexFactory.generateCategories(this.tagsIndex, this.galleryIndex?.properties.tagCategories);
      this.tagsCategories = categories;
      return categories;
    },
    // Searches for tags
    async search(filters: string[]) {
      const results = filters.flatMap(filter => indexFactory.searchTags(this.tagsIndex, filter, true));
      this.currentSearch = results;
      return results;
    },
  },
});
