<!-- ldgallery - A static generator which turns a collection of tagged
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
-->

<template>
  <GalleryTiles
    :items="items.filteredByPath"
    :noresult-message="noResult"
  />
</template>

<script setup lang="ts">
import { useIndexSearch } from '@/services/indexSearch';
import { useGalleryStore } from '@/store/galleryStore';
import { useUiStore } from '@/store/uiStore';
import { computed, onUnmounted } from 'vue';
import { useI18n } from 'vue-i18n';
import GalleryTiles from './GalleryTiles.vue';

const { t } = useI18n();
const uiStore = useUiStore();
const galleryStore = useGalleryStore();
const indexSearch = useIndexSearch();

uiStore.toggleFullscreen(false);
uiStore.searchMode = true;
onUnmounted(() => {
  uiStore.searchMode = false;
  galleryStore.currentSearch = [];
});

const items = computed(() => {
  const { currentPath, currentSearch } = galleryStore;
  if (!currentPath) return { searchResult: [], filteredByPath: [] };
  const searchResult = indexSearch(currentSearch);
  const filteredByPath = searchResult.filter(item => item.path.startsWith(currentPath));
  return { searchResult, filteredByPath };
});
const otherCount = computed(() => items.value.searchResult.length - items.value.filteredByPath.length);
const noResult = computed(() => t('search.no-result-fmt', [otherCount.value]));
</script>
