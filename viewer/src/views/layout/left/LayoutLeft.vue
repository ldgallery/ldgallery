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
  <div
    class="flex-column"
    :class="$style.sidebar"
  >
    <LayoutTagList v-model="searchFilters" />
    <LayoutTagInput
      v-model="searchFilters"
      @search="search"
      @opening="isOpenSearch=true"
      @closing="isOpenSearch=false"
    />
    <!-- We disable the components beneath the Dropdown for accessibility purposes -->
    <LayoutCommandSearch
      v-show="!isOpenSearch"
      @clear="clear"
      @search="search"
    />
    <h1
      v-show="!isOpenSearch"
      :class="$style.title"
    >
      {{ t("panelLeft.propositions.related") }}
    </h1>
    <div
      v-show="!isOpenSearch"
      class="scrollbar no-scroll-x"
      :class="$style.flexShrinkFully"
    >
      <LayoutProposition
        v-for="category in galleryStore.tagsCategories"
        :key="category.tag"
        v-model:search-filters="searchFilters"
        :category="galleryStore.tagsIndex[category.tag]"
        :show-category="galleryStore.tagsCategories.length > 1"
        :tags-index="category.index"
        :current-tags="currentTags"
      />
    </div>

    <template v-if="galleryStore.currentItem">
      <div class="flex-grow-1" />
      <h1
        :class="[$style.infoPanelTitleBar, $style.title]"
        class="flex"
        @click="infoOpen = !infoOpen"
      >
        {{ t("panelLeft.information.title") }}
        <fa-icon :icon="infoOpen ? faCaretDown : faCaretUp" />
      </h1>
      <transition name="slide">
        <LayoutInformation
          v-show="infoOpen"
          :item="galleryStore.currentItem"
          class="scrollbar no-scroll-x"
        />
      </transition>
    </template>
  </div>
</template>

<script setup lang="ts">
import { TagSearch } from '@/@types/tag';
import { useNavigation } from '@/services/navigation';
import { useGalleryStore } from '@/store/galleryStore';
import { faCaretDown, faCaretUp } from '@fortawesome/free-solid-svg-icons';
import { computed, ref, watch } from 'vue';
import { useI18n } from 'vue-i18n';
import { useRoute, useRouter } from 'vue-router';
import LayoutCommandSearch from './LayoutCommandSearch.vue';
import LayoutInformation from './LayoutInformation.vue';
import LayoutProposition from './LayoutProposition.vue';
import LayoutTagInput from './LayoutTagInput.vue';
import LayoutTagList from './LayoutTagList.vue';

const { t } = useI18n();
const route = useRoute();
const router = useRouter();
const navigation = useNavigation();
const galleryStore = useGalleryStore();

const searchFilters = ref<TagSearch[]>([]);
const infoOpen = ref(true);
const isOpenSearch = ref(false);

function clear() {
  searchFilters.value = [];
  search();
}

function search() {
  const lastDirectory = navigation.getLastDirectory(galleryStore.currentItemPath);
  router.push({ path: lastDirectory.path, query: serializeSearch() }).catch(err => {
    if (err.name !== 'NavigationDuplicated') throw err;
  });
}

function serializeSearch() {
  const query: Record<string, null> = {};
  searchFilters.value.forEach(filter => (query[filter.display] = null));
  return query;
}

const currentTags = computed(() => galleryStore.currentItem?.stringTags ?? []);

watch(() => route.query, (query) => {
  const filters = Object.keys(query);
  if (filters.length > 0) galleryStore.search(filters).then(search => (searchFilters.value = [...search]));
}, { immediate: true });
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.sidebar {
  .title {
    background-color: $proposed-category-bgcolor;
    color: $title-color;
    padding: 0.2em 0.5em;
    margin: 0 0 1px 0;
    font-variant: small-caps;
    justify-content: space-between;
    user-select: none;

    >svg {
      color: $link;
      margin-top: 2px; // Fixes a vertical centering issue with the carret
    }
  }

  .infoPanelTitleBar {
    cursor: pointer;
  }
}

.flexShrinkFully {
  flex-shrink: 1000;
}
</style>
