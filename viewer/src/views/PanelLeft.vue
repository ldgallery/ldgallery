<!-- ldgallery - A static generator which turns a collection of tagged
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
-->

<template>
  <div class="flex-column" :class="$style.sidebar">
    <ld-tag-input
      :search-filters.sync="searchFilters"
      :tags-index="$galleryStore.tagsIndex"
      @onkeyenter-empty="search"
    />
    <ld-command-search @clear="clear" @search="search" />
    <h1 class="title">{{ $t("panelLeft.propositions") }}</h1>
    <div class="scrollbar no-scroll-x flex-grow-1" :class="$style.flexShrink1000">
      <ld-proposition
        v-for="category in $galleryStore.tagsCategories"
        :key="category.tag"
        :category="$galleryStore.tagsIndex[category.tag]"
        :show-category="$galleryStore.tagsCategories.length > 1"
        :search-filters.sync="searchFilters"
        :tags-index="category.index"
        :current-tags="currentTags"
      />
    </div>
    <h1 class="flex title" @click="infoOpen = !infoOpen">
      {{ $t("panelLeft.information.title") }}
      <fa-icon :icon="infoOpen ? 'caret-down' : 'caret-up'" />
    </h1>
    <transition name="flex-expand">
      <ld-information v-show="infoOpen" :item="$galleryStore.currentItem" class="scrollbar no-scroll-x" />
    </transition>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, Watch } from "vue-property-decorator";
import { Dictionary, Route } from "vue-router/types/router";
import Navigation from "@/services/navigation";
import IndexFactory from "@/services/indexfactory";

@Component
export default class PanelLeft extends Vue {
  searchFilters: Tag.Search[] = [];
  infoOpen: boolean = true;

  mounted() {
    this.restoreSearchFilters(this.$route);
  }

  clear() {
    this.searchFilters = [];
    this.search();
  }

  search() {
    const lastDirectory = Navigation.getLastDirectory(this.$galleryStore.currentItemPath);
    this.$router.push({ path: lastDirectory.path, query: this.serializeSearch() }).catch(err => {
      if (err.name !== "NavigationDuplicated") throw err;
    });
  }

  serializeSearch() {
    let query: Dictionary<null> = {};
    this.searchFilters.forEach(filter => (query[filter.display] = null));
    return query;
  }

  get currentTags() {
    return this.$galleryStore.currentItem?.tags ?? [];
  }

  @Watch("$route")
  restoreSearchFilters(route: Route) {
    const query = Object.keys(route.query);
    if (query.length > 0) this.$galleryStore.search(query).then(search => (this.searchFilters = [...search]));
  }
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme.scss";

.sidebar {
  :global(.title) {
    background-color: $proposed-category-bgcolor;
    padding: 0.2em 0.5em;
    margin: 0 0 1px 0;
    font-variant: small-caps;
    justify-content: space-between;
    user-select: none;
    > svg {
      color: $link;
      margin-top: 2px; // Fixes a vertical centering issue with the carret
    }
  }
}

.flexShrink1000 {
  flex-shrink: 1000;
}
</style>
