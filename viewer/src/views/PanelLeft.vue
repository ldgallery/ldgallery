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
  <div class="flex-column sidebar">
    <ld-tag-input v-model="$uiStore.searchFilters" :tags-index="$galleryStore.tagsIndex" />
    <ld-command-search @clear="clear" @search="search" />
    <h1 class="title">{{$t('panelLeft.propositions')}}</h1>
    <ld-proposition
      v-model="$uiStore.searchFilters"
      :tags-index="$galleryStore.tagsIndex"
      :current-tags="currentTags()"
      class="scrollbar no-scroll-x"
    />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";
import { Dictionary } from "vue-router/types/router";

@Component
export default class PanelLeft extends Vue {
  clear() {
    this.$uiStore.searchFilters = [];
    this.search();
  }

  search() {
    this.$router.push({ query: this.serializeSearch() }).catch(err => {
      if (err.name !== "NavigationDuplicated") throw err;
    });
  }

  serializeSearch() {
    let query: Dictionary<null> = {};
    this.$uiStore.searchFilters.forEach(filter => (query[filter.display] = null));
    return query;
  }

  currentTags() {
    return this.$galleryStore.currentItem?.tags ?? [];
  }
}
</script>

<style lang="scss">
.sidebar {
  .title {
    margin: 0.2em 0.5em !important;
  }
}
</style>
