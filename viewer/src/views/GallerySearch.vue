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
  <ld-gallery :items="items()" :noresult="$t('search.no-results')" />
</template>

<script lang="ts">
import { Component, Vue, Prop, Watch } from "vue-property-decorator";
import { Operation } from "@/@types/Operation";
import IndexSearch from "@/services/indexsearch";
import IndexFactory from "@/services/indexfactory";

@Component
export default class GalleryPicture extends Vue {
  @Prop(String) readonly path!: string;
  @Prop(Array) readonly query!: string[];

  currentSearch: Tag.Search[] = [];

  mounted() {
    this.$uiStore.fullscreen = false;
    this.$uiStore.searchMode = true;
    this.restoreSearchFilters();
  }

  destroyed() {
    this.$uiStore.searchMode = false;
  }

  items() {
    return IndexSearch.search(this.currentSearch, this.path);
  }

  @Watch("query")
  restoreSearchFilters() {
    const tagsIndex = this.$galleryStore.tagsIndex;
    this.currentSearch = this.query.flatMap(filter => IndexFactory.searchTags(tagsIndex, filter));
    this.$uiStore.searchFilters = [...this.currentSearch];
  }
}
</script>

<style lang="scss">
</style>
