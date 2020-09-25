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
  <ld-gallery :items="items" :noresult="noResult" />
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";
import { Operation } from "@/@types/Operation";
import IndexSearch from "@/services/indexsearch";

@Component
export default class GalleryPicture extends Vue {
  @Prop(String) readonly path!: string;
  otherCount: number = 0;

  mounted() {
    this.$uiStore.toggleFullscreen(false);
    this.$uiStore.toggleSearchMode(true);
  }

  destroyed() {
    this.$uiStore.toggleSearchMode(false);
    this.$galleryStore.setCurrentSearch([]);
  }

  get items() {
    const searchResult = IndexSearch.search(this.$galleryStore.currentSearch);
    const filteredByPath = searchResult.filter(item => item.path.startsWith(this.path));
    this.otherCount = searchResult.length - filteredByPath.length;
    return filteredByPath;
  }

  get noResult() {
    return this.$tc("search.no-result-fmt", this.otherCount, [this.otherCount]);
  }
}
</script>

<style lang="scss"></style>
