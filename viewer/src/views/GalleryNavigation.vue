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
  <!-- TODO: eliminate intermediate div -->
  <div>
    <ld-error v-if="checkType(null)" icon="folder-open" :message="$t('gallery.unknown-resource')" />
    <gallery-search v-else-if="checkType('directory') && query.length > 0" :path="path" />
    <gallery-directory v-else-if="checkType('directory')" :directory="$galleryStore.currentItem" />
    <ld-picture v-else-if="checkType('picture')" :picture="$galleryStore.currentItem" />
    <ld-plain-text-viewer v-else-if="checkType('plaintext')" :plain-text-item="$galleryStore.currentItem" />
    <ld-pdf-viewer v-else-if="checkType('pdf')" :pdf-item="$galleryStore.currentItem" />
    <ld-video-viewer v-else-if="checkType('video')" :video-item="$galleryStore.currentItem" />
    <ld-audio-viewer v-else-if="checkType('audio')" :audio-item="$galleryStore.currentItem" />
    <ld-download v-else :item="$galleryStore.currentItem" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, Watch } from "vue-property-decorator";
import Navigation from "@/services/navigation";
import GalleryDirectory from "./GalleryDirectory.vue";
import GallerySearch from "@/views/GallerySearch.vue";

@Component({
  components: {
    GalleryDirectory,
    GallerySearch,
  },
})
export default class GalleryNavigation extends Vue {
  @Prop(String) readonly path!: string;
  @Prop(Array) readonly query!: string[];

  mounted() {
    this.pathChanged();
  }

  @Watch("path")
  pathChanged() {
    this.$galleryStore.setCurrentPath(this.path);
  }

  checkType(type: Gallery.ItemType | null): boolean {
    return Navigation.checkType(this.$galleryStore.currentItem, type);
  }
}
</script>

<style lang="scss">
</style>
