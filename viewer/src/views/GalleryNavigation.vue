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
  <div>
    <ld-error v-if="isError" icon="folder-open" :message="$t('gallery.unknown-resource')" />
    <gallery-search v-else-if="isSearch" :path="path" />
    <component :is="componentName" v-else :key="$galleryStore.currentItem.path" :item="$galleryStore.currentItem" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, Watch } from "vue-property-decorator";
import { ItemType } from "@/@types/ItemType";
import Navigation from "@/services/navigation";
import GallerySearch from "@/views/GallerySearch.vue";

@Component({
  components: {
    GallerySearch,
  },
})
export default class GalleryNavigation extends Vue {
  @Prop(String) readonly path!: string;
  @Prop(Array) readonly query!: string[];

  readonly COMPONENT_BY_TYPE: Record<ItemType, string> = {
    directory: "ld-directory",
    picture: "ld-picture",
    plaintext: "ld-plain-text-viewer",
    pdf: "ld-pdf-viewer",
    video: "ld-video-viewer",
    audio: "ld-audio-viewer",
    other: "ld-download",
  };

  mounted() {
    this.pathChanged();
  }

  get isError() {
    return this.checkType(null);
  }

  get isSearch() {
    return this.checkType(ItemType.DIRECTORY) && this.query.length > 0;
  }

  get componentName() {
    return this.COMPONENT_BY_TYPE[this.$galleryStore.currentItem?.properties.type ?? ItemType.OTHER];
  }

  @Watch("path")
  pathChanged() {
    this.$galleryStore.setCurrentPath(this.path);
  }

  checkType(type: ItemType | null): boolean {
    return Navigation.checkType(this.$galleryStore.currentItem, type);
  }
}
</script>

<style lang="scss"></style>
