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
    <gallery-search v-if="query.length" :path="path" />
    <gallery-directory v-else-if="checkType('directory')" :directory="$galleryStore.currentItem" />
    <ld-picture v-else-if="checkType('picture')" :picture="$galleryStore.currentItem" />
    <div v-else>{{$t("gallery.unknowntype")}}</div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, Watch } from "vue-property-decorator";
import { Operation } from "@/@types/Operation";
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

  private checkType(type: Gallery.ItemType): boolean {
    return Navigation.checkType(this.$galleryStore.currentItem, type);
  }
}
</script>

<style lang="scss">
</style>
