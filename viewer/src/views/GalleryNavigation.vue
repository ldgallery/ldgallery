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
    <component :is="dispatch().component" v-bind="dispatch().properties" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, Watch } from "vue-property-decorator";
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

  dispatch(): { component: string, properties: {} } {
    switch (this.$galleryStore.currentItem?.properties.type ?? null) {
      case null:
        return {
          component: "ld-error",
          properties: { icon: "folder-open", message: this.$t("gallery.unknown-resource") }
        };

      case "directory":
        return this.query.length > 0
          ? { component: "gallery-search", properties: { path: this.path } }
          : { component: "gallery-directory", properties: { directory: this.$galleryStore.currentItem } };

      case "picture":
        return { component: "ld-picture", properties: { picture: this.$galleryStore.currentItem } };

      default:
        return {
          component: "ld-error",
          properties: { icon: "file", message: this.$t("gallery.unknown-type") }
        };
    }
  }
}
</script>

<style lang="scss">
</style>
