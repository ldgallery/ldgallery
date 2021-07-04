<!--
-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2020  Pacien TRAN-GIRARD
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
  <!-- intermediate container necessary to eliminate the scrollbar -->
  <div class="fill no-scroll">
    <video class="fill" :src="itemResourceUrl" :poster="thumbnailResourceUrl" preload="auto" controls>
      <ld-download-viewer :item="item" />
    </video>
  </div>
</template>

<script lang="ts">
import { VideoItem } from "@/@types/gallery";
import { Component, Prop, Vue } from "vue-property-decorator";

@Component
export default class LdVideoViewer extends Vue {
  @Prop({ required: true }) readonly item!: VideoItem;

  get itemResourceUrl(): string {
    return this.$galleryStore.resourceRoot + this.item.properties.resource;
  }

  get thumbnailResourceUrl(): string {
    return this.item.thumbnail ? this.$galleryStore.resourceRoot + this.item.thumbnail.resource : "";
  }
}
</script>
