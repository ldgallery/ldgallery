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
    <img v-if="item.thumbnail" class="thumbnail" :src="pictureSrc" :title="item.path" />
    <div v-else class="flex-column flex-center">
      <fa-icon icon="folder" class="fa-4x" />
      {{item.path}}
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
export default class GalleryThumbnail extends Vue {
  @Prop({ required: true }) readonly item!: Gallery.Item;

  get pictureSrc() {
    return `${process.env.VUE_APP_DATA_URL}${this.item.thumbnail}`;
  }
}
</script>

<style lang="scss">
.thumbnail {
  max-width: 250px;
  max-height: 250px;
}
</style>
