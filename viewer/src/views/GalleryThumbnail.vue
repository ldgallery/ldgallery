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
  <div class="forcedsize" :class="{preload: loading}">
    <v-lazy-image
      v-if="item.thumbnail"
      class="thumbnail"
      :src="pictureSrc"
      :title="item.path"
      @intersect="loading=true"
      @load="loading=false"
    />
    <div v-else class="flex-column flex-center">
      <fa-icon icon="folder" size="4x" />
      {{item.title}}
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
export default class GalleryThumbnail extends Vue {
  @Prop({ required: true }) readonly item!: Gallery.Item;

  loading: boolean = false;

  get pictureSrc() {
    return `${process.env.VUE_APP_DATA_URL}${this.item.thumbnail}`;
  }
}
</script>

<style lang="scss">
@import "@/assets/scss/theme.scss";

.thumbnail {
  max-width: 250px;
  max-height: 250px;
}
.preload {
  animation: preloadAnimation 1s infinite ease-in-out alternate;
}
@keyframes preloadAnimation {
  from {
    background-color: $content-bgcolor;
  }
  to {
    background-color: $loader-color;
  }
}
// Temporary size until we get the true thumbnail size
.forcedsize {
  width: 250px;
  height: 250px;
  text-align: center;
}
</style>
