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
  <div
    v-dragscroll
    class="scrollbar"
    :class="{'fit-to-screen': !$uiStore.fullscreen, 'original-size': $uiStore.fullscreen}"
    @click="onClick"
    @dragscrollstart="dragging=true"
  >
    <v-lazy-image :src="pictureSrc" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
export default class LdPicture extends Vue {
  @Prop({ required: true }) readonly picture!: Gallery.Picture;

  dragging: boolean = false;

  get pictureSrc() {
    return `${process.env.VUE_APP_DATA_URL}${this.picture.properties.resource}`;
  }

  onClick() {
    if (!this.dragging) this.$uiStore.toggleFullscreen();
    this.dragging = false;
  }
}
</script>

<style lang="scss">
.fit-to-screen {
  display: flex;
  justify-content: space-around;
  height: 100%;
  & > img {
    object-fit: contain;
    cursor: zoom-in;
  }
}
.original-size {
  display: block;
  text-align: center;
  cursor: grab;
  height: 100%;
  & > img {
    max-width: unset;
    max-height: unset;
    object-fit: none;
    cursor: zoom-out;
  }
}
</style>
