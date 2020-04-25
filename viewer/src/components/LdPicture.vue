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
    @click.capture="e => dragScrollClickFix.onClickCapture(e)"
    @click="$uiStore.toggleFullscreen()"
    @dragscrollstart="dragScrollClickFix.onDragScrollStart()"
    @dragscrollend="dragScrollClickFix.onDragScrollEnd()"
  >
    <v-lazy-image
      :src="pictureSrc(picture.properties.resource)"
      :class="{'slow-loading': Boolean(slowLoadingStyle)}"
      :style="slowLoadingStyle"
      @load="clearSlowLoading"
    />
    <b-loading :active="loader" :is-full-page="false" class="ld-picture-loader" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";
import DragScrollClickFix from "@/services/dragscrollclickfix";

@Component
export default class LdPicture extends Vue {
  @Prop({ required: true }) readonly picture!: Gallery.Picture;

  readonly SLOW_LOADING_TIMEOUT_MS: number = 1500;
  readonly dragScrollClickFix = new DragScrollClickFix();

  slowLoadingStyle: string | null = null;
  loader: boolean = false;
  timer: NodeJS.Timeout | null = null;

  mounted() {
    this.timer = setTimeout(this.generateSlowLoadingStyle, this.SLOW_LOADING_TIMEOUT_MS);
  }

  destroyed() {
    this.clearSlowLoading();
  }

  clearSlowLoading() {
    if (this.timer) clearTimeout(this.timer);
    this.timer = null;
    this.slowLoadingStyle = null;
    this.loader = false;
  }

  pictureSrc(resource: string) {
    return `${process.env.VUE_APP_DATA_URL}${this.$galleryStore.config!.galleryRoot}${resource}`;
  }

  generateSlowLoadingStyle() {
    this.clearSlowLoading();
    this.loader = true;
    if (this.picture.thumbnail) {
      const url = this.pictureSrc(this.picture.thumbnail.resource);
      this.slowLoadingStyle = `background-image: url('${url}');`;
    }
  }
}
</script>

<style lang="scss">
@import "~@/assets/scss/theme.scss";

.ld-picture-loader {
  position: relative;
  & .loading-background {
    background: none !important;
  }
}
img.slow-loading {
  background-repeat: no-repeat;
  background-position: center;
  background-size: contain;
  background-color: $content-bgcolor;
  background-blend-mode: soft-light;
  opacity: 1 !important;
}
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
