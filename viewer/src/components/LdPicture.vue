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
    ref="containerElement"
    v-dragscroll
    class="scrollbar ld-picture-container"
    @click.capture="e => dragScrollClickFix.onClickCapture(e)"
    @dblclick="$uiStore.toggleFullscreen()"
    @dragstart.prevent
    @dragscrollstart="dragScrollClickFix.onDragScrollStart()"
    @dragscrollend="dragScrollClickFix.onDragScrollEnd()"
  >
    <v-lazy-image
      ref="imageElement"
      :src="pictureSrc(picture.properties.resource)"
      class="ld-picture-element"
      :class="{'slow-loading': Boolean(slowLoadingStyle)}"
      :style="slowLoadingStyle"
      @load="lazyImageLoaded"
    />
    <b-loading :active="loader" :is-full-page="false" class="ld-picture-loader" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, Ref } from "vue-property-decorator";
import LdZoom from "@/services/ldzoom";
import DragScrollClickFix from "@/services/dragscrollclickfix";

@Component
export default class LdPicture extends Vue {
  @Prop({ required: true }) readonly picture!: Gallery.Picture;
  @Ref() readonly containerElement!: HTMLDivElement;
  @Ref() readonly imageElement!: any; // FIXME: no typedef for v-lazy-image

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

  lazyImageLoaded() {
    this.clearSlowLoading();
    new LdZoom(this.containerElement, this.imageElement.$el, 10, 1 / 5).install();
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

.ld-picture-container {
  height: 100%;
}

.ld-picture-element {
  max-width: unset;
  max-height: unset;
  cursor: grab;
}

.slow-loading {
  background-repeat: no-repeat;
  background-position: center;
  background-size: contain;
  background-color: $content-bgcolor;
  background-blend-mode: soft-light;
  opacity: 1 !important;
}

.ld-picture-loader {
  position: relative;
  & .loading-background {
    background: none !important;
  }
}
</style>
