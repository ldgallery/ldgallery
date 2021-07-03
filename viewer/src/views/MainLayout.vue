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
  <div :class="{ [$style.fullscreen]: $uiStore.fullscreen, [$style.fullwidth]: $uiStore.fullWidth }">
    <ld-title :gallery-title="$galleryStore.galleryTitle" :current-item="$galleryStore.currentItem" />
    <PanelTop v-if="isReady" :class="[$style.layout, $style.layoutTop]" />
    <PanelLeft v-if="isReady" :class="[$style.layout, $style.layoutLeft]" />
    <b-loading v-if="isLoading" active />
    <SplashScreen
      v-else-if="$uiStore.splashScreenEnabled"
      :class="$style.layout"
      @validation="$uiStore.validateSpashScreen()"
    />
    <router-view v-else ref="content" :class="[$style.layout, $style.layoutContent]" class="scrollbar" tabindex="01" />
    <ld-key-press :keycode="27" @action="$uiStore.toggleFullscreen(false)" />
  </div>
</template>

<script lang="ts">
import { ScrollPosition } from "@/@types/scrollposition";
import { Component, Ref, Vue, Watch } from "vue-property-decorator";
import { Route } from "vue-router";
import PanelLeft from "./PanelLeft.vue";
import PanelTop from "./PanelTop.vue";
import SplashScreen from "./SplashScreen.vue";

@Component({
  components: {
    PanelLeft,
    PanelTop,
    SplashScreen,
  },
})
export default class MainLayout extends Vue {
  @Ref() readonly content?: Vue;

  isLoading: boolean = true;
  scrollPositions: ScrollPosition = {};

  get contentDiv(): HTMLDivElement | null {
    return (this.content?.$el as HTMLDivElement) ?? null;
  }

  get isReady(): boolean {
    return (
      !this.$uiStore.splashScreenEnabled &&
      !this.isLoading &&
      this.$galleryStore.config !== null &&
      this.$galleryStore.currentPath !== null
    );
  }

  mounted() {
    history.replaceState({ ldgallery: "ENTRYPOINT" }, "");
    this.fetchGalleryItems();
    document.body.addEventListener("fullscreenchange", this.onFullscreenChange);
  }

  destroyed() {
    document.body.removeEventListener("fullscreenchange", this.onFullscreenChange);
  }

  moveFocusToContentDiv() {
    setTimeout(() => this.contentDiv?.focus());
  }

  @Watch("$route")
  routeChanged(newRoute: Route, oldRoute: Route) {
    if (!this.contentDiv) return;
    this.scrollPositions[oldRoute.path] = this.contentDiv.scrollTop;
    this.$nextTick(() => (this.contentDiv!.scrollTop = this.scrollPositions[newRoute.path]));
    this.moveFocusToContentDiv();
  }

  fetchGalleryItems() {
    this.isLoading = true;
    this.$galleryStore
      .fetchConfig()
      .then(this.$uiStore.initFromConfig)
      .then(this.$galleryStore.fetchGalleryItems)
      .then(this.moveFocusToContentDiv)
      .finally(() => (this.isLoading = false))
      .catch(this.displayError);
  }

  displayError(reason: any) {
    this.$buefy.snackbar.open({
      message: `${reason}`,
      actionText: this.$t("snack.retry"),
      position: "is-top",
      type: "is-danger",
      indefinite: true,
      onAction: this.fetchGalleryItems,
    });
  }

  @Watch("$uiStore.fullscreen")
  applyFullscreen(fullscreen: boolean) {
    if (fullscreen && !document.fullscreen) document.body.requestFullscreen();
    else if (document.fullscreen) document.exitFullscreen();
  }

  onFullscreenChange() {
    this.$uiStore.toggleFullscreen(document.fullscreen);
  }
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme.scss";

:global(body),
:global(html) {
  height: 100%;
  overflow: hidden;
  touch-action: none;
  background-color: $content-bgcolor;
  --layout-top: #{$layout-top};
  --layout-left: #{$layout-left};
}
.layout {
  position: fixed;
  transition: all $transition-flex-expand linear;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
  &.layoutTop {
    height: $layout-top;
    z-index: 1;
  }
  &.layoutLeft {
    top: $layout-top;
    width: $layout-left;
    z-index: 2;
  }
  &.layoutContent {
    top: var(--layout-top);
    left: var(--layout-left);
    z-index: 3;
    overflow-x: hidden;
    &:focus {
      outline: none;
    }
  }
}
.fullscreen {
  --layout-top: 0px;
  @extend .fullwidth;
}
.fullwidth {
  --layout-left: 0px;
  .layout {
    &.layout-left {
      transform: translate(-$layout-left, 0);
    }
  }
}

.layout {
  &.layoutTop {
    background-color: $panel-top-bgcolor;
    color: $panel-top-txtcolor;
  }
  &.layoutLeft {
    background-color: $panel-left-bgcolor;
    color: $panel-left-txtcolor;
  }
  &.layoutContent {
    background-color: $content-bgcolor;
  }
}
</style>
