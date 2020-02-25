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
  <div :class="{'fullscreen': $uiStore.fullscreen, 'fullwidth': $uiStore.fullWidth}">
    <ld-title :current-item-path="$galleryStore.currentItemPath" />
    <panel-top v-if="!isLoading" class="layout layout-top" />
    <panel-left v-if="!isLoading" class="layout layout-left" />
    <router-view v-if="!isLoading" ref="content" class="layout layout-content scrollbar" />
    <b-loading :active="isLoading" is-full-page />
    <ld-key-press :keycode="27" @action="$uiStore.fullscreen=false" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Ref, Watch } from "vue-property-decorator";
import PanelLeft from "./PanelLeft.vue";
import PanelTop from "./PanelTop.vue";
import { Route } from "vue-router";

@Component({
  components: { PanelLeft, PanelTop },
})
export default class MainLayout extends Vue {
  @Ref() readonly content!: Vue;

  isLoading: boolean = true;
  scrollPositions: ScrollPosition = {};

  mounted() {
    history.replaceState({ ldgallery: "ENTRYPOINT" }, "");
    this.fetchGalleryItems();
  }

  @Watch("$route")
  routeChanged(newRoute: Route, oldRoute: Route) {
    const el = this.content.$el;
    this.scrollPositions[oldRoute.path] = el.scrollTop;
    this.$nextTick(() => (el.scrollTop = this.scrollPositions[newRoute.path]));
  }

  fetchGalleryItems() {
    this.isLoading = true;
    this.$galleryStore
      .fetchConfig()
      .then(this.$galleryStore.fetchGalleryItems)
      .finally(() => (this.isLoading = false))
      .catch(this.displayError);
  }

  displayError(reason: any) {
    this.$buefy.snackbar.open({
      message: `Error ${reason}`,
      actionText: "Retry",
      position: "is-top",
      type: "is-danger",
      indefinite: true,
      onAction: this.fetchGalleryItems,
    });
  }
}
</script>

<style lang="scss">
@import "@/assets/scss/theme.scss";

body,
html {
  height: 100%;
  overflow: hidden;
  background-color: $content-bgcolor;
  --layout-top: #{$layout-top};
  --layout-left: #{$layout-left};
}
.layout {
  position: fixed;
  transition: all 0.1s linear;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
  &.layout-top {
    height: $layout-top;
    z-index: 1;
  }
  &.layout-left {
    top: $layout-top;
    width: $layout-left;
    z-index: 2;
  }
  &.layout-content {
    top: var(--layout-top);
    left: var(--layout-left);
    z-index: 3;
    overflow-x: hidden;
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
  &.layout-top {
    background-color: $panel-top-bgcolor;
    color: $panel-top-txtcolor;
  }
  &.layout-left {
    background-color: $panel-left-bgcolor;
    color: $panel-left-txtcolor;
  }
  &.layout-content {
    background-color: $content-bgcolor;
  }
}

// TODO: Remove when #21 (remove explicit navigation/search modes) is resolved
// Forced at the bottom right corner so we can continue working on the sidebar without interference
.tmp-mode-selector {
  position: absolute;
  bottom: 0;
  right: 0;
  z-index: 100;
  opacity: 0.75;
}
// =====
</style>
