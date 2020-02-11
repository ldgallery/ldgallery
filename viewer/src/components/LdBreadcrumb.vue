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
    ref="breadcrumb"
    v-dragscroll
    class="ld-breadcrumb flex scrollbar"
    @click.capture="e => dragScrollClickFix.onClickCapture(e)"
    @dragscrollstart="dragScrollClickFix.onDragScrollStart()"
    @dragscrollend="dragScrollClickFix.onDragScrollEnd()"
    @dragscrollmove="checkForOverflowMask"
  >
    <div v-show="overflowMask" class="ld-breadcrumb-overflow-mask"></div>
    <ul class="ld-breadcrumb">
      <li v-for="(item,idx) in $galleryStore.currentItemPath" :key="item.path">
        <fa-icon v-if="idx > 0" icon="angle-right" class="disabled" />
        <router-link :to="item.path" class="link">
          <fa-icon :icon="getIcon(item)" size="lg" />
          {{item.title}}
        </router-link>
      </li>
    </ul>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Ref, Watch } from "vue-property-decorator";
import DragScrollClickFix from "@/dragscrollclickfix";
import Tools from "@/tools";

@Component
export default class LdBreadcrumb extends Vue {
  @Ref() readonly breadcrumb!: HTMLUListElement;

  readonly dragScrollClickFix = new DragScrollClickFix();

  dragging: boolean = false;
  overflowMask: boolean = false;

  mounted() {
    window.addEventListener("resize", this.checkForOverflowMask);
  }

  destroyed() {
    window.removeEventListener("resize", this.checkForOverflowMask);
  }

  checkForOverflowMask() {
    this.overflowMask = this.breadcrumb.scrollLeft > 1;
  }

  @Watch("$galleryStore.currentItemPath")
  changedCurrentItemPath() {
    this.$nextTick(() => {
      this.breadcrumb.scrollLeft = this.breadcrumb.scrollWidth;
      this.checkForOverflowMask();
    });
  }

  getIcon(item: Gallery.Item) {
    return Tools.getIcon(item);
  }
}
</script>

<style lang="scss">
@import "@/assets/scss/theme.scss";

.ld-breadcrumb-overflow-mask {
  position: absolute;
  width: 100%;
  height: 100%;
  background: linear-gradient(
    to right,
    rgba($panel-top-bgcolor, 1) $breadcrumb-margins,
    rgba($panel-top-bgcolor, 0) $breadcrumb-overflow-mask-size
  );
  pointer-events: none;
}

.ld-breadcrumb {
  ul {
    display: flex;
    white-space: nowrap;
  }
  a {
    padding: $breadcrumb-margins 0;
    margin-left: $breadcrumb-margins;
  }
  li {
    align-self: center;
    margin-right: $breadcrumb-margins;
  }
  &.scrollbar {
    overflow-y: hidden;
    scrollbar-width: none;
    &::-webkit-scrollbar {
      height: 0;
    }
  }
}
</style>
