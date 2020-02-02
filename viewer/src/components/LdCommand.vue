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
  <div class="flex command-btns">
    <div class="link" :title="$t('title.tags')" @click="$uiStore.toggleFullWidth()">
      <fa-icon :icon="commandTagsIcon()" size="lg" />
    </div>
    <router-link to="/" :class="{'disabled': isRoot()}" :title="$t('title.home')">
      <fa-icon icon="home" size="lg" />
    </router-link>
    <div class="link" :title="$t('title.back')" @click="$router.go(-1)">
      <fa-icon icon="arrow-left" size="lg" />
    </div>
    <router-link :class="{'disabled': isRoot()}" :title="$t('title.parent')" :to="parent()">
      <fa-icon icon="folder" size="xs" />
      <fa-icon icon="level-up-alt" size="lg" />
    </router-link>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { RawLocation } from "vue-router";

@Component
export default class LdCommand extends Vue {
  commandTagsIcon(): string {
    return this.$uiStore.fullWidth ? "tags" : "window-close";
  }

  isRoot(): boolean {
    return this.$galleryStore.currentItemPath.length <= 1;
  }

  parent(): RawLocation {
    if (!this.isRoot()) return this.$galleryStore.currentItemPath[this.$galleryStore.currentItemPath.length - 2];
    return "";
  }
}
</script>

<style lang="scss">
@import "@/assets/scss/theme.scss";

.command-btns {
  justify-content: space-around;
  vertical-align: middle;
  align-items: center;
  width: $layout-left;
  > * {
    // Unify the minor size differences between icons
    width: 26px;
    height: 26px;
    margin-top: 2px;
  }
}
</style>
