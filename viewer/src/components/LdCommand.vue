<!-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2020  Guillaume FOUET
--               2020       Pacien TRAN-GIRARD
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
    <a class="link" :title="$t('command.search')" @click="$uiStore.toggleFullWidth()">
      <fa-icon :icon="commandToggleSearchPanelIcon()" size="lg" />
    </a>
    <router-link
      to="/"
      class="command-secondary"
      :class="{'disabled': isRoot()}"
      :title="$t('command.home')"
    >
      <fa-icon icon="home" size="lg" />
    </router-link>
    <a class="link command-secondary" :title="$t('command.back')" @click="$router.go(-1)">
      <fa-icon icon="arrow-left" size="lg" />
    </a>
    <router-link :class="{'disabled': isRoot()}" :title="$t('command.parent')" :to="parent()">
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
  commandToggleSearchPanelIcon(): string {
    return this.$uiStore.fullWidth ? "search" : "angle-double-left";
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
@import "@/assets/scss/_buefy_variables.scss";
@import "@/assets/scss/theme.scss";

.command-btns {
  background-color: $command-buttons-bgcolor;
  justify-content: space-around;
  vertical-align: middle;
  align-items: center;
  flex: 0 0 $layout-left;

  > a {
    // normalise icon active boxes
    width: $layout-top;
    line-height: $layout-top;
    text-align: center;
    vertical-align: middle;
  }

  @media only screen and (max-width: $tablet) {
    flex: 0 1;

    > .command-secondary {
      display: none;
    }
  }
}
</style>
