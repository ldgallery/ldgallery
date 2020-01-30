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
  <div class="flex">
    <div class="command-btns">
      <fa-icon icon="filter" size="lg" class="disabled" />
      <router-link to="/" :class="{disabled: $galleryStore.currentItemPath.length <= 1}">
        <fa-icon icon="home" size="lg" />
      </router-link>
      <div class="link" @click="$router.go(-1)">
        <fa-icon icon="arrow-left" size="lg" />
      </div>
    </div>
    <ul class="pathBreadcrumb">
      <li v-for="(item,idx) in $galleryStore.currentItemPath" :key="item.path">
        <router-link :to="item.path">
          <fa-icon :icon="getIcon(item)" size="lg" />
          {{item.title}}
        </router-link>
        <fa-icon v-if="(idx+1) < $galleryStore.currentItemPath.length" icon="angle-right" />
      </li>
    </ul>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, Watch } from "vue-property-decorator";
import Gallery from "./Gallery.vue";

@Component
export default class PanelTop extends Vue {
  getIcon(item: Gallery.Item) {
    if (item.path.length <= 1) return "home";
    switch (item.properties.type) {
      case "picture":
        return "image";
      case "directory":
        return "folder";
    }
  }
}
</script>

<style lang="scss">
@import "@/assets/scss/theme.scss";

.pathBreadcrumb {
  display: flex;
  list-style: none;
  margin: 5px;
  a {
    margin-right: 5px;
  }
  li:not(:first-child) {
    margin-left: 10px;
  }
}
.command-btns {
  display: flex;
  justify-content: space-around;
  vertical-align: middle;
  align-items: center;
  width: $layout-left;
}
</style>
