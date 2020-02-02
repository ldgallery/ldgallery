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
  <ul class="ld-breadcrumb">
    <li v-for="(item,idx) in $galleryStore.currentItemPath" :key="item.path">
      <router-link :to="item.path">
        <fa-icon :icon="getIcon(item)" size="lg" />
        {{item.title}}
      </router-link>
      <fa-icon v-if="(idx+1) < $galleryStore.currentItemPath.length" icon="angle-right" />
    </li>
  </ul>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import Tools from "@/tools";

@Component
export default class LdBreadcrumb extends Vue {
  getIcon(item: Gallery.Item) {
    return Tools.getIcon(item);
  }
}
</script>

<style lang="scss">
@import "@/assets/scss/theme.scss";

.ld-breadcrumb {
  border-left: 1px solid $disabled-color;
  padding-left: 15px;
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
</style>
