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
  <ld-error v-if="hasNoResults" icon="search" :message="noresult" />
  <div v-else class="thumbnail-tiles">
    <router-link v-for="item in sortedItems" :key="item.path" :to="item.path">
      <ld-thumbnail :item="item" />
    </router-link>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, Model } from "vue-property-decorator";
import DragScrollClickFix from "@/services/dragscrollclickfix";

@Component
export default class LdPicture extends Vue {
  @Prop({ type: Array, required: true }) readonly items!: Gallery.Item[];
  @Prop(String) readonly noresult?: string;

  get sortedItems() {
    return this.items.sort(this.$uiStore.sort.fn);
  }

  get hasNoResults(): boolean {
    return Boolean(this.noresult) && this.items.length === 0;
  }
}
</script>

<style lang="scss">
.thumbnail-tiles {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  justify-content: space-evenly;

  & > a {
    margin: 2px;
  }
}
</style>
