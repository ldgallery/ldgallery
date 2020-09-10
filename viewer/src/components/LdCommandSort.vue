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
  <b-dropdown v-model="selectedSort" :mobile-modal="false" append-to-body @change="onChangeSort">
    <a slot="trigger" class="link">
      <fa-icon icon="sort-amount-down" size="lg" />
    </a>
    <b-dropdown-item v-for="(sort, idx) in SORTS" :key="idx" :value="idx">
      <fa-icon :icon="['far', idx === selectedSort ? 'dot-circle' : 'circle']" />
      <span :class="$style.ml1">{{ sort.name }}</span>
    </b-dropdown-item>
  </b-dropdown>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";
import { RawLocation } from "vue-router";
import ItemSortFn from "@/services/itemSortFn";

@Component
export default class LdCommandSort extends Vue {
  readonly SORTS = [
    { name: this.$t("command.sort.byName"), fn: ItemSortFn.sortByName },
    { name: this.$t("command.sort.byDateDesc"), fn: ItemSortFn.sortByDateDesc },
  ];

  selectedSort = 0;

  onChangeSort(newValue: number) {
    this.$uiStore.setSortFn(this.SORTS[newValue].fn);
  }
}
</script>

<style lang="scss" module>
.ml1 {
  margin-left: 0.5em;
}
</style>
