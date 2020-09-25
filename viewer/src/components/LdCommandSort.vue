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
  <b-dropdown v-model="selectedSort" :mobile-modal="false" append-to-body>
    <a slot="trigger" class="link">
      <fa-icon icon="sort-amount-down" size="lg" />
    </a>
    <b-dropdown-item v-for="(sort, idx) in ITEM_SORTS" :key="idx" :value="sort">
      <fa-icon :icon="['far', sort === selectedSort ? 'dot-circle' : 'circle']" />
      <span :class="$style.dropdownLabel">{{ sort.text }}</span>
    </b-dropdown-item>
  </b-dropdown>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";
import ItemComparators, { ItemSort } from "@/services/itemComparators";

@Component
export default class LdCommandSort extends Vue {
  readonly ITEM_SORTS = ItemComparators.ITEM_SORTS;

  get selectedSort() {
    return this.$uiStore.sort;
  }

  set selectedSort(newValue: ItemSort) {
    this.$uiStore.setSort(newValue);
  }
}
</script>

<style lang="scss" module>
.dropdownLabel {
  margin-left: 0.5em;
}
</style>
