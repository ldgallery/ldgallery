<!-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2022  Guillaume FOUET
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
  <LdLink
    :title="t('command.sort.title')"
    :tabindex="props.tabindex"
    @click="openDropdown"
  >
    <fa-icon
      :icon="faSortAmountDown"
      size="lg"
    />
    <teleport to="body">
      <Transition name="fade">
        <div
          v-if="showDropdown"
          ref="dropdown"
          :class="$style.dropdown"
        >
          <div
            v-for="(sort,idx) in itemComparator.ITEM_SORTS"
            :key="sort.name"
            :tabindex="props.tabindex + idx + 1"
            @click="selectedSort=sort"
            @keypress.enter.space="selectedSort=sort"
          >
            <fa-icon :icon="sort.name == selectedSort.name ? faDotCircle : faCircle" />
            <span v-text="sort.text" />
          </div>
        </div>
      </Transition>
    </teleport>
  </LdLink>
</template>

<script setup lang="ts">
import LdLink from '@/components/LdLink.vue';
import { ItemSort, useItemComparator } from '@/services/itemComparator';
import { useUiStore } from '@/store/uiStore';
import { faCircle, faDotCircle, faSortAmountDown } from '@fortawesome/free-solid-svg-icons';
import { onClickOutside, onKeyStroke } from '@vueuse/core';
import { computed, ref } from 'vue';
import { useI18n } from 'vue-i18n';

const props = defineProps({
  tabindex: { type: Number, required: true },
});

const { t } = useI18n();
const uiStore = useUiStore();
const itemComparator = useItemComparator();

const selectedSort = computed({
  get: () => uiStore.sort,
  set: (newValue: ItemSort) => (uiStore.sort = newValue),
});

const showDropdown = ref(false);

const dropdown = ref();
onClickOutside(dropdown, closeDropdown);
onKeyStroke('Escape', closeDropdown);

function openDropdown() {
  showDropdown.value = true;
}
function closeDropdown() {
  setTimeout(() => (showDropdown.value = false));
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.dropdown {
  position: absolute;
  top: $layout-top;
  left: 0;
  z-index: 10;
  width: $layout-left;
  color: $input-color;
  background-color: $dropdown-item-color;
  padding: 8px 0px;
  cursor: pointer;
  > div {
    padding: 8px 14px;
    margin: 2px; // For the focus border
    > span {
      padding-left: 12px;
    }
    &:hover {
      background-color: $dropdown-item-hover-color;
    }
    &:focus {
      outline: solid 1px $button-active-color;
    }
  }
}
</style>
