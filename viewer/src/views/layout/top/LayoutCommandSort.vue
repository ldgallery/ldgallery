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
    @click="showDropdown=!showDropdown"
  >
    <fa-icon
      :icon="faSortAmountDown"
      size="lg"
    />
    <teleport to="body">
      <LdDropdown
        v-model="showDropdown"
        :list="itemComparator.ITEM_SORTS"
        :tabindex-root="props.tabindex + 1"
        :class="$style.dropdown"
        @select="(sort: ItemSort) => selectedSort=sort"
      >
        <template #option="{option}:{option:ItemSort}">
          <fa-icon :icon="option.name == selectedSort.name ? faDotCircle : faCircle" />
          <span v-text="option.text" />
        </template>
      </LdDropdown>
    </teleport>
  </LdLink>
</template>

<script setup lang="ts">
import LdDropdown from '@/components/LdDropdown.vue';
import LdLink from '@/components/LdLink.vue';
import { ItemSort, useItemComparator } from '@/services/itemComparator';
import { useUiStore } from '@/store/uiStore';
import { faCircle, faDotCircle, faSortAmountDown } from '@fortawesome/free-solid-svg-icons';
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
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.dropdown {
  top: $layout-top;
  > div {
    padding: 8px 14px;
    > span {
      padding-left: 12px;
    }
  }
}
</style>
