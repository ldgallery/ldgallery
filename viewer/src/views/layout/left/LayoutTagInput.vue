<!-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2022  Guillaume FOUET
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
  <LdInput
    ref="input"
    v-model="search"
    :placeholder="t('tagInput.placeholder')"
    :tabindex="50"
    @focus="e => (e.target as HTMLInputElement).select()"
    @keypress.enter="inputEnter"
    @keydown.backspace="inputBackspace"
  />
  <LdDropdown
    ref="dropdown"
    v-model="showDropdown"
    :list="filteredTags"
    list-key="tagfiltered"
    :tabindex-root="51"
    :class="$style.dropdown"
    :style="dropdownStyle"
    @select="addTag"
    @opening="emit('opening')"
    @closing="cleanSearch(); emit('closing');"
  >
    <template #option="{option}:{option:TagSearch}">
      <div v-text="option.display" />
      <div v-text="option.items.length" />
    </template>
    <template #empty>
      <div
        :class="$style.nomatch"
        v-text="t('tagInput.nomatch')"
      />
    </template>
  </LdDropdown>
</template>

<script setup lang="ts">
import { TagSearch } from '@/@types/tag';
import LdDropdown from '@/components/LdDropdown.vue';
import LdInput from '@/components/LdInput.vue';
import { useIndexFactory } from '@/services/indexFactory';
import { useGalleryStore } from '@/store/galleryStore';
import { computedEager, useElementBounding, useFocus, useVModel } from '@vueuse/core';
import { computed, ref, StyleValue, watchEffect } from 'vue';
import { useI18n } from 'vue-i18n';

const props = defineProps({
  modelValue: { type: Array<TagSearch>, required: true },
});
const emit = defineEmits(['update:modelValue', 'search', 'opening', 'closing']);
const model = useVModel(props, 'modelValue', emit);

const { t } = useI18n();
const galeryStore = useGalleryStore();
const indexFactory = useIndexFactory();

const search = ref('');
const showDropdown = ref(false);

watchEffect(() => (showDropdown.value = !!search.value));

// ---

const dropdown = ref();
const { top } = useElementBounding(dropdown);
const dropdownStyle = computedEager<StyleValue>(() => ({ height: `calc(100vh - 8px - ${top.value}px)` }));

const input = ref();
const { focused } = useFocus(input);

// ---

const filteredTags = computed(() => indexFactory.searchTags(galeryStore.tagsIndex, search.value, false)
  .filter(filterAlreadyPresent)
  .sort((a, b) => b.items.length - a.items.length));

function filterAlreadyPresent(newSearch: TagSearch) {
  return !model.value.find(
    currentSearch =>
      currentSearch.tag === newSearch.tag && (!currentSearch.parent || currentSearch.parent === newSearch.parent),
  );
}

function addTag(tag?: TagSearch) {
  const toPush = tag ?? filteredTags.value[0];
  if (!toPush) return;
  model.value.push(toPush);
  cleanSearch();
}
function inputEnter() {
  if (search.value) addTag();
  else emit('search');
}
function inputBackspace() {
  !showDropdown.value && model.value.pop();
}
function cleanSearch() {
  search.value = '';
  focused.value = true;
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.dropdown {
  > div {
    display: flex;
    justify-content: space-between;
    > div {
      padding: 0 4px;
    }
    > div:last-child {
      color: $text-light;
    }
  }
  .nomatch {
    color: $disabled-color;
    justify-content: center;
    cursor: default;
  }
}
</style>
