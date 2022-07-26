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
  <div style="position:relative;">
    <Transition name="fade">
      <div
        v-if="openDropdown"
        ref="dropdown"
        class="scrollbar"
        :class="$style.dropdown"
        :style="dropdownStyle"
      >
        <div
          v-for="(tag,idx) in filteredTags"
          :key="tag.tagfiltered"
          :tabindex="51 + idx"
          @click="addTag(tag)"
          @keypress.enter.space="addTag(tag)"
        >
          <div v-text="tag.display" />
          <div v-text="tag.items.length" />
        </div>
        <div
          v-if="!filteredTags.length"
          class="disaled"
          :class="$style.nomatch"
          v-text="t('tagInput.nomatch')"
        />
      </div>
    </Transition>
  </div>
</template>

<script setup lang="ts">
import { TagSearch } from '@/@types/tag';
import LdInput from '@/components/LdInput.vue';
import { useIndexFactory } from '@/services/indexFactory';
import { useGalleryStore } from '@/store/galleryStore';
import { computedEager, onClickOutside, onKeyStroke, useElementBounding, useFocus, useVModel } from '@vueuse/core';
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
const openDropdown = computedEager<boolean>(() => !!search.value);
watchEffect(() => {
  if (openDropdown.value) emit('opening');
  else emit('closing');
});

// ---

const dropdown = ref();
const { top } = useElementBounding(dropdown);
const dropdownStyle = computedEager<StyleValue>(() => ({ height: `calc(100vh - 8px - ${top.value}px)` }));
onClickOutside(dropdown, closeDropdown);
onKeyStroke('Escape', closeDropdown);

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
  closeDropdown();
}
function inputEnter() {
  if (search.value) addTag();
  else emit('search');
}
function inputBackspace() {
  !openDropdown.value && model.value.pop();
}
function closeDropdown() {
  search.value = '';
  focused.value = true;
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.dropdown {
  position: absolute;
  left: 0;
  z-index: 10;
  width: $layout-left;
  color: $input-color;
  background-color: $dropdown-item-color;
  padding: 4px 0px;
  > div {
    display: flex;
    justify-content: space-between;
    padding: 4px 0;
    margin: 2px; // For the focus border
    cursor: pointer;
    > div {
      padding: 0 4px;
    }
    > div:last-child {
      color: $text-light;
    }
    &:hover {
      background-color: $dropdown-item-hover-color;
    }
    &:focus {
      outline: solid 1px $button-active-color;
    }
    &.nomatch {
      color: $text-light;
      justify-content: center;
    }
  }
}
</style>
