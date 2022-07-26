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
  <div
    ref="breadcrumb"
    v-dragscroll.x
    class="flex scrollbar"
    :class="$style.ldBreadcrumb"
  >
    <div
      v-show="!arrivedState.left"
      :class="$style.ldBreadcrumbOverflowMask"
    />
    <ul>
      <li
        v-for="(item, idx) in currentItemPath"
        :key="item.path"
      >
        <fa-icon
          v-if="idx > 0"
          :icon="faAngleRight"
          class="disabled"
        />
        <LdLink
          :to="item.path"
        >
          <fa-icon
            :icon="navigation.getIcon(item)"
            size="lg"
          />
          {{ item.title }}
        </LdLink>
      </li>
      <li v-if="searchMode">
        <fa-icon
          :icon="faAngleRight"
          class="disabled"
        />
        <fa-icon
          :icon="faSearch"
          size="lg"
          class="disabled"
        />
      </li>
    </ul>
  </div>
</template>

<script setup lang="ts">
import { Item } from '@/@types/gallery';
import LdLink from '@/components/LdLink.vue';
import { useNavigation } from '@/services/navigation';
import { faAngleRight, faSearch } from '@fortawesome/free-solid-svg-icons';
import { useScroll } from '@vueuse/core';
import { nextTick, onMounted, ref, watch } from 'vue';

const props = defineProps({
  currentItemPath: { type: Array<Item>, required: true },
  searchMode: Boolean,
});

const breadcrumb = ref<HTMLDivElement>();

const navigation = useNavigation();
const { arrivedState } = useScroll(breadcrumb);

onMounted(() =>
  watch(() => props.currentItemPath, () => {
    const div = breadcrumb.value;
    if (div) nextTick(() => (div.scrollLeft = div.scrollWidth));
  }, { immediate: true }));
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.ldBreadcrumbOverflowMask {
  position: absolute;
  width: 100%;
  height: 100%;
  background: linear-gradient(
    to right,
    rgba($panel-top-bgcolor, 1) $breadcrumb-margins,
    rgba($panel-top-bgcolor, 0) $breadcrumb-overflow-mask-size
  );
  pointer-events: none;
}

.ldBreadcrumb {
  ul {
    display: flex;
    white-space: nowrap;
    list-style: none;
    padding: 2px; // Necessary for the focus outline
    align-items: center;
    li {
      > * {
        margin-left: $breadcrumb-margins;
      }
      > a {
        padding: $breadcrumb-margins 2px;
      }
    }
  }
  &:global(.scrollbar) {
    overflow-y: hidden;
    scrollbar-width: none;
    &::-webkit-scrollbar {
      height: 0;
    }
  }
}
</style>
