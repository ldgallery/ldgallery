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
    v-if="hasResults"
    :class="$style.thumbnailTiles"
  >
    <LdLink
      v-for="item in orderedItems"
      :key="item.path"
      :to="item.path"
    >
      <ItemThumbnail :item="item" />
    </LdLink>
  </div>
  <LdNotice
    v-else
    :icon="faSearch"
    :message="noresultMessage"
  />
</template>

<script setup lang="ts">
import { Item } from '@/@types/gallery';
import LdLink from '@/components/LdLink.vue';
import LdNotice from '@/components/LdNotice.vue';
import { useUiStore } from '@/store/uiStore';
import { faSearch } from '@fortawesome/free-solid-svg-icons';
import { computed } from 'vue';
import ItemThumbnail from './ItemThumbnail.vue';

const props = defineProps({
  items: { type: Array<Item>, required: true },
  noresultMessage: { type: String, required: true },
});

const uiStore = useUiStore();
const hasResults = computed(() => props.items.length);
const orderedItems = computed(() => [...props.items].sort(uiStore.sort.fn));
</script>

<style lang="scss" module>
.thumbnailTiles {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  justify-content: space-evenly;

  & > a {
    margin: 2px;
  }
}
</style>
