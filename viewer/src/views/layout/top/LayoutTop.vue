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
    v-if="galleryStore.currentItem"
    class="flex"
  >
    <LayoutCommand
      :current-item-path="galleryStore.currentItemPath"
      :item="galleryStore.currentItem"
    />
    <LayoutBreadcrumb
      :current-item-path="galleryStore.currentItemPath"
      :search-mode="uiStore.searchMode"
    />
  </div>
</template>

<script setup lang="ts">
import { useGalleryStore } from '@/store/galleryStore';
import { useUiStore } from '@/store/uiStore';
import LayoutBreadcrumb from './LayoutBreadcrumb.vue';
import LayoutCommand from './LayoutCommand.vue';

const uiStore = useUiStore();
const galleryStore = useGalleryStore();
</script>
