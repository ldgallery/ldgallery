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
  <GalleryTiles
    :items="props.item.properties.items"
    :noresult-message="t('directory.no-results')"
  />
</template>

<script setup lang="ts">
import { DirectoryItem } from '@/@types/gallery';
import { useUiStore } from '@/store/uiStore';
import GalleryTiles from '@/views/GalleryTiles.vue';
import { PropType } from 'vue';
import { useI18n } from 'vue-i18n';

const props = defineProps({
  item: { type: Object as PropType<DirectoryItem>, required: true },
});

const { t } = useI18n();
const uiStore = useUiStore();

uiStore.toggleFullscreen(false);
</script>
