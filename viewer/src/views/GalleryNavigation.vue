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
  <div>
    <LdNotice
      v-if="isError"
      :icon="faFolderOpen"
      :message="t('gallery.unknown-resource')"
    />
    <GallerySearch v-else-if="isSearch" />
    <component
      :is="componentName"
      v-else
      :key="componentKey"
      :item="galleryStore.currentItem"
    />
  </div>
</template>

<script setup lang="ts">
import { ItemType } from '@/@types/itemType';
import LdNotice from '@/components/LdNotice.vue';
import { isDirectory } from '@/services/itemGuards';
import { useGalleryStore } from '@/store/galleryStore';
import { faFolderOpen } from '@fortawesome/free-solid-svg-icons';
import { computedEager } from '@vueuse/shared';
import { computed, watchEffect } from 'vue';
import { useI18n } from 'vue-i18n';
import GallerySearch from './GallerySearch.vue';
import AudioViewer from './item_handlers/AudioViewer.vue';
import DirectoryViewer from './item_handlers/DirectoryViewer.vue';
import DownloadViewer from './item_handlers/DownloadViewer.vue';
import MarkdownViewer from './item_handlers/MarkdownViewer.vue';
import PdfViewer from './item_handlers/PdfViewer.vue';
import PictureViewer from './item_handlers/PictureViewer.vue';
import PlainTextViewer from './item_handlers/PlainTextViewer.vue';
import VideoViewer from './item_handlers/VideoViewer.vue';

const props = defineProps({
  path: { type: String, required: true },
  query: { type: Array<string>, required: true },
});

const { t } = useI18n();
const galleryStore = useGalleryStore();

const COMPONENT_BY_TYPE: Record<ItemType, unknown> = {
  directory: DirectoryViewer,
  picture: PictureViewer,
  plaintext: PlainTextViewer,
  markdown: MarkdownViewer,
  pdf: PdfViewer,
  video: VideoViewer,
  audio: AudioViewer,
  other: DownloadViewer,
};

const isError = computedEager(() => !galleryStore.currentItem?.properties.type);
const isSearch = computedEager(() => isDirectory(galleryStore.currentItem) && props.query.length > 0);
const componentName = computed(() => COMPONENT_BY_TYPE[galleryStore.currentItem?.properties.type ?? ItemType.OTHER]);
const componentKey = computed(() => galleryStore.currentItem?.path ?? '');

watchEffect(() => (galleryStore.currentPath = props.path));
</script>
