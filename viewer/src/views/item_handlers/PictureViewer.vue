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
  <!-- @dragstart.prevent stops Drag 'n Drop of pictures on FireFox -->
  <div
    ref="containerElement"
    v-dragscroll
    class="scrollbar"
    :class="$style.pictureContainer"
    @dblclick="uiStore.toggleFullscreen()"
    @dragstart.prevent="false"
  >
    <img
      v-if="!error"
      ref="imageElement"
      :src="itemResourceUrl"
      :class="[$style.pictureElement, $style.slowLoading]"
      :style="imageStyle"
      @load="clearSlowLoading"
      @error="onError"
    >
    <LdLoading v-if="loader" />
  </div>
</template>

<script setup lang="ts">
import { PictureItem } from '@/@types/gallery';
import LdLoading from '@/components/LdLoading.vue';
import { useItemResource } from '@/services/ui/ldItemResourceUrl';
import { useLdZoom } from '@/services/ui/ldZoom';
import { useUiStore } from '@/store/uiStore';
import { unrefElement, VueInstance } from '@vueuse/core';
import { createToast } from 'mosha-vue-toastify';
import { CSSProperties, onMounted, onUnmounted, PropType, ref, toRef } from 'vue';
import { useI18n } from 'vue-i18n';

const props = defineProps({
  item: { type: Object as PropType<PictureItem>, required: true },
});

const { t } = useI18n();
const uiStore = useUiStore();

const imageStyle = ref<CSSProperties>({});
const error = ref(false);
const loader = ref(false);

const containerElement = ref<HTMLDivElement>();
const imageElement = ref<VueInstance>();

const { itemResourceUrl, thumbnailResourceUrl } = useItemResource(toRef(props, 'item'));

onMounted(() => {
  generateSlowLoadingStyle();
  if (!containerElement.value) return;
  useLdZoom(
    imageStyle,
    containerElement.value,
    unrefElement(imageElement) as HTMLImageElement,
    props.item.properties,
  );
});

onUnmounted(() => {
  clearSlowLoading();
});

function clearSlowLoading() {
  imageStyle.value.backgroundImage = undefined;
  loader.value = false;
}

function generateSlowLoadingStyle() {
  clearSlowLoading();
  loader.value = true;
  if (thumbnailResourceUrl.value) {
    imageStyle.value.backgroundImage = `url('${thumbnailResourceUrl.value}')`;
  }
}

function onError() {
  clearSlowLoading();
  error.value = true;
  createToast(t('gallery.resource-loading-error'), {
    type: 'danger',
    position: 'top-center',
    timeout: 10000,
    showIcon: true,
  });
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.pictureContainer {
  height: 100%;
}

.pictureElement {
  max-width: unset;
  max-height: unset;
  cursor: grab;
}

.slowLoading {
  background-repeat: no-repeat;
  background-position: center;
  background-size: contain;
  background-color: $content-bgcolor;
  background-blend-mode: soft-light;
  opacity: 1 !important;
}
</style>
