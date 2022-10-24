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
  <div :class="{ [$style.preload]: loading }">
    <!-- Can't use img loading="lazy" yet because @loadstart doesn't work on Chromium -->
    <!-- Also it loads the picture before scroll init (too early) -->
    <VLazyImage
      v-if="thumbnailResourceUrl"
      :src="thumbnailResourceUrl"
      :style="pictureStyle"
      :title="item.title"
      @intersect="loading = true"
      @load="loading = false"
    />
    <div
      v-else
      class="flex-column flex-center"
      :class="$style.thumbnailOther"
    >
      <div>
        <fa-icon
          :icon="icon"
          size="4x"
        />
      </div>
      <div v-text="item.title" />
    </div>
  </div>
</template>

<script setup lang="ts">
import { Item } from '@/@types/gallery';
import { useNavigation } from '@/services/navigation';
import { useItemResource } from '@/services/ui/ldItemResourceUrl';
import VLazyImage from 'v-lazy-image';
import { computed, PropType, ref } from 'vue';

const props = defineProps({
  item: { type: Object as PropType<Item>, required: true },
});

const navigation = useNavigation();

const loading = ref(false);

const { thumbnailResourceUrl } = useItemResource(props.item);

const pictureStyle = computed(() => {
  const resolution = props.item.thumbnail?.resolution ?? { width: 1, height: 1 };
  return { width: `${resolution.width}px`, height: `${resolution.height}px` };
});

const icon = computed(() => navigation.getIcon(props.item));
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.thumbnailOther {
  width: $thumbnail-other-size;
  height: $thumbnail-other-size;
  padding-top: $body-line-height * 1em;
  text-align: center;
  word-break: break-word;
  overflow: hidden;
  > div {
    min-height: $body-line-height * 3em;
  }
}

.preload {
  animation: preloadAnimation 1s infinite ease-in-out alternate;
}
@keyframes preloadAnimation {
  from {
    background-color: $content-bgcolor;
  }
  to {
    background-color: $skeleton-color;
  }
}
</style>
