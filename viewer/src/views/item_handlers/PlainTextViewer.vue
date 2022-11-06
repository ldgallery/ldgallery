<!--
-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2020  Pacien TRAN-GIRARD
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
  <!-- Outer div necessary for the resize handle to appear on Firefox. -->
  <div
    :class="$style.content"
    class="fill"
  >
    <pre
      v-if="isFinished"
      v-text="data"
    />
    <LdLoading v-else />
  </div>
</template>

<script setup lang="ts">
import { PlainTextItem } from '@/@types/gallery';
import LdLoading from '@/components/LdLoading.vue';
import { useItemResource } from '@/services/ui/ldItemResourceUrl';
import { useFetch } from '@vueuse/core';
import { PropType, toRef } from 'vue';

const props = defineProps({
  item: { type: Object as PropType<PlainTextItem>, required: true },
});

const { itemResourceUrl } = useItemResource(toRef(props, 'item'));
const { isFinished, data } = useFetch(itemResourceUrl).text();
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.content {
  display: flex;
  justify-content: center;
  background: $viewer-text-background;
  color: $viewer-text;

  > pre {
    width: 100%;
    max-width: 100%; // Forbid overflow when resizing.
    margin: 1em; // Necessary for the resize handle to be selectable on qutebrowser.
    border: none;
    white-space: pre-wrap;
    resize: horizontal; // Allow the user to adjust the width of the text view for easier column reading.
    overflow: hidden; // Necessary for the resize handle to be shown in Chromium.
  }
}
</style>
