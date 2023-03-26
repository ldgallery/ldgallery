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
  <pre
    v-if="isFinished"
    :class="$style.content"
    v-text="data"
  />
  <LdLoading v-else />
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
  margin: 1em;
  background: $viewer-text-background;
  color: $viewer-text;
  white-space: pre-wrap;
}
</style>
