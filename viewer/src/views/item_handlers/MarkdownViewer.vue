<!--
-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2021  Guillaume FOUET
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
  <LdLoading v-if="isFetching" />
  <LdMarkdown
    v-else-if="!error && data"
    :class="$style.container"
    :markdown="data"
  />
</template>

<script setup lang="ts">
import { MarkdownItem } from '@/@types/gallery';
import { LdMarkdown } from '@/components/async';
import LdLoading from '@/components/LdLoading.vue';
import { useLdFetch } from '@/services/api/ldFetch';
import { useItemResource } from '@/services/ui/ldItemResourceUrl';
import { PropType, toRef } from 'vue';

const props = defineProps({
  item: { type: Object as PropType<MarkdownItem>, required: true },
});

const { itemResourceUrl } = useItemResource(toRef(props, 'item'));
const { isFetching, data, error } = useLdFetch(itemResourceUrl).text();
</script>

<style lang="scss" module>
.container {
  padding: 8px;
}
</style>
