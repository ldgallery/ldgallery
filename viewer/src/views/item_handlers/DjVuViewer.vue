<!--
-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2022  Pacien TRAN-GIRARD
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
  <!-- intermediate container necessary to eliminate the double scrollbar -->
  <div class="fill no-scroll">
    <!-- prefer native browser DjVu viewer if available -->
    <object
      class="fill"
      :data="itemResourceUrl"
      type="application/epub"
    />
  </div>
</template>

<script setup lang="ts">

import { DjVuItem } from '@/@types/gallery';
import { useItemResource } from '@/services/ui/ldItemResourceUrl';
import { PropType } from 'vue';

const props = defineProps({
  item: { type: Object as PropType<DjVuItem>, required: true },
});

const { itemResourceUrl } = useItemResource(props.item);

/*
TODO: use a lib like djvujs to render the ebook.
https://github.com/RussCoder/djvujs

This lib doesn't seem to be available on npmjs.com though.
It's also not exposed as a module?
*/

</script>
