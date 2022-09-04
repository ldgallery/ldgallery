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
  <input
    v-model="model"
    :class="$style.ldInput"
  >
</template>

<script setup lang="ts">
import { useVModel } from '@vueuse/core';

const props = defineProps({
  modelValue: { type: String, required: true },
});
const emit = defineEmits(['update:modelValue']);
const model = useVModel(props, 'modelValue', emit);
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.ldInput {
  box-sizing: border-box;
  width: 100%;
  padding: 0 6px;
  font-size: 1.25rem;
  height: 1.7em;
  border: none;
  box-shadow: none;
  color: $text;
  background-color: $input-background-color;
  border: solid 1px $input-background-color;
  &:focus {
    outline: none;
    border-color: $input-focus-box-border-color;
  }
  &::placeholder {
    color: $input-placeholder-color;
    font-weight: 100;
  }
}
</style>
