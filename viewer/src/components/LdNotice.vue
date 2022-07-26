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

<!-- eslint-disable vue/no-v-html -->
<template>
  <div class="flex-column container-vh-centering">
    <fa-icon
      :class="$style.icon"
      :icon="props.icon"
      size="6x"
    />
    <div
      :class="$style.message"
      v-html="props.message"
    />
  </div>
</template>

<script setup lang="ts">
import { IconDefinition } from '@fortawesome/fontawesome-common-types';
import { PropType } from 'vue';

const props = defineProps({
  icon: { type: Object as PropType<IconDefinition>, required: true },
  message: { type: String, required: true },
});
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme.scss";

.icon {
  color: $disabled-color;
}

.message {
  color: $text-light;
  margin-top: 0.5em;
}
</style>
