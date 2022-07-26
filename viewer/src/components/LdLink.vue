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
  <router-link
    v-if="props.to"
    :to="props.to"
    :class="$style.link"
    v-bind="attrs"
    @keypress.space="pushTo"
  >
    <slot />
  </router-link>
  <a
    v-else
    :class="$style.link"
    v-bind="attrs"
    @click="click"
    @keypress.space.enter="click"
  >
    <slot />
  </a>
</template>

<script setup lang="ts">
import { PropType, useAttrs } from 'vue';
import { RouteLocationRaw, useRouter } from 'vue-router';

const props = defineProps({
  to: { type: [String, Object] as PropType<RouteLocationRaw>, default: null },
});
const emit = defineEmits(['click']);
const attrs = useAttrs();
const router = useRouter();

function click() {
  emit('click');
}
function pushTo() {
  router.push(props.to);
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

a, .link {
  color: $link;
  text-decoration: none;
  cursor: pointer;
  &:hover {
    color: $link-hover;
  }
  &:focus {
    color: $link-hover;
    outline: 1px solid  $button-active-color;
  }
}
</style>
