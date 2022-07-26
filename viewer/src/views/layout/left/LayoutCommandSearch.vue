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
  <div
    class="flex"
    :class="[$style.commands, $style.webkitFlexShrinkFix]"
  >
    <button
      tabindex="60"
      @click="emit('clear')"
    >
      <fa-icon :icon="faEraser" />
      <span>{{ t("command.search.clear") }}</span>
    </button>
    <button
      tabindex="61"
      class="flex-grow-1"
      @click="emit('search')"
    >
      <fa-icon :icon="faSearch" />
      <span>{{ t("command.search.search") }}</span>
    </button>
  </div>
</template>

<script setup lang="ts">
import { faEraser, faSearch } from '@fortawesome/free-solid-svg-icons';
import { useI18n } from 'vue-i18n';

const emit = defineEmits(['clear', 'search']);

const { t } = useI18n();
</script>

<style lang="scss" module>
// fix flexbox shrinking and overlap in old webkit versions
// https://github.com/pacien/ldgallery/issues/183
.webkitFlexShrinkFix {
  flex: none;
}

.commands {
  span {
    margin-left: 0.5rem;
  }
}
</style>
